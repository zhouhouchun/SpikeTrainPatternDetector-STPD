import Foundation

/// A read-only source table accepted by the scientific-review presentation layer.
///
/// The source rows remain authoritative. This type only gives the presentation layer a stable,
/// package-reader-independent input so the same review tables can be built for a live run and for a
/// verified `.stpdresult` package.
public struct STPDResultSemanticSourceTable: Sendable, Hashable {
    public let fileName: String
    public let columns: [STPDResultColumnDefinition]
    public let rows: [[String]]

    public init(
        fileName: String,
        columns: [STPDResultColumnDefinition],
        rows: [[String]]
    ) {
        self.fileName = fileName
        self.columns = columns
        self.rows = rows
    }
}

public struct STPDResultSemanticConsistencyCheck: Sendable, Hashable {
    public let id: String
    public let status: String
    public let severity: String
    public let details: String

    public init(
        id: String,
        status: String,
        severity: String,
        details: String
    ) {
        self.id = id
        self.status = status
        self.severity = severity
        self.details = details
    }
}

public enum STPDResultSemanticTableGroup: String, Sendable, Hashable {
    case review
    case scientific
    case isi
    case diagnostic
}

public enum STPDResultSemanticColumnKind: String, Sendable, Hashable {
    case identifier
    case text
    case integer
    case real
    case status
    case longText
}

public struct STPDResultSemanticColumn: Sendable, Hashable, Identifiable {
    public let id: String
    public let title: String
    public let unit: String?
    public let kind: STPDResultSemanticColumnKind
    public let help: String

    public init(
        id: String,
        title: String,
        unit: String? = nil,
        kind: STPDResultSemanticColumnKind = .text,
        help: String = ""
    ) {
        self.id = id
        self.title = title
        self.unit = unit
        self.kind = kind
        self.help = help
    }

    public var displayTitle: String {
        guard let unit, !unit.isEmpty else {
            return title
        }
        return "\(title) (\(unit))"
    }
}

public struct STPDResultSemanticRow: Sendable, Hashable, Identifiable {
    public let id: String
    public let values: [String]

    public init(id: String, values: [String]) {
        self.id = id
        self.values = values
    }
}

public struct STPDResultSemanticTable: Sendable, Hashable, Identifiable {
    public let id: String
    public let title: String
    public let subtitle: String
    public let group: STPDResultSemanticTableGroup
    public let sourceFileNames: [String]
    public let columns: [STPDResultSemanticColumn]
    public let rows: [STPDResultSemanticRow]

    public init(
        id: String,
        title: String,
        subtitle: String,
        group: STPDResultSemanticTableGroup,
        sourceFileNames: [String],
        columns: [STPDResultSemanticColumn],
        rows: [STPDResultSemanticRow]
    ) {
        self.id = id
        self.title = title
        self.subtitle = subtitle
        self.group = group
        self.sourceFileNames = sourceFileNames
        self.columns = columns
        self.rows = rows
    }
}

public struct STPDResultSemanticPatternCount: Sendable, Hashable, Identifiable {
    public let pattern: String
    public let count: Int

    public var id: String { pattern }

    public init(pattern: String, count: Int) {
        self.pattern = pattern
        self.count = count
    }
}

public struct STPDResultSemanticOverview: Sendable, Hashable {
    public let datasetName: String
    public let trainCount: Int?
    public let spikeCount: Int?
    public let allEventCount: Int
    public let authoritativeEventCount: Int
    public let highConfidenceEventCount: Int
    public let reviewCandidateCount: Int
    public let burstFamilyEventCount: Int
    public let isiLabelCount: Int
    public let structureCandidateCount: Int
    public let diagnosticCandidateCount: Int
    public let consistencyPassCount: Int
    public let consistencyAttentionCount: Int
    public let patternCounts: [STPDResultSemanticPatternCount]
}

public enum STPDResultSemanticPresentationError: Error, LocalizedError, Hashable {
    case duplicateTable(String)
    case duplicateColumn(table: String, column: String)
    case invalidRowWidth(table: String, row: Int, expected: Int, actual: Int)
    case missingRequiredTable(String)
    case missingRequiredColumn(table: String, column: String)
    case incompleteTableSet([String])
    case duplicateKey(table: String, column: String, value: String)
    case missingJoinedRow(table: String, column: String, value: String)
    case orphanJoinedRow(table: String, column: String, value: String)
    case invalidInteger(table: String, column: String, value: String)
    case invalidStringList(table: String, column: String)

    public var errorDescription: String? {
        switch self {
        case .duplicateTable(let table):
            return "Scientific review input contains duplicate table \(table)."
        case let .duplicateColumn(table, column):
            return "Table \(table) contains duplicate column \(column)."
        case let .invalidRowWidth(table, row, expected, actual):
            return "Table \(table) row \(row + 1) has \(actual) values; expected \(expected)."
        case .missingRequiredTable(let table):
            return "Scientific review requires \(table), but it is missing."
        case let .missingRequiredColumn(table, column):
            return "Table \(table) is missing required column \(column)."
        case .incompleteTableSet(let tables):
            return "Scientific review requires the complete table set: \(tables.joined(separator: ", "))."
        case let .duplicateKey(table, column, value):
            return "Table \(table) contains duplicate \(column) value \(value)."
        case let .missingJoinedRow(table, column, value):
            return "Table \(table) is missing the joined \(column) value \(value)."
        case let .orphanJoinedRow(table, column, value):
            return "Table \(table) contains orphan \(column) value \(value)."
        case let .invalidInteger(table, column, value):
            return "Table \(table) contains invalid integer \(value) in \(column)."
        case let .invalidStringList(table, column):
            return "Table \(table) contains an invalid canonical string list in \(column)."
        }
    }
}

/// Deterministic, R-inspired scientific review tables derived from authoritative result-package rows.
///
/// This is deliberately a presentation projection, not a new result schema. It never changes source
/// rows, detector labels, review authority, package validation, or exported bytes. Metrics are joined
/// only when an event has exactly one unambiguous source candidate. Multi-source and manual-only
/// events remain visible, with their unavailable candidate metrics left blank.
public struct STPDResultSemanticPresentation: Sendable, Hashable {
    public static let authoritativeEventsTableID = "review.authoritative"
    public static let highConfidenceTableID = "review.high_confidence"
    public static let reviewCandidatesTableID = "review.candidates"
    public static let allEventsTableID = "review.all_events"
    public static let burstFamilyTableID = "review.burst_family"
    public static let isiLabelsTableID = "isi.labels"
    public static let structureCandidatesTableID = "diagnostic.structure_candidates"
    public static let diagnosticCandidatesTableID = "diagnostic.candidates"

    public let overview: STPDResultSemanticOverview
    public let reviewTables: [STPDResultSemanticTable]
    public let scientificTables: [STPDResultSemanticTable]
    public let isiTables: [STPDResultSemanticTable]
    public let diagnosticTables: [STPDResultSemanticTable]
    public let sourceTables: [STPDResultSemanticSourceTable]
    public let consistencyChecks: [STPDResultSemanticConsistencyCheck]
    public let scientificNotes: [String]

    public var allSemanticTables: [STPDResultSemanticTable] {
        reviewTables + scientificTables + isiTables + diagnosticTables
    }

    public func table(id: String) -> STPDResultSemanticTable? {
        allSemanticTables.first { $0.id == id }
    }

    public static func make(
        sourceTables: [STPDResultSemanticSourceTable],
        consistencyChecks: [STPDResultSemanticConsistencyCheck] = []
    ) throws -> STPDResultSemanticPresentation {
        let indexedTables = try indexed(sourceTables)
        guard let eventsSource = indexedTables[STPDResultTable.eventsFinal.rawValue] else {
            throw STPDResultSemanticPresentationError.missingRequiredTable(
                STPDResultTable.eventsFinal.rawValue
            )
        }

        let featureName = STPDResultTable.candidateFeatures.rawValue
        let decisionName = STPDResultTable.finalDecisions.rawValue
        try requireAllOrNone(
            indexedTables,
            names: [featureName, decisionName]
        )
        let featureTable = indexedTables[featureName]
        let decisionTable = indexedTables[decisionName]
        try requireEventJoinColumns(
            eventTable: eventsSource,
            featureTable: featureTable,
            decisionTable: decisionTable
        )
        let featureRows = try keyedRows(featureTable, key: "candidate_uid")
        let decisionRows = try keyedRows(decisionTable, key: "candidate_uid")

        let eventRecords = try eventRecords(
            eventTable: eventsSource,
            featureTable: featureTable,
            featureRows: featureRows,
            decisionTable: decisionTable,
            decisionRows: decisionRows
        )
        let authoritative = eventRecords.filter(\.isAuthoritative)
        let highConfidence = eventRecords.filter(\.isHighConfidence)
        let reviewCandidates = eventRecords.filter { !$0.isAuthoritative }
        let burstFamily = eventRecords.filter { burstFamilyLabels.contains($0.normalizedPattern) }

        let eventSources = [
            STPDResultTable.eventsFinal.rawValue,
            featureTable?.source.fileName,
            decisionTable?.source.fileName,
        ].compactMap { $0 }
        let reviewTables = [
            eventTable(
                id: authoritativeEventsTableID,
                title: "Accepted / authoritative final events",
                subtitle: "Accepted automatic events and manual-authority events. This is an authority view, not a detector-confidence claim.",
                records: authoritative,
                sources: eventSources
            ),
            eventTable(
                id: highConfidenceTableID,
                title: "Automatic high-confidence events",
                subtitle: "Accepted automatic events whose single source candidate explicitly records a high audit-confidence tier.",
                records: highConfidence,
                sources: eventSources
            ),
            eventTable(
                id: reviewCandidatesTableID,
                title: "Review candidates",
                subtitle: "Possible bursts, non-accepted events, and records whose audit authority requires inspection.",
                records: reviewCandidates,
                sources: eventSources
            ),
            eventTable(
                id: allEventsTableID,
                title: "All final events",
                subtitle: "Complete event ledger with review status and available candidate-level structure metrics.",
                records: eventRecords,
                sources: eventSources
            ),
            eventTable(
                id: burstFamilyTableID,
                title: "Burst family",
                subtitle: "Burst, high-frequency burst, long-burst, Burst II, and possible-burst records shown without merging their labels.",
                records: burstFamily,
                sources: eventSources
            ),
        ]

        let scientificTables = patternSpecifications.map { specification in
            eventTable(
                id: "science.\(specification.id)",
                title: specification.title,
                subtitle: specification.subtitle,
                records: eventRecords.filter {
                    specification.labels.contains($0.normalizedPattern)
                },
                sources: eventSources,
                group: .scientific
            )
        }

        var notes = [
            "MM is unavailable because the legacy max-to-mean metric was intentionally removed from the Mac detector; no value is fabricated.",
            "Pre-LV and After-LV are not present in the current authoritative result schema. CV, CV2, LV, gap, and contrast fields are shown where an event has one unambiguous source candidate.",
        ]
        let ambiguousMetricCount = eventRecords.filter {
            $0.metricAvailability == "multiple source candidates"
        }.count
        let missingMetricCount = eventRecords.filter {
            $0.metricAvailability == "candidate metrics unavailable"
        }.count
        if ambiguousMetricCount > 0 {
            notes.append(
                "\(ambiguousMetricCount) event(s) have multiple source candidates; candidate metrics are intentionally not aggregated."
            )
        }
        if missingMetricCount > 0 {
            notes.append(
                "\(missingMetricCount) event(s) reference unavailable candidate metrics; their event geometry remains visible."
            )
        }

        var diagnosticTables: [STPDResultSemanticTable] = []
        if let structure = try candidatePresentation(
            indexedTables: indexedTables,
            ledgerName: STPDResultTable.candidateLedger.rawValue,
            featureName: STPDResultTable.candidateFeatures.rawValue,
            decisionName: STPDResultTable.finalDecisions.rawValue,
            id: structureCandidatesTableID,
            title: "Structural candidate review",
            subtitle: "Candidate structure, decision, regularity, and the available gap/contrast boundary evidence."
        ) {
            diagnosticTables.append(structure)
        }
        if let diagnostic = try candidatePresentation(
            indexedTables: indexedTables,
            ledgerName: STPDResultTable.candidateLedgerDiagnostic.rawValue,
            featureName: STPDResultTable.candidateFeaturesDiagnostic.rawValue,
            decisionName: STPDResultTable.finalDecisionsDiagnostic.rawValue,
            id: diagnosticCandidatesTableID,
            title: "Diagnostic candidates",
            subtitle: "Diagnostic-only candidate rows with current structure metrics, decisions, uncertainty, and failure reasons. These rows are not classified as near misses."
        ) {
            diagnosticTables.append(diagnostic)
        }

        var isiTables: [STPDResultSemanticTable] = []
        if let source = indexedTables[STPDResultTable.isiLabelsFinal.rawValue] {
            isiTables.append(try isiPresentation(source))
        } else {
            notes.append("ISI_labels_final.csv is missing; per-ISI scientific review is unavailable.")
        }

        let metadata = indexedTables[STPDResultTable.runMetadata.rawValue]
        let metadataRow = metadata?.rows.first
        let datasetName = metadata.flatMap { metadataTable in
            metadataRow.map { row in
                metadataTable.value(row, "dataset_name")
            }
        } ?? ""
        let trainCount = try optionalInteger(
            table: metadata,
            row: metadataRow,
            column: "train_count"
        )
        let spikeCount = try optionalInteger(
            table: metadata,
            row: metadataRow,
            column: "spike_count"
        )
        let isiCount = isiTables.first?.rows.count ?? 0
        let structureCount = diagnosticTables.first {
            $0.id == structureCandidatesTableID
        }?.rows.count ?? 0
        let diagnosticCount = diagnosticTables.first {
            $0.id == diagnosticCandidatesTableID
        }?.rows.count ?? 0
        let passCount = consistencyChecks.filter {
            normalize($0.status) == "pass"
        }.count
        let patternCounts = Dictionary(grouping: eventRecords, by: \.normalizedPattern)
            .map { STPDResultSemanticPatternCount(pattern: $0.key, count: $0.value.count) }
            .sorted {
                if $0.count != $1.count { return $0.count > $1.count }
                return $0.pattern < $1.pattern
            }

        return STPDResultSemanticPresentation(
            overview: STPDResultSemanticOverview(
                datasetName: datasetName,
                trainCount: trainCount,
                spikeCount: spikeCount,
                allEventCount: eventRecords.count,
                authoritativeEventCount: authoritative.count,
                highConfidenceEventCount: highConfidence.count,
                reviewCandidateCount: reviewCandidates.count,
                burstFamilyEventCount: burstFamily.count,
                isiLabelCount: isiCount,
                structureCandidateCount: structureCount,
                diagnosticCandidateCount: diagnosticCount,
                consistencyPassCount: passCount,
                consistencyAttentionCount: consistencyChecks.count - passCount,
                patternCounts: patternCounts
            ),
            reviewTables: reviewTables,
            scientificTables: scientificTables,
            isiTables: isiTables,
            diagnosticTables: diagnosticTables,
            sourceTables: sourceTables.sorted { $0.fileName < $1.fileName },
            consistencyChecks: consistencyChecks,
            scientificNotes: notes
        )
    }
}

// MARK: - Projection implementation

private extension STPDResultSemanticPresentation {
    struct IndexedTable {
        let source: STPDResultSemanticSourceTable
        let columnIndex: [String: Int]

        var rows: [[String]] { source.rows }

        func require(_ column: String) throws -> Int {
            guard let index = columnIndex[column] else {
                throw STPDResultSemanticPresentationError.missingRequiredColumn(
                    table: source.fileName,
                    column: column
                )
            }
            return index
        }

        func value(_ row: [String], _ column: String) -> String {
            guard let index = columnIndex[column],
                  row.indices.contains(index) else {
                return ""
            }
            return row[index]
        }
    }

    struct EventRecord {
        let id: String
        let normalizedPattern: String
        let isAuthoritative: Bool
        let isHighConfidence: Bool
        let metricAvailability: String
        let sortTrain: String
        let sortStart: Int
        let row: STPDResultSemanticRow
    }

    struct PatternSpecification {
        let id: String
        let title: String
        let subtitle: String
        let labels: Set<String>
    }

    static let eventColumns = [
        STPDResultSemanticColumn(id: "train", title: "Train", kind: .identifier),
        STPDResultSemanticColumn(id: "pattern", title: "Pattern", kind: .status),
        STPDResultSemanticColumn(id: "subtype", title: "Subtype", kind: .text),
        STPDResultSemanticColumn(id: "review", title: "Review", kind: .status),
        STPDResultSemanticColumn(id: "aligned_start", title: "Aligned start", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "aligned_end", title: "Aligned end", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "raw_start", title: "Raw start", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "raw_end", title: "Raw end", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "duration", title: "Duration", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "n_spikes", title: "Spikes", kind: .integer),
        STPDResultSemanticColumn(id: "n_isi", title: "ISIs", kind: .integer),
        STPDResultSemanticColumn(id: "isi_range", title: "ISI range", kind: .identifier),
        STPDResultSemanticColumn(id: "median", title: "Median ISI", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "q10", title: "ISI q10", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "q90", title: "ISI q90", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "max", title: "Max ISI", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "mean", title: "Mean ISI", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "cv", title: "CV", kind: .real),
        STPDResultSemanticColumn(id: "cv2", title: "CV2", kind: .real),
        STPDResultSemanticColumn(id: "lv", title: "LV", kind: .real),
        STPDResultSemanticColumn(id: "pre_gap", title: "Pre gap", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "post_gap", title: "Post gap", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "pre_contrast", title: "Pre contrast", kind: .real),
        STPDResultSemanticColumn(id: "post_contrast", title: "Post contrast", kind: .real),
        STPDResultSemanticColumn(id: "edge_contrast", title: "Edge contrast", kind: .real),
        STPDResultSemanticColumn(id: "score", title: "Score", kind: .real),
        STPDResultSemanticColumn(id: "confidence", title: "Confidence", kind: .status),
        STPDResultSemanticColumn(id: "authority", title: "Authority", kind: .status),
        STPDResultSemanticColumn(id: "reason", title: "Reason", kind: .longText),
        STPDResultSemanticColumn(
            id: "metric_source",
            title: "Metric source",
            kind: .status,
            help: "Candidate metrics are shown only for one unambiguous source candidate."
        ),
        STPDResultSemanticColumn(id: "decision_path", title: "Decision path", kind: .longText),
        STPDResultSemanticColumn(id: "event_id", title: "Event ID", kind: .identifier),
    ]

    static let candidateColumns = [
        STPDResultSemanticColumn(id: "train", title: "Train", kind: .identifier),
        STPDResultSemanticColumn(id: "class", title: "Candidate class", kind: .text),
        STPDResultSemanticColumn(id: "final_label", title: "Final label", kind: .status),
        STPDResultSemanticColumn(id: "gate", title: "Gate", kind: .status),
        STPDResultSemanticColumn(id: "action", title: "Action", kind: .status),
        STPDResultSemanticColumn(id: "selected", title: "Selected", kind: .status),
        STPDResultSemanticColumn(id: "review", title: "Review", kind: .status),
        STPDResultSemanticColumn(id: "isi_range", title: "ISI range", kind: .identifier),
        STPDResultSemanticColumn(id: "n_isi", title: "ISIs", kind: .integer),
        STPDResultSemanticColumn(id: "n_spikes", title: "Spikes", kind: .integer),
        STPDResultSemanticColumn(id: "q10", title: "ISI q10", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "median", title: "Median ISI", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "q90", title: "ISI q90", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "max", title: "Max ISI", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "mean", title: "Mean ISI", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "cv", title: "CV", kind: .real),
        STPDResultSemanticColumn(id: "cv2", title: "CV2", kind: .real),
        STPDResultSemanticColumn(id: "lv", title: "LV", kind: .real),
        STPDResultSemanticColumn(id: "pre_gap", title: "Pre gap", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "post_gap", title: "Post gap", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "pre_contrast", title: "Pre contrast", kind: .real),
        STPDResultSemanticColumn(id: "post_contrast", title: "Post contrast", kind: .real),
        STPDResultSemanticColumn(id: "edge_contrast", title: "Edge contrast", kind: .real),
        STPDResultSemanticColumn(id: "geom_contrast", title: "Geometric contrast", kind: .real),
        STPDResultSemanticColumn(id: "required_contrast", title: "Required contrast", kind: .real),
        STPDResultSemanticColumn(id: "boundary_source", title: "Boundary source", kind: .text),
        STPDResultSemanticColumn(id: "score", title: "Score", kind: .real),
        STPDResultSemanticColumn(id: "confidence", title: "Confidence", kind: .status),
        STPDResultSemanticColumn(id: "selection", title: "Selection status", kind: .status),
        STPDResultSemanticColumn(id: "uncertainty", title: "Uncertainty", kind: .longText),
        STPDResultSemanticColumn(id: "failure", title: "Failure reason", kind: .longText),
        STPDResultSemanticColumn(id: "layer", title: "Layer", kind: .text),
        STPDResultSemanticColumn(id: "decision_path", title: "Decision path", kind: .longText),
        STPDResultSemanticColumn(id: "candidate_id", title: "Candidate ID", kind: .identifier),
    ]

    static let isiColumns = [
        STPDResultSemanticColumn(id: "train", title: "Train", kind: .identifier),
        STPDResultSemanticColumn(id: "isi_index", title: "ISI index", kind: .integer),
        STPDResultSemanticColumn(id: "aligned_time", title: "Aligned time", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "raw_time", title: "Raw time", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "isi", title: "ISI", unit: "s", kind: .real),
        STPDResultSemanticColumn(id: "final_pattern", title: "Final pattern", kind: .status),
        STPDResultSemanticColumn(id: "final_subtype", title: "Final subtype", kind: .text),
        STPDResultSemanticColumn(id: "final_source", title: "Final source", kind: .status),
        STPDResultSemanticColumn(id: "auto_pattern", title: "Auto pattern", kind: .status),
        STPDResultSemanticColumn(id: "auto_subtype", title: "Auto subtype", kind: .text),
        STPDResultSemanticColumn(id: "changed", title: "Changed by review", kind: .status),
        STPDResultSemanticColumn(id: "qc_class", title: "QC class", kind: .status),
        STPDResultSemanticColumn(id: "artifact", title: "Below minimum ISI", kind: .status),
        STPDResultSemanticColumn(id: "refractory", title: "Refractory suspect", kind: .status),
        STPDResultSemanticColumn(id: "review_note", title: "Review note", kind: .longText),
        STPDResultSemanticColumn(id: "isi_id", title: "ISI ID", kind: .identifier),
    ]

    static let burstFamilyLabels: Set<String> = [
        "burst", "high_frequency_burst", "long_burst", "possible_burst",
        "possible_burst_review", "burst_ii",
    ]

    static let patternSpecifications = [
        PatternSpecification(
            id: "burst",
            title: "Burst events",
            subtitle: "Canonical burst, high-frequency burst, and Burst II records.",
            labels: ["burst", "high_frequency_burst", "burst_ii"]
        ),
        PatternSpecification(
            id: "long_burst",
            title: "Long-burst events",
            subtitle: "Events whose final scientific class is long burst.",
            labels: ["long_burst"]
        ),
        PatternSpecification(
            id: "possible_burst",
            title: "Possible-burst review",
            subtitle: "Possible-burst and possible-burst-review records kept separate from canonical bursts.",
            labels: ["possible_burst", "possible_burst_review"]
        ),
        PatternSpecification(
            id: "tonic",
            title: "Tonic epochs",
            subtitle: "Classic tonic state epochs.",
            labels: ["tonic"]
        ),
        PatternSpecification(
            id: "high_frequency_tonic",
            title: "High-frequency tonic epochs",
            subtitle: "High-frequency tonic state epochs.",
            labels: ["high_frequency_tonic"]
        ),
        PatternSpecification(
            id: "high_frequency_spiking",
            title: "High-frequency spiking epochs",
            subtitle: "High-frequency spiking states, kept distinct from tonic and burst.",
            labels: ["high_frequency_spiking"]
        ),
        PatternSpecification(
            id: "pause",
            title: "Pause events",
            subtitle: "Final pause/gap events.",
            labels: ["pause"]
        ),
    ]

    static func indexed(
        _ sourceTables: [STPDResultSemanticSourceTable]
    ) throws -> [String: IndexedTable] {
        var result: [String: IndexedTable] = [:]
        for source in sourceTables {
            guard result[source.fileName] == nil else {
                throw STPDResultSemanticPresentationError.duplicateTable(source.fileName)
            }
            var columnIndex: [String: Int] = [:]
            for (index, column) in source.columns.enumerated() {
                guard columnIndex[column.name] == nil else {
                    throw STPDResultSemanticPresentationError.duplicateColumn(
                        table: source.fileName,
                        column: column.name
                    )
                }
                columnIndex[column.name] = index
            }
            for (rowIndex, row) in source.rows.enumerated() {
                guard row.count == source.columns.count else {
                    throw STPDResultSemanticPresentationError.invalidRowWidth(
                        table: source.fileName,
                        row: rowIndex,
                        expected: source.columns.count,
                        actual: row.count
                    )
                }
            }
            result[source.fileName] = IndexedTable(
                source: source,
                columnIndex: columnIndex
            )
        }
        return result
    }

    static func keyedRows(
        _ table: IndexedTable?,
        key: String
    ) throws -> [String: [String]] {
        guard let table else {
            return [:]
        }
        _ = try table.require(key)
        var result: [String: [String]] = [:]
        for row in table.rows {
            let value = table.value(row, key)
            guard result[value] == nil else {
                throw STPDResultSemanticPresentationError.duplicateKey(
                    table: table.source.fileName,
                    column: key,
                    value: value
                )
            }
            result[value] = row
        }
        return result
    }

    static func requireAllOrNone(
        _ tables: [String: IndexedTable],
        names: [String]
    ) throws {
        let presentCount = names.filter { tables[$0] != nil }.count
        guard presentCount == 0 || presentCount == names.count else {
            throw STPDResultSemanticPresentationError.incompleteTableSet(names)
        }
    }

    static func requireColumns(
        _ table: IndexedTable?,
        columns: [String]
    ) throws {
        guard let table else { return }
        for column in columns {
            _ = try table.require(column)
        }
    }

    static func requireEventJoinColumns(
        eventTable: IndexedTable,
        featureTable: IndexedTable?,
        decisionTable: IndexedTable?
    ) throws {
        try requireColumns(
            eventTable,
            columns: [
                "event_uid", "train_id", "train_name", "final_label",
                "final_subtype", "state_tonic_subtype",
                "state_high_frequency_subtypes", "authority_origin",
                "start_isi_index", "end_isi_index", "raw_start_sec", "raw_end_sec",
                "aligned_start_sec", "aligned_end_sec", "duration_sec", "score",
                "source_candidate_uids", "audit_recommended_subtype",
                "audit_review_status", "decision_path",
            ]
        )
        try requireColumns(
            featureTable,
            columns: [
                "candidate_uid", "pre_gap_sec", "post_gap_sec", "intra_q10_sec",
                "intra_q50_sec", "intra_q90_sec", "max_intra_isi_sec",
                "mean_intra_isi_sec", "cv", "cv2", "lv", "pre_ratio_q90",
                "post_ratio_q90", "edge_contrast_min_q90",
                "edge_contrast_geom_q90", "score",
                "anchor_band_source", "anchor_contrast_geom_required",
            ]
        )
        try requireColumns(
            decisionTable,
            columns: [
                "candidate_uid", "final_label", "gate_status", "action",
                "selected_for_auto", "selection_status", "audit_review_status",
                "audit_confidence_tier", "audit_uncertainty_reason",
                "failure_reason", "decision_path",
            ]
        )
    }

    static func requireExactKeyCoverage(
        referenceRows: [String: [String]],
        joinedTable: IndexedTable,
        joinedRows: [String: [String]],
        key: String
    ) throws {
        for value in referenceRows.keys.sorted() where joinedRows[value] == nil {
            throw STPDResultSemanticPresentationError.missingJoinedRow(
                table: joinedTable.source.fileName,
                column: key,
                value: value
            )
        }
        for value in joinedRows.keys.sorted() where referenceRows[value] == nil {
            throw STPDResultSemanticPresentationError.orphanJoinedRow(
                table: joinedTable.source.fileName,
                column: key,
                value: value
            )
        }
    }

    static func eventRecords(
        eventTable: IndexedTable,
        featureTable: IndexedTable?,
        featureRows: [String: [String]],
        decisionTable: IndexedTable?,
        decisionRows: [String: [String]]
    ) throws -> [EventRecord] {
        var seenIDs = Set<String>()
        var records: [EventRecord] = []
        for sourceRow in eventTable.rows {
            let eventID = eventTable.value(sourceRow, "event_uid")
            guard seenIDs.insert(eventID).inserted else {
                throw STPDResultSemanticPresentationError.duplicateKey(
                    table: eventTable.source.fileName,
                    column: "event_uid",
                    value: eventID
                )
            }
            let startText = eventTable.value(sourceRow, "start_isi_index")
            let endText = eventTable.value(sourceRow, "end_isi_index")
            guard let start = Int(startText) else {
                throw STPDResultSemanticPresentationError.invalidInteger(
                    table: eventTable.source.fileName,
                    column: "start_isi_index",
                    value: startText
                )
            }
            guard let end = Int(endText), end >= start else {
                throw STPDResultSemanticPresentationError.invalidInteger(
                    table: eventTable.source.fileName,
                    column: "end_isi_index",
                    value: endText
                )
            }
            let candidateUIDs = try parseStringList(
                eventTable.value(sourceRow, "source_candidate_uids"),
                table: eventTable.source.fileName,
                column: "source_candidate_uids"
            )
            let featureRow: [String]?
            let decisionRow: [String]?
            let metricAvailability: String
            for candidateUID in candidateUIDs {
                guard featureTable != nil else {
                    throw STPDResultSemanticPresentationError.missingJoinedRow(
                        table: STPDResultTable.candidateFeatures.rawValue,
                        column: "candidate_uid",
                        value: candidateUID
                    )
                }
                guard featureRows[candidateUID] != nil else {
                    throw STPDResultSemanticPresentationError.missingJoinedRow(
                        table: featureTable?.source.fileName
                            ?? STPDResultTable.candidateFeatures.rawValue,
                        column: "candidate_uid",
                        value: candidateUID
                    )
                }
                guard decisionTable != nil else {
                    throw STPDResultSemanticPresentationError.missingJoinedRow(
                        table: STPDResultTable.finalDecisions.rawValue,
                        column: "candidate_uid",
                        value: candidateUID
                    )
                }
                guard decisionRows[candidateUID] != nil else {
                    throw STPDResultSemanticPresentationError.missingJoinedRow(
                        table: decisionTable?.source.fileName
                            ?? STPDResultTable.finalDecisions.rawValue,
                        column: "candidate_uid",
                        value: candidateUID
                    )
                }
            }
            if candidateUIDs.count == 1 {
                featureRow = featureRows[candidateUIDs[0]]
                decisionRow = decisionRows[candidateUIDs[0]]
                metricAvailability = "single source candidate"
            } else if candidateUIDs.isEmpty {
                featureRow = nil
                decisionRow = nil
                metricAvailability = "event geometry only"
            } else {
                featureRow = nil
                decisionRow = nil
                metricAvailability = "multiple source candidates"
            }

            func feature(_ column: String) -> String {
                guard let featureTable, let featureRow else {
                    return ""
                }
                return featureTable.value(featureRow, column)
            }
            func decision(_ column: String) -> String {
                guard let decisionTable, let decisionRow else {
                    return ""
                }
                return decisionTable.value(decisionRow, column)
            }

            let pattern = eventTable.value(sourceRow, "final_label")
            let normalizedPattern = normalize(pattern)
            let reviewStatus = eventTable.value(sourceRow, "audit_review_status")
            let acceptedStatus = ["accepted", "manual"].contains(normalize(reviewStatus))
            let isPossibleBurst = normalizedPattern == "possible_burst"
                || normalizedPattern == "possible_burst_review"
                || normalize(eventTable.value(sourceRow, "audit_recommended_subtype"))
                    .contains("possible_burst")
            let subtype = firstNonEmpty([
                eventTable.value(sourceRow, "final_subtype"),
                eventTable.value(sourceRow, "state_tonic_subtype"),
                eventTable.value(sourceRow, "state_high_frequency_subtypes"),
                eventTable.value(sourceRow, "audit_recommended_subtype"),
            ])
            let reason = firstNonEmpty([
                decision("audit_uncertainty_reason"),
                decision("failure_reason"),
                eventTable.value(sourceRow, "decision_path"),
            ])
            let nISI = end - start + 1
            let valuesByColumnID = [
                "event_id": eventID,
                "train": firstNonEmpty([
                    eventTable.value(sourceRow, "train_name"),
                    eventTable.value(sourceRow, "train_id"),
                ]),
                "pattern": pattern,
                "subtype": subtype,
                "review": reviewStatus,
                "authority": eventTable.value(sourceRow, "authority_origin"),
                "isi_range": "\(start)–\(end)",
                "n_isi": "\(nISI)",
                "n_spikes": "\(nISI + 1)",
                "aligned_start": eventTable.value(sourceRow, "aligned_start_sec"),
                "aligned_end": eventTable.value(sourceRow, "aligned_end_sec"),
                "raw_start": eventTable.value(sourceRow, "raw_start_sec"),
                "raw_end": eventTable.value(sourceRow, "raw_end_sec"),
                "duration": eventTable.value(sourceRow, "duration_sec"),
                "pre_gap": feature("pre_gap_sec"),
                "post_gap": feature("post_gap_sec"),
                "q10": feature("intra_q10_sec"),
                "median": feature("intra_q50_sec"),
                "q90": feature("intra_q90_sec"),
                "max": feature("max_intra_isi_sec"),
                "mean": feature("mean_intra_isi_sec"),
                "cv": feature("cv"),
                "cv2": feature("cv2"),
                "lv": feature("lv"),
                "pre_contrast": feature("pre_ratio_q90"),
                "post_contrast": feature("post_ratio_q90"),
                "edge_contrast": feature("edge_contrast_min_q90"),
                "score": firstNonEmpty([
                    eventTable.value(sourceRow, "score"),
                    feature("score"),
                ]),
                "confidence": decision("audit_confidence_tier"),
                "reason": reason,
                "metric_source": metricAvailability,
                "decision_path": eventTable.value(sourceRow, "decision_path"),
            ]
            records.append(
                EventRecord(
                    id: eventID,
                    normalizedPattern: normalizedPattern,
                    isAuthoritative: acceptedStatus && !isPossibleBurst,
                    isHighConfidence:
                        normalize(eventTable.value(sourceRow, "authority_origin"))
                            == "automatic"
                        && normalize(reviewStatus) == "accepted"
                        && normalize(decision("audit_confidence_tier")) == "high"
                        && !isPossibleBurst,
                    metricAvailability: metricAvailability,
                    sortTrain: eventTable.value(sourceRow, "train_id"),
                    sortStart: start,
                    row: projectedRow(
                        id: eventID,
                        columns: eventColumns,
                        valuesByColumnID: valuesByColumnID
                    )
                )
            )
        }
        return records.sorted {
            if $0.sortTrain != $1.sortTrain { return $0.sortTrain < $1.sortTrain }
            if $0.sortStart != $1.sortStart { return $0.sortStart < $1.sortStart }
            return $0.id < $1.id
        }
    }

    static func eventTable(
        id: String,
        title: String,
        subtitle: String,
        records: [EventRecord],
        sources: [String],
        group: STPDResultSemanticTableGroup = .review
    ) -> STPDResultSemanticTable {
        STPDResultSemanticTable(
            id: id,
            title: title,
            subtitle: subtitle,
            group: group,
            sourceFileNames: sources,
            columns: eventColumns,
            rows: records.map(\.row)
        )
    }

    static func candidatePresentation(
        indexedTables: [String: IndexedTable],
        ledgerName: String,
        featureName: String,
        decisionName: String,
        id: String,
        title: String,
        subtitle: String
    ) throws -> STPDResultSemanticTable? {
        let presence = [
            indexedTables[ledgerName] != nil,
            indexedTables[featureName] != nil,
            indexedTables[decisionName] != nil,
        ]
        if presence.allSatisfy({ !$0 }) {
            return nil
        }
        guard presence.allSatisfy({ $0 }) else {
            throw STPDResultSemanticPresentationError.incompleteTableSet(
                [ledgerName, featureName, decisionName]
            )
        }
        guard let ledger = indexedTables[ledgerName],
              let features = indexedTables[featureName],
              let decisions = indexedTables[decisionName] else {
            throw STPDResultSemanticPresentationError.incompleteTableSet(
                [ledgerName, featureName, decisionName]
            )
        }
        try requireColumns(
            ledger,
            columns: [
                "candidate_uid", "train_id", "train_name", "candidate_layer",
                "candidate_class", "start_isi_index", "end_isi_index", "n_isi",
                "n_spikes", "final_label", "gate_status", "action",
                "selected_for_auto", "selection_status",
            ]
        )
        try requireColumns(
            features,
            columns: [
                "candidate_uid", "pre_gap_sec", "post_gap_sec", "intra_q10_sec",
                "intra_q50_sec", "intra_q90_sec", "max_intra_isi_sec",
                "mean_intra_isi_sec", "cv", "cv2", "lv", "pre_ratio_q90",
                "post_ratio_q90", "edge_contrast_min_q90",
                "edge_contrast_geom_q90", "score", "anchor_band_source",
                "anchor_contrast_geom_required",
            ]
        )
        try requireColumns(
            decisions,
            columns: [
                "candidate_uid", "final_label", "gate_status", "action",
                "selected_for_auto", "selection_status", "audit_review_status",
                "audit_confidence_tier", "audit_uncertainty_reason",
                "failure_reason", "decision_path",
            ]
        )
        let ledgerRows = try keyedRows(ledger, key: "candidate_uid")
        let featureRows = try keyedRows(features, key: "candidate_uid")
        let decisionRows = try keyedRows(decisions, key: "candidate_uid")
        try requireExactKeyCoverage(
            referenceRows: ledgerRows,
            joinedTable: features,
            joinedRows: featureRows,
            key: "candidate_uid"
        )
        try requireExactKeyCoverage(
            referenceRows: ledgerRows,
            joinedTable: decisions,
            joinedRows: decisionRows,
            key: "candidate_uid"
        )

        var sortableRows: [(train: String, start: Int, row: STPDResultSemanticRow)] = []
        for ledgerRow in ledger.rows {
            let uid = ledger.value(ledgerRow, "candidate_uid")
            let startText = ledger.value(ledgerRow, "start_isi_index")
            let endText = ledger.value(ledgerRow, "end_isi_index")
            guard let start = Int(startText) else {
                throw STPDResultSemanticPresentationError.invalidInteger(
                    table: ledgerName,
                    column: "start_isi_index",
                    value: startText
                )
            }
            guard let end = Int(endText), end >= start else {
                throw STPDResultSemanticPresentationError.invalidInteger(
                    table: ledgerName,
                    column: "end_isi_index",
                    value: endText
                )
            }
            guard let featureRow = featureRows[uid] else {
                throw STPDResultSemanticPresentationError.missingJoinedRow(
                    table: featureName,
                    column: "candidate_uid",
                    value: uid
                )
            }
            guard let decisionRow = decisionRows[uid] else {
                throw STPDResultSemanticPresentationError.missingJoinedRow(
                    table: decisionName,
                    column: "candidate_uid",
                    value: uid
                )
            }
            func feature(_ column: String) -> String {
                return features.value(featureRow, column)
            }
            func decision(_ column: String) -> String {
                return decisions.value(decisionRow, column)
            }
            let valuesByColumnID = [
                "candidate_id": uid,
                "train": firstNonEmpty([
                    ledger.value(ledgerRow, "train_name"),
                    ledger.value(ledgerRow, "train_id"),
                ]),
                "layer": ledger.value(ledgerRow, "candidate_layer"),
                "class": ledger.value(ledgerRow, "candidate_class"),
                "final_label": firstNonEmpty([
                    decision("final_label"),
                    ledger.value(ledgerRow, "final_label"),
                ]),
                "gate": firstNonEmpty([
                    decision("gate_status"),
                    ledger.value(ledgerRow, "gate_status"),
                ]),
                "action": firstNonEmpty([
                    decision("action"),
                    ledger.value(ledgerRow, "action"),
                ]),
                "selected": firstNonEmpty([
                    decision("selected_for_auto"),
                    ledger.value(ledgerRow, "selected_for_auto"),
                ]),
                "selection": firstNonEmpty([
                    decision("selection_status"),
                    ledger.value(ledgerRow, "selection_status"),
                ]),
                "isi_range": "\(start)–\(end)",
                "n_isi": ledger.value(ledgerRow, "n_isi"),
                "n_spikes": ledger.value(ledgerRow, "n_spikes"),
                "q10": feature("intra_q10_sec"),
                "median": feature("intra_q50_sec"),
                "q90": feature("intra_q90_sec"),
                "max": feature("max_intra_isi_sec"),
                "mean": feature("mean_intra_isi_sec"),
                "cv": feature("cv"),
                "cv2": feature("cv2"),
                "lv": feature("lv"),
                "pre_gap": feature("pre_gap_sec"),
                "post_gap": feature("post_gap_sec"),
                "pre_contrast": feature("pre_ratio_q90"),
                "post_contrast": feature("post_ratio_q90"),
                "edge_contrast": feature("edge_contrast_min_q90"),
                "geom_contrast": feature("edge_contrast_geom_q90"),
                "required_contrast": feature("anchor_contrast_geom_required"),
                "boundary_source": feature("anchor_band_source"),
                "score": feature("score"),
                "review": decision("audit_review_status"),
                "confidence": decision("audit_confidence_tier"),
                "uncertainty": decision("audit_uncertainty_reason"),
                "failure": decision("failure_reason"),
                "decision_path": decision("decision_path"),
            ]
            sortableRows.append(
                (
                    train: ledger.value(ledgerRow, "train_id"),
                    start: start,
                    row: projectedRow(
                        id: uid,
                        columns: candidateColumns,
                        valuesByColumnID: valuesByColumnID
                    )
                )
            )
        }
        let rows = sortableRows.sorted {
            if $0.train != $1.train { return $0.train < $1.train }
            if $0.start != $1.start { return $0.start < $1.start }
            return $0.row.id < $1.row.id
        }.map(\.row)
        return STPDResultSemanticTable(
            id: id,
            title: title,
            subtitle: subtitle,
            group: .diagnostic,
            sourceFileNames: [ledgerName, featureName, decisionName],
            columns: candidateColumns,
            rows: rows
        )
    }

    static func isiPresentation(
        _ source: IndexedTable
    ) throws -> STPDResultSemanticTable {
        for column in [
            "isi_uid", "train_id", "train_name", "isi_index", "timestamp_sec",
            "aligned_timestamp_sec", "isi_sec", "auto_pattern", "auto_subtype",
            "final_pattern", "final_subtype", "final_source", "isi_qc_class",
            "artifact_floor_status", "qc_refractory_suspect", "review_note",
            "review_changed_projection",
        ] {
            _ = try source.require(column)
        }
        var seen = Set<String>()
        var sortableRows: [(train: String, index: Int, row: STPDResultSemanticRow)] = []
        for sourceRow in source.rows {
            let uid = source.value(sourceRow, "isi_uid")
            guard seen.insert(uid).inserted else {
                throw STPDResultSemanticPresentationError.duplicateKey(
                    table: source.source.fileName,
                    column: "isi_uid",
                    value: uid
                )
            }
            let indexText = source.value(sourceRow, "isi_index")
            guard let index = Int(indexText) else {
                throw STPDResultSemanticPresentationError.invalidInteger(
                    table: source.source.fileName,
                    column: "isi_index",
                    value: indexText
                )
            }
            let valuesByColumnID = [
                "isi_id": uid,
                "train": firstNonEmpty([
                    source.value(sourceRow, "train_name"),
                    source.value(sourceRow, "train_id"),
                ]),
                "isi_index": indexText,
                "aligned_time": source.value(sourceRow, "aligned_timestamp_sec"),
                "raw_time": source.value(sourceRow, "timestamp_sec"),
                "isi": source.value(sourceRow, "isi_sec"),
                "auto_pattern": source.value(sourceRow, "auto_pattern"),
                "auto_subtype": source.value(sourceRow, "auto_subtype"),
                "final_pattern": source.value(sourceRow, "final_pattern"),
                "final_subtype": source.value(sourceRow, "final_subtype"),
                "final_source": source.value(sourceRow, "final_source"),
                "qc_class": source.value(sourceRow, "isi_qc_class"),
                "artifact": source.value(sourceRow, "artifact_floor_status"),
                "refractory": source.value(sourceRow, "qc_refractory_suspect"),
                "review_note": source.value(sourceRow, "review_note"),
                "changed": source.value(sourceRow, "review_changed_projection"),
            ]
            sortableRows.append(
                (
                    train: source.value(sourceRow, "train_id"),
                    index: index,
                    row: projectedRow(
                        id: uid,
                        columns: isiColumns,
                        valuesByColumnID: valuesByColumnID
                    )
                )
            )
        }
        return STPDResultSemanticTable(
            id: isiLabelsTableID,
            title: "Per-ISI final labels",
            subtitle: "Final and automatic labels, provenance, review changes, and QC flags for every ISI.",
            group: .isi,
            sourceFileNames: [source.source.fileName],
            columns: isiColumns,
            rows: sortableRows.sorted {
                if $0.train != $1.train { return $0.train < $1.train }
                if $0.index != $1.index { return $0.index < $1.index }
                return $0.row.id < $1.row.id
            }.map(\.row)
        )
    }

    static func optionalInteger(
        table: IndexedTable?,
        row: [String]?,
        column: String
    ) throws -> Int? {
        guard let table, let row else {
            return nil
        }
        let value = table.value(row, column)
        guard !value.isEmpty else {
            return nil
        }
        guard let integer = Int(value) else {
            throw STPDResultSemanticPresentationError.invalidInteger(
                table: table.source.fileName,
                column: column,
                value: value
            )
        }
        return integer
    }

    static func parseStringList(
        _ value: String,
        table: String,
        column: String
    ) throws -> [String] {
        guard let data = value.data(using: .utf8),
              let decoded = try? JSONDecoder().decode([String].self, from: data) else {
            throw STPDResultSemanticPresentationError.invalidStringList(
                table: table,
                column: column
            )
        }
        return decoded
    }

    static func normalize(_ value: String) -> String {
        value.trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased()
            .replacingOccurrences(of: "-", with: "_")
            .replacingOccurrences(of: " ", with: "_")
    }

    static func projectedRow(
        id: String,
        columns: [STPDResultSemanticColumn],
        valuesByColumnID: [String: String]
    ) -> STPDResultSemanticRow {
        STPDResultSemanticRow(
            id: id,
            values: columns.map { valuesByColumnID[$0.id] ?? "" }
        )
    }

    static func firstNonEmpty(_ values: [String]) -> String {
        values.first {
            !$0.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
        } ?? ""
    }
}
