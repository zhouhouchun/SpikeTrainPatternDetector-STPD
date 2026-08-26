import CryptoKit
import Foundation

public struct CanonicalManualResultPackageFile: Codable, Hashable, Sendable {
    public let fileName: String
    public let sha256: String
    public let byteCount: Int
    public let rowCount: Int
}

public struct CanonicalManualResultPackageManifest: Codable, Hashable, Sendable {
    public let schemaContractID: String
    public let schemaContractDigest: String
    public let canonicalSchemaContractID: String
    public let canonicalSchemaContractDigest: String
    public let canonicalDatasetDigest: String
    public let confirmedImportRecordDigest: String
    public let manualDecisionDigest: String
    public let reviewer: String
    public let confirmedAtUnixSeconds: String
    public let sourceMode: String
    public let files: [CanonicalManualResultPackageFile]

    public func encodedData() throws -> Data {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
        return try encoder.encode(self)
    }
}

public struct CanonicalManualResultPackage: Sendable {
    public let manifest: CanonicalManualResultPackageManifest
    public let isiLabelsCSV: Data

    init(
        manifest: CanonicalManualResultPackageManifest,
        isiLabelsCSV: Data
    ) {
        self.manifest = manifest
        self.isiLabelsCSV = isiLabelsCSV
    }
}

public struct CanonicalManualResultPackageReadRow: Identifiable, Hashable, Sendable {
    public let trainID: String
    public let isiIndex: Int
    public let leftTimestampMicroseconds: Int64
    public let rightTimestampMicroseconds: Int64
    public let intervalMicroseconds: Int64
    public let statePattern: String
    public let eventPattern: String
    public let otherPattern: String

    public var id: String { "\(trainID)\u{1}\(isiIndex)" }
}

public struct CanonicalManualResultPackageReadResult: Sendable {
    public let manifest: CanonicalManualResultPackageManifest
    public let rows: [CanonicalManualResultPackageReadRow]
}

public enum CanonicalManualResultPackageError: Error, Equatable, Sendable, LocalizedError {
    case canonicalFingerprintMismatch
    case confirmationDigestMismatch
    case activityModeNotSupported
    case temporalContractNotEligible
    case invalidUTF8Encoding

    public var errorDescription: String? {
        switch self {
        case .canonicalFingerprintMismatch:
            return "The confirmed manual review and persisted scientific import have different canonical identities."
        case .confirmationDigestMismatch:
            return "The manual decision evidence does not reproduce its confirmed digest."
        case .activityModeNotSupported:
            return "Complete biological manual-result packages currently require putative single-unit data."
        case .temporalContractNotEligible:
            return "Manual-result sealing requires continuous-untrialed data with full imported-excerpt coverage."
        case .invalidUTF8Encoding:
            return "The canonical manual result could not be encoded as UTF-8."
        }
    }
}

/// Exports the current identity-bound manual draft without requiring complete review. This is a
/// convenience/audit artifact only: blank State/Event/Other cells remain explicitly unreviewed and
/// the authority column prevents the CSV from being mistaken for a sealed scientific result.
public enum CanonicalManualISIDraftCSVExporter {
    public static let schemaContractID = "canonical_manual_isi_draft_csv"
    public static let authorityStatus = "identity_bound_manual_draft_unsealed"
    public static let headers = [
        "draft_schema_contract_id", "draft_schema_contract_digest",
        "canonical_schema_contract_id", "canonical_schema_contract_digest",
        "canonical_dataset_digest", "confirmed_import_record_digest",
        "train_id", "isi_index", "left_timestamp_us", "right_timestamp_us", "isi_us",
        "state_pattern", "event_pattern", "other_pattern",
        "review_status", "authority_status",
    ]

    public static var schemaContractDigest: String {
        STPDStableIdentifier.digest(Data(schemaTranscript.utf8))
    }

    public static func csv(
        persistedImport: PersistedConfirmedScientificImportManifest,
        draft: CanonicalManualISILabelDraft
    ) throws -> String {
        try table(persistedImport: persistedImport, draft: draft).csv(lineEnding: "\r\n")
    }

    public static func table(
        persistedImport: PersistedConfirmedScientificImportManifest,
        draft: CanonicalManualISILabelDraft
    ) throws -> ManualISIExportTable {
        let shadow = try CanonicalScientificImportProjector.project(
            persistedImport.baseConfirmation.validatedImport
        )
        guard shadow.fingerprint == persistedImport.canonicalFingerprint,
              draft.canonicalFingerprint == shadow.fingerprint else {
            throw CanonicalManualResultPackageError.canonicalFingerprintMismatch
        }
        let rows = try CanonicalManualISILabelProjector.rows(
            dataset: shadow.dataset,
            fingerprint: shadow.fingerprint,
            draft: draft
        )
        return ManualISIExportTable(headers: headers, rows: rows.map { row in
            [
                schemaContractID,
                schemaContractDigest,
                shadow.fingerprint.schemaContractID,
                shadow.fingerprint.schemaContractDigest,
                shadow.fingerprint.datasetDigest,
                persistedImport.confirmationRecordDigest,
                row.trainID.semanticID.canonicalText,
                String(row.isiIndex),
                String(row.leftTick.microseconds),
                String(row.rightTick.microseconds),
                String(row.intervalMicroseconds),
                row.stateLabel?.rawValue ?? "",
                row.eventLabel?.rawValue ?? "",
                row.otherLabel?.rawValue ?? "",
                row.isReviewed ? "manually_labeled" : "unreviewed",
                authorityStatus,
            ]
        })
    }

    private static let schemaTranscript = """
    schema_contract_id=canonical_manual_isi_draft_csv
    source_mode=partial_manual_review_draft
    grain=one_row_per_real_canonical_isi
    timestamp_unit=integer_microseconds
    state_event_tracks=independent
    other_track=exclusive_with_state_and_event
    unreviewed_rows=retained_with_blank_pattern_columns
    dataset_identity=canonical_schema_id+canonical_schema_digest+canonical_dataset_digest
    import_identity=confirmed_import_record_digest
    authority_status=identity_bound_manual_draft_unsealed
    row_order=canonical_train_id_utf8_then_isi_index_ascending
    csv_line_ending=crlf
    """
}

/// Builds the manual-only `.stpdresult` scientific payload. It consumes no detector run and never
/// converts canonical integer microseconds to legacy `Double` for identity or geometry.
public enum CanonicalManualResultPackageBuilder {
    public static let schemaContractID = "canonical_manual_isi_result_package"
    public static let manifestFileName = "manifest.json"
    public static let isiLabelsFileName = "ISI_labels_manual_final.csv"
    public static let sourceMode = "complete_manual_review"
    public static let isiLabelsHeaders = [
        "canonical_schema_contract_id", "canonical_schema_contract_digest",
        "canonical_dataset_digest", "confirmed_import_record_digest",
        "manual_decision_digest", "train_id", "isi_index",
        "left_timestamp_us", "right_timestamp_us", "isi_us",
        "state_pattern", "event_pattern", "other_pattern",
        "reviewer", "confirmed_at_unix_sec", "authority_status",
    ]

    public static var schemaContractDigest: String {
        sha256(Data(schemaTranscript.utf8))
    }

    public static func build(
        persistedImport: PersistedConfirmedScientificImportManifest,
        confirmedLabels: ConfirmedCanonicalManualISILabels
    ) throws -> CanonicalManualResultPackage {
        guard persistedImport.canonicalFingerprint
                == confirmedLabels.canonicalFingerprint else {
            throw CanonicalManualResultPackageError.canonicalFingerprintMismatch
        }
        let base = persistedImport.baseConfirmation
        guard base.activityMode == .putativeSingleUnit else {
            throw CanonicalManualResultPackageError.activityModeNotSupported
        }
        guard base.recordingSegment.regime == .continuousUntrialed,
              base.recordingSegment.importedExcerptCoverage
                == .allSpikeTrainsFullImportedExcerpt else {
            throw CanonicalManualResultPackageError.temporalContractNotEligible
        }
        let shadow = try CanonicalScientificImportProjector.project(
            base.validatedImport
        )
        guard shadow.fingerprint == persistedImport.canonicalFingerprint else {
            throw CanonicalManualResultPackageError.canonicalFingerprintMismatch
        }

        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: confirmedLabels.canonicalFingerprint,
            decisions: confirmedLabels.decisions
        )
        let rebuilt = try ConfirmedCanonicalManualISILabels.confirmComplete(
            draft: draft,
            dataset: shadow.dataset,
            fingerprint: shadow.fingerprint,
            reviewer: confirmedLabels.reviewer,
            confirmedAt: confirmedLabels.confirmedAt
        )
        guard rebuilt.decisionDigest == confirmedLabels.decisionDigest else {
            throw CanonicalManualResultPackageError.confirmationDigestMismatch
        }
        let rows = try CanonicalManualISILabelProjector.rows(
            dataset: shadow.dataset,
            fingerprint: shadow.fingerprint,
            draft: draft
        )
        let csv = csvData(
            rows: rows,
            fingerprint: shadow.fingerprint,
            confirmedImportRecordDigest:
                persistedImport.confirmationRecordDigest,
            confirmation: confirmedLabels
        )
        let table = CanonicalManualResultPackageFile(
            fileName: isiLabelsFileName,
            sha256: sha256(csv),
            byteCount: csv.count,
            rowCount: rows.count
        )
        let manifest = CanonicalManualResultPackageManifest(
            schemaContractID: schemaContractID,
            schemaContractDigest: schemaContractDigest,
            canonicalSchemaContractID: shadow.fingerprint.schemaContractID,
            canonicalSchemaContractDigest:
                shadow.fingerprint.schemaContractDigest,
            canonicalDatasetDigest: shadow.fingerprint.datasetDigest,
            confirmedImportRecordDigest:
                persistedImport.confirmationRecordDigest,
            manualDecisionDigest: confirmedLabels.decisionDigest,
            reviewer: confirmedLabels.reviewer,
            confirmedAtUnixSeconds: timestamp(confirmedLabels.confirmedAt),
            sourceMode: sourceMode,
            files: [table]
        )
        return CanonicalManualResultPackage(
            manifest: manifest,
            isiLabelsCSV: csv
        )
    }

    public static func validate(_ package: CanonicalManualResultPackage) throws {
        guard package.manifest.schemaContractID == schemaContractID,
              package.manifest.schemaContractDigest == schemaContractDigest,
              package.manifest.sourceMode == sourceMode,
              package.manifest.files.count == 1,
              let table = package.manifest.files.first,
              table.fileName == isiLabelsFileName,
              table.sha256 == sha256(package.isiLabelsCSV),
              table.byteCount == package.isiLabelsCSV.count,
              table.rowCount == max(0, lineCount(package.isiLabelsCSV) - 1) else {
            throw CanonicalManualResultPackageError.confirmationDigestMismatch
        }
    }

    private static func csvData(
        rows: [CanonicalManualISILabelRow],
        fingerprint: CanonicalScientificDatasetFingerprint,
        confirmedImportRecordDigest: String,
        confirmation: ConfirmedCanonicalManualISILabels
    ) -> Data {
        var lines = [isiLabelsHeaders.map(escape).joined(separator: ",")]
        lines.reserveCapacity(rows.count + 1)
        for row in rows {
            lines.append([
                fingerprint.schemaContractID,
                fingerprint.schemaContractDigest,
                fingerprint.datasetDigest,
                confirmedImportRecordDigest,
                confirmation.decisionDigest,
                row.trainID.semanticID.canonicalText,
                String(row.isiIndex),
                String(row.leftTick.microseconds),
                String(row.rightTick.microseconds),
                String(row.intervalMicroseconds),
                row.stateLabel?.rawValue ?? "",
                row.eventLabel?.rawValue ?? "",
                row.otherLabel?.rawValue ?? "",
                confirmation.reviewer,
                timestamp(confirmation.confirmedAt),
                "sealed_complete_manual_review",
            ].map(escape).joined(separator: ","))
        }
        return Data((lines.joined(separator: "\r\n") + "\r\n").utf8)
    }

    private static func lineCount(_ data: Data) -> Int {
        data.reduce(0) { $1 == 0x0A ? $0 + 1 : $0 }
    }

    private static func timestamp(_ date: Date) -> String {
        String(format: "%.6f", date.timeIntervalSince1970)
    }

    private static func escape(_ value: String) -> String {
        guard value.contains(",") || value.contains("\"")
                || value.contains("\n") || value.contains("\r") else {
            return value
        }
        return "\"" + value.replacingOccurrences(of: "\"", with: "\"\"") + "\""
    }

    private static func sha256(_ data: Data) -> String {
        SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
    }

    private static let schemaTranscript = """
    schema_contract_id=canonical_manual_isi_result_package
    source_mode=complete_manual_review
    file=ISI_labels_manual_final.csv
    grain=one_row_per_real_canonical_isi
    timestamp_unit=integer_microseconds
    state_event_tracks=independent
    other_track=exclusive_with_state_and_event
    review_coverage=all_real_isis
    dataset_identity=canonical_schema_id+canonical_schema_digest+canonical_dataset_digest
    confirmation_identity=confirmed_import_record_digest+manual_decision_digest
    row_order=canonical_train_id_utf8_then_isi_index_ascending
    csv_line_ending=crlf
    """
}
