import Foundation

/// A format-neutral, unsealed manual-ISI draft decoded from one of this app's own export tables.
/// The value is deliberately not authority-bearing: callers must validate it against the current
/// persisted import and reconstruct every real ISI before installing the returned draft.
public struct CanonicalManualISIDraftImport: Hashable, Sendable {
    public let canonicalSchemaContractID: String
    public let canonicalSchemaContractDigest: String
    public let canonicalDatasetDigest: String
    public let confirmedImportRecordDigest: String
    public let decisions: [CanonicalManualISILabelDecision]
    public let rowCount: Int
    public let labeledISIIndexCount: Int

    private let rows: [Row]

    public static func decode(
        tables: [ManualISIExportTable]
    ) throws -> CanonicalManualISIDraftImport {
        guard !tables.isEmpty else { throw Error.noWorksheets }

        var decodedRows: [Row] = []
        var identity: Identity?
        var seenRows = Set<RowKey>()
        var decisionValues: [CanonicalManualISILabelDecision] = []
        var labeledRows = Set<RowKey>()

        for (sheetOffset, table) in tables.enumerated() {
            guard table.headers == CanonicalManualISIDraftCSVExporter.headers else {
                throw Error.invalidHeaders(sheet: sheetOffset + 1)
            }
            guard !table.rows.isEmpty else {
                throw Error.emptyWorksheet(sheet: sheetOffset + 1)
            }
            for (rowOffset, fields) in table.rows.enumerated() {
                let row = try Row(fields: fields, sheet: sheetOffset + 1, row: rowOffset + 2)
                let rowIdentity = row.identity
                if let identity, identity != rowIdentity {
                    throw Error.inconsistentIdentity(sheet: sheetOffset + 1, row: rowOffset + 2)
                }
                identity = rowIdentity
                guard seenRows.insert(row.key).inserted else {
                    throw Error.duplicateISI(trainID: row.trainID, isiIndex: row.isiIndex)
                }
                if row.isReviewed { labeledRows.insert(row.key) }
                decisionValues.append(contentsOf: row.decisions)
                decodedRows.append(row)
            }
        }

        guard let identity else { throw Error.noRows }
        return CanonicalManualISIDraftImport(
            canonicalSchemaContractID: identity.canonicalSchemaContractID,
            canonicalSchemaContractDigest: identity.canonicalSchemaContractDigest,
            canonicalDatasetDigest: identity.canonicalDatasetDigest,
            confirmedImportRecordDigest: identity.confirmedImportRecordDigest,
            decisions: decisionValues,
            rowCount: decodedRows.count,
            labeledISIIndexCount: labeledRows.count,
            rows: decodedRows
        )
    }

    /// Rebuilds a safe draft only when the imported table is an exact one-row-per-real-ISI image
    /// of the current persisted canonical data. This check intentionally rejects exports from a
    /// different source, confirmation record, activity interpretation, or timestamp geometry.
    public func validatedDraft(
        dataset: CanonicalScientificDataset,
        persistedImport: PersistedConfirmedScientificImportManifest
    ) throws -> CanonicalManualISILabelDraft {
        try validatedDraft(
            dataset: dataset,
            fingerprint: persistedImport.canonicalFingerprint,
            confirmationRecordDigest: persistedImport.confirmationRecordDigest
        )
    }

    /// The same strict reconstruction used by the persisted-import overload. The explicit inputs
    /// keep the pure validation kernel independently testable; callers still need a persisted
    /// confirmation to obtain those values in the application workflow.
    public func validatedDraft(
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        confirmationRecordDigest: String
    ) throws -> CanonicalManualISILabelDraft {
        guard canonicalSchemaContractID == fingerprint.schemaContractID,
              canonicalSchemaContractDigest == fingerprint.schemaContractDigest,
              canonicalDatasetDigest == fingerprint.datasetDigest else {
            throw Error.canonicalIdentityMismatch
        }
        guard confirmedImportRecordDigest == confirmationRecordDigest else {
            throw Error.confirmationRecordMismatch
        }

        let empty = CanonicalManualISILabelDraft(canonicalFingerprint: fingerprint)
        let expected = try CanonicalManualISILabelProjector.rows(
            dataset: dataset,
            fingerprint: fingerprint,
            draft: empty
        )
        guard expected.count == rows.count else {
            throw Error.realISIRowCountMismatch(expected: expected.count, actual: rows.count)
        }
        var expectedByKey: [RowKey: CanonicalManualISILabelRow] = [:]
        expectedByKey.reserveCapacity(expected.count)
        for value in expected {
            let key = RowKey(
                trainID: value.trainID.semanticID.canonicalText,
                isiIndex: value.isiIndex
            )
            expectedByKey[key] = value
        }
        for row in rows {
            guard let expected = expectedByKey.removeValue(forKey: row.key) else {
                throw Error.unknownISI(trainID: row.trainID, isiIndex: row.isiIndex)
            }
            guard expected.leftTick.microseconds == row.leftTimestampMicroseconds,
                  expected.rightTick.microseconds == row.rightTimestampMicroseconds,
                  expected.intervalMicroseconds == row.intervalMicroseconds else {
                throw Error.geometryMismatch(trainID: row.trainID, isiIndex: row.isiIndex)
            }
        }
        guard expectedByKey.isEmpty else {
            throw Error.missingRealISI
        }
        // Re-project after construction so duplicate tracks, invalid labels, and Other coexistence
        // remain governed by the same Core contract as interactive workbench edits.
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: fingerprint,
            decisions: decisions
        )
        _ = try CanonicalManualISILabelProjector.rows(
            dataset: dataset,
            fingerprint: fingerprint,
            draft: draft
        )
        return draft
    }
}

public enum CanonicalManualISIDraftImportError: Error, Equatable, Sendable, LocalizedError {
    case noWorksheets
    case emptyWorksheet(sheet: Int)
    case invalidHeaders(sheet: Int)
    case malformedRow(sheet: Int, row: Int, column: String)
    case inconsistentIdentity(sheet: Int, row: Int)
    case duplicateISI(trainID: String, isiIndex: Int)
    case invalidLabel(track: String, value: String)
    case invalidReviewStatus
    case otherCoexistsWithBiologicalLabel
    case noRows
    case canonicalIdentityMismatch
    case confirmationRecordMismatch
    case realISIRowCountMismatch(expected: Int, actual: Int)
    case unknownISI(trainID: String, isiIndex: Int)
    case geometryMismatch(trainID: String, isiIndex: Int)
    case missingRealISI

    public var errorDescription: String? {
        switch self {
        case .noWorksheets: return "The manual-ISI workbook contains no worksheets."
        case .emptyWorksheet(let sheet): return "Worksheet \(sheet) contains no ISI rows."
        case .invalidHeaders(let sheet): return "Worksheet \(sheet) does not use the manual-ISI draft columns."
        case .malformedRow(let sheet, let row, let column):
            return "Worksheet \(sheet), row \(row) has an invalid \(column) value."
        case .inconsistentIdentity(let sheet, let row):
            return "Worksheet \(sheet), row \(row) belongs to a different manual-ISI draft."
        case .duplicateISI(let trainID, let isiIndex):
            return "The workbook repeats ISI \(isiIndex) in spike train \(trainID)."
        case .invalidLabel(let track, let value):
            return "\(value) is not a valid positive \(track) manual label."
        case .invalidReviewStatus: return "The review status does not match the row labels."
        case .otherCoexistsWithBiologicalLabel:
            return "An ISI cannot be Other and a biological State/Event at the same time."
        case .noRows: return "The manual-ISI workbook contains no rows."
        case .canonicalIdentityMismatch:
            return "This manual-ISI draft belongs to a different canonical dataset."
        case .confirmationRecordMismatch:
            return "This manual-ISI draft belongs to a different confirmed import record."
        case .realISIRowCountMismatch(let expected, let actual):
            return "The draft has \(actual) ISI rows, but the current dataset has \(expected)."
        case .unknownISI(let trainID, let isiIndex):
            return "The draft refers to unavailable ISI \(isiIndex) in spike train \(trainID)."
        case .geometryMismatch(let trainID, let isiIndex):
            return "The timestamps for ISI \(isiIndex) in spike train \(trainID) do not match the current data."
        case .missingRealISI: return "The draft omits one or more real ISIs from the current dataset."
        }
    }
}

private extension CanonicalManualISIDraftImport {
    typealias Error = CanonicalManualISIDraftImportError

    struct Identity: Hashable, Sendable {
        let canonicalSchemaContractID: String
        let canonicalSchemaContractDigest: String
        let canonicalDatasetDigest: String
        let confirmedImportRecordDigest: String
    }

    struct RowKey: Hashable, Sendable {
        let trainID: String
        let isiIndex: Int
    }

    struct Row: Hashable, Sendable {
        let identity: Identity
        let trainID: String
        let scientificTrainID: ScientificSpikeTrainID
        let isiIndex: Int
        let leftTimestampMicroseconds: Int64
        let rightTimestampMicroseconds: Int64
        let intervalMicroseconds: Int64
        let stateLabel: ManualAnnotationLabel?
        let eventLabel: ManualAnnotationLabel?
        let otherLabel: ManualAnnotationLabel?
        let isReviewed: Bool

        var key: RowKey { RowKey(trainID: trainID, isiIndex: isiIndex) }

        var decisions: [CanonicalManualISILabelDecision] {
            return [
                stateLabel.map { CanonicalManualISILabelDecision(trainID: scientificTrainID, isiIndex: isiIndex, track: .state, label: $0) },
                eventLabel.map { CanonicalManualISILabelDecision(trainID: scientificTrainID, isiIndex: isiIndex, track: .event, label: $0) },
                otherLabel.map { CanonicalManualISILabelDecision(trainID: scientificTrainID, isiIndex: isiIndex, track: .other, label: $0) },
            ].compactMap { $0 }
        }

        init(fields: [String], sheet: Int, row: Int) throws {
            guard fields.count == CanonicalManualISIDraftCSVExporter.headers.count else {
                throw Error.malformedRow(sheet: sheet, row: row, column: "row width")
            }
            func value(_ name: String) -> String {
                fields[CanonicalManualISIDraftCSVExporter.headers.firstIndex(of: name)!]
            }
            func exactInt(_ name: String) throws -> Int {
                let raw = value(name)
                guard let parsed = Int(raw), String(parsed) == raw else {
                    throw Error.malformedRow(sheet: sheet, row: row, column: name)
                }
                return parsed
            }
            func exactInt64(_ name: String) throws -> Int64 {
                let raw = value(name)
                guard let parsed = Int64(raw), String(parsed) == raw else {
                    throw Error.malformedRow(sheet: sheet, row: row, column: name)
                }
                return parsed
            }
            let trainID = value("train_id")
            let semanticTrainID: ScientificSemanticID
            do { semanticTrainID = try ScientificSemanticID(validating: trainID) }
            catch { throw Error.malformedRow(sheet: sheet, row: row, column: "train_id") }
            let state = try Self.label(value("state_pattern"), track: .state)
            let event = try Self.label(value("event_pattern"), track: .event)
            let other = try Self.label(value("other_pattern"), track: .other)
            guard !(other != nil && (state != nil || event != nil)) else {
                throw Error.otherCoexistsWithBiologicalLabel
            }
            let reviewed = state != nil || event != nil || other != nil
            let status = value("review_status")
            guard status == (reviewed ? "manually_labeled" : "unreviewed") else {
                throw Error.invalidReviewStatus
            }
            guard value("draft_schema_contract_id")
                    == CanonicalManualISIDraftCSVExporter.schemaContractID,
                  value("draft_schema_contract_digest")
                    == CanonicalManualISIDraftCSVExporter.schemaContractDigest,
                  value("authority_status")
                    == CanonicalManualISIDraftCSVExporter.authorityStatus else {
                throw Error.malformedRow(sheet: sheet, row: row, column: "draft identity")
            }
            self.identity = Identity(
                canonicalSchemaContractID: value("canonical_schema_contract_id"),
                canonicalSchemaContractDigest: value("canonical_schema_contract_digest"),
                canonicalDatasetDigest: value("canonical_dataset_digest"),
                confirmedImportRecordDigest: value("confirmed_import_record_digest")
            )
            self.trainID = trainID
            self.scientificTrainID = ScientificSpikeTrainID(semanticTrainID)
            self.isiIndex = try exactInt("isi_index")
            self.leftTimestampMicroseconds = try exactInt64("left_timestamp_us")
            self.rightTimestampMicroseconds = try exactInt64("right_timestamp_us")
            self.intervalMicroseconds = try exactInt64("isi_us")
            self.stateLabel = state
            self.eventLabel = event
            self.otherLabel = other
            self.isReviewed = reviewed
        }

        private static func label(
            _ rawValue: String,
            track: ManualAnnotationSemanticTrack
        ) throws -> ManualAnnotationLabel? {
            guard !rawValue.isEmpty else { return nil }
            guard let label = ManualAnnotationLabel(rawValue: rawValue),
                  label.polarity == .positive,
                  label.semanticTrack == track else {
                throw Error.invalidLabel(track: track.rawValue, value: rawValue)
            }
            return label
        }
    }
}
