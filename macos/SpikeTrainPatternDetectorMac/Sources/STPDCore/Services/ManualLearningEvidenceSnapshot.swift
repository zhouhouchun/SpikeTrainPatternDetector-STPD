import CryptoKit
import Foundation

/// A family-specific negative observation. Negative evidence remains separate from the positive
/// State/Event/Other tracks so that a veto cannot silently become a biological label.
public enum ManualLearningNegativeTarget: String, CaseIterable, Hashable, Sendable {
    case burstFamily = "burst_family"
}

/// One row-level family veto retained for audit and conflict detection. It is not a negative
/// candidate/segment statistical unit and must not be counted as one by a future learner.
public struct ManualLearningNegativeDecision: Hashable, Sendable {
    public let trainID: ScientificSpikeTrainID
    public let isiIndex: Int
    public let target: ManualLearningNegativeTarget

    public init(
        trainID: ScientificSpikeTrainID,
        isiIndex: Int,
        target: ManualLearningNegativeTarget
    ) {
        self.trainID = trainID
        self.isiIndex = isiIndex
        self.target = target
    }
}

/// QC standing for one exact canonical ISI. Invalid intervals remain in the snapshot and therefore
/// retain geometry; downstream metrics decide whether they can provide numerical support.
public enum ManualLearningISIQuality: String, CaseIterable, Hashable, Sendable {
    case valid
    case nonPositive = "non_positive"
    case belowMinimum = "below_minimum"
}

public enum ManualLearningEvidenceConflict: String, CaseIterable, Hashable, Sendable {
    case positiveAndNegativeBurstFamily = "positive_and_negative_burst_family"

    public var target: ManualLearningNegativeTarget {
        switch self {
        case .positiveAndNegativeBurstFamily: return .burstFamily
        }
    }

    public func isRelevant(
        to track: ManualAnnotationSemanticTrack,
        label: ManualAnnotationLabel
    ) -> Bool {
        switch target {
        case .burstFamily:
            return track == .event
                && ManualAnnotationProjector.burstFamilyLabels.contains(label.rawValue)
        }
    }
}

public struct ManualLearningEvidenceRow: Identifiable, Hashable, Sendable {
    public let trainID: ScientificSpikeTrainID
    public let isiIndex: Int
    public let leftTick: MicrosecondTick
    public let rightTick: MicrosecondTick
    public let intervalMicroseconds: Int64
    public let quality: ManualLearningISIQuality
    public let stateLabel: ManualAnnotationLabel?
    public let eventLabel: ManualAnnotationLabel?
    public let otherLabel: ManualAnnotationLabel?
    public let negativeTargets: [ManualLearningNegativeTarget]
    public let conflicts: [ManualLearningEvidenceConflict]

    /// Snapshot-scoped row identity. Cross-dataset caches must pair it with the snapshot digest.
    public var id: String { "\(trainID.semanticID.canonicalText)\u{1}\(isiIndex)" }

    /// Blank is an explicit unknown learning state. It is not converted to Other.
    public var isUnknown: Bool {
        stateLabel == nil && eventLabel == nil && otherLabel == nil && negativeTargets.isEmpty
    }
}

/// Stable, partial, dual-track evidence bound to one canonical dataset identity. This is a learning
/// input and audit artifact only; it grants no detector, result-package, or export authority.
public struct ManualLearningEvidenceSnapshot: Hashable, Sendable {
    public let schemaContractID: String
    public let schemaContractDigest: String
    public let canonicalFingerprint: CanonicalScientificDatasetFingerprint
    public let minimumValidISIMicroseconds: Int64
    public let rows: [ManualLearningEvidenceRow]
    public let digest: String
}

public enum ManualLearningEvidenceSnapshotError: Error, Equatable, Sendable, LocalizedError {
    case invalidMinimumValidISI(Int64)
    case suppliedFingerprintDoesNotMatchDataset
    case unknownNegativeTrain(ScientificSpikeTrainID)
    case invalidNegativeISIIndex(trainID: ScientificSpikeTrainID, isiIndex: Int)

    public var errorDescription: String? {
        switch self {
        case .invalidMinimumValidISI(let value):
            return "The minimum valid ISI must be nonnegative; received \(value) microseconds."
        case .suppliedFingerprintDoesNotMatchDataset:
            return "The supplied fingerprint does not match the canonical dataset."
        case .unknownNegativeTrain(let trainID):
            return "Negative evidence refers to unknown spike train \(trainID.semanticID.canonicalText)."
        case .invalidNegativeISIIndex(let trainID, let isiIndex):
            return "Negative evidence refers to invalid ISI \(isiIndex) in \(trainID.semanticID.canonicalText)."
        }
    }
}

public enum ManualLearningEvidenceSnapshotBuilder {
    public static let schemaContractID =
        "canonical_partial_dual_track_manual_isi_learning_evidence"

    public static func build(
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        draft: CanonicalManualISILabelDraft,
        minimumValidISIMicroseconds: Int64,
        negativeDecisions: [ManualLearningNegativeDecision] = []
    ) throws -> ManualLearningEvidenceSnapshot {
        guard minimumValidISIMicroseconds >= 0 else {
            throw ManualLearningEvidenceSnapshotError.invalidMinimumValidISI(
                minimumValidISIMicroseconds
            )
        }
        let recomputed = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)
        guard recomputed == fingerprint else {
            throw ManualLearningEvidenceSnapshotError.suppliedFingerprintDoesNotMatchDataset
        }

        let projected = try CanonicalManualISILabelProjector.rows(
            dataset: dataset,
            fingerprint: fingerprint,
            draft: draft
        )
        let trainSizes = Dictionary(uniqueKeysWithValues: dataset.spikeTrains.map {
            ($0.semanticID, $0.rawTimestamps.count)
        })
        let negatives = try canonicalNegatives(negativeDecisions, trainSizes: trainSizes)
        let negativesByRow = Dictionary(grouping: negatives) {
            RowKey(trainID: $0.trainID, isiIndex: $0.isiIndex)
        }

        let rows = projected.map { source -> ManualLearningEvidenceRow in
            let targets = (negativesByRow[
                RowKey(trainID: source.trainID, isiIndex: source.isiIndex)
            ] ?? []).map(\.target)
            var conflicts: [ManualLearningEvidenceConflict] = []
            if targets.contains(.burstFamily),
               source.eventLabel.map(isBurstFamily) == true {
                conflicts.append(.positiveAndNegativeBurstFamily)
            }
            return ManualLearningEvidenceRow(
                trainID: source.trainID,
                isiIndex: source.isiIndex,
                leftTick: source.leftTick,
                rightTick: source.rightTick,
                intervalMicroseconds: source.intervalMicroseconds,
                quality: quality(
                    source.intervalMicroseconds,
                    minimumValid: minimumValidISIMicroseconds
                ),
                stateLabel: source.stateLabel,
                eventLabel: source.eventLabel,
                otherLabel: source.otherLabel,
                negativeTargets: targets,
                conflicts: conflicts
            )
        }

        let contractDigest = schemaContractDigest()
        let digest = evidenceDigest(
            fingerprint: fingerprint,
            contractDigest: contractDigest,
            minimumValidISIMicroseconds: minimumValidISIMicroseconds,
            rows: rows
        )
        return ManualLearningEvidenceSnapshot(
            schemaContractID: schemaContractID,
            schemaContractDigest: contractDigest,
            canonicalFingerprint: fingerprint,
            minimumValidISIMicroseconds: minimumValidISIMicroseconds,
            rows: rows,
            digest: digest
        )
    }

    private struct RowKey: Hashable {
        let trainID: ScientificSpikeTrainID
        let isiIndex: Int
    }

    private static func canonicalNegatives(
        _ values: [ManualLearningNegativeDecision],
        trainSizes: [ScientificSpikeTrainID: Int]
    ) throws -> [ManualLearningNegativeDecision] {
        var unique: [String: ManualLearningNegativeDecision] = [:]
        for value in values {
            guard let timestampCount = trainSizes[value.trainID] else {
                throw ManualLearningEvidenceSnapshotError.unknownNegativeTrain(value.trainID)
            }
            guard value.isiIndex > 0, value.isiIndex < timestampCount else {
                throw ManualLearningEvidenceSnapshotError.invalidNegativeISIIndex(
                    trainID: value.trainID,
                    isiIndex: value.isiIndex
                )
            }
            unique[negativeKey(value)] = value
        }
        return unique.values.sorted {
            let left = $0.trainID.semanticID.canonicalText
            let right = $1.trainID.semanticID.canonicalText
            if left != right { return left.utf8.lexicographicallyPrecedes(right.utf8) }
            if $0.isiIndex != $1.isiIndex { return $0.isiIndex < $1.isiIndex }
            return $0.target.rawValue < $1.target.rawValue
        }
    }

    private static func negativeKey(_ value: ManualLearningNegativeDecision) -> String {
        "\(value.trainID.semanticID.canonicalText)\u{1}\(value.isiIndex)\u{1}\(value.target.rawValue)"
    }

    private static func quality(
        _ interval: Int64,
        minimumValid: Int64
    ) -> ManualLearningISIQuality {
        if interval <= 0 { return .nonPositive }
        if interval < minimumValid { return .belowMinimum }
        return .valid
    }

    private static func isBurstFamily(_ label: ManualAnnotationLabel) -> Bool {
        ManualAnnotationProjector.burstFamilyLabels.contains(label.rawValue)
    }

    private static func schemaContractDigest() -> String {
        var encoder = ManualLearningDigestEncoder()
        [
            schemaContractID,
            "sha256",
            "utf8_token",
            "uint64_big_endian_token_length",
            "dataset_fingerprint_schema_and_digest",
            "minimum_valid_isi_microseconds",
            "canonical_train_then_isi_order",
            "exact_signed_microsecond_geometry",
            "independent_state_event_other_tracks",
            "unknown_is_not_other",
            "family_specific_negative_row_evidence_not_negative_statistical_units",
            "positive_negative_conflicts_are_retained_and_target_specific",
        ].forEach { encoder.append($0) }
        ManualLearningISIQuality.allCases.forEach {
            encoder.append("quality:\($0.rawValue)")
        }
        ManualAnnotationSemanticTrack.allCases.forEach {
            encoder.append("track:\($0.rawValue)")
        }
        ManualAnnotationLabel.allCases.forEach {
            encoder.append("label:\($0.rawValue):\($0.semanticTrack.rawValue):\($0.polarity.rawValue)")
        }
        ManualLearningNegativeTarget.allCases.forEach {
            encoder.append("negative_target:\($0.rawValue)")
        }
        ManualLearningEvidenceConflict.allCases.forEach {
            encoder.append("conflict:\($0.rawValue):\($0.target.rawValue)")
        }
        ManualLearningEvidenceSchema.semanticRuleTokens.forEach {
            encoder.append($0)
        }
        return encoder.finalizeHex()
    }

    private static func evidenceDigest(
        fingerprint: CanonicalScientificDatasetFingerprint,
        contractDigest: String,
        minimumValidISIMicroseconds: Int64,
        rows: [ManualLearningEvidenceRow]
    ) -> String {
        var encoder = ManualLearningDigestEncoder()
        [
            schemaContractID,
            contractDigest,
            fingerprint.schemaContractID,
            fingerprint.schemaContractDigest,
            fingerprint.datasetDigest,
            String(minimumValidISIMicroseconds),
            String(rows.count),
        ].forEach { encoder.append($0) }
        for row in rows {
            [
                row.trainID.semanticID.canonicalText,
                String(row.isiIndex),
                String(row.leftTick.microseconds),
                String(row.rightTick.microseconds),
                String(row.intervalMicroseconds),
                row.quality.rawValue,
                row.stateLabel?.rawValue ?? "",
                row.eventLabel?.rawValue ?? "",
                row.otherLabel?.rawValue ?? "",
                row.negativeTargets.map(\.rawValue).joined(separator: "\u{1}"),
                row.conflicts.map(\.rawValue).joined(separator: "\u{1}"),
            ].forEach { encoder.append($0) }
        }
        return encoder.finalizeHex()
    }
}

/// Shared semantic-rule transcript used by every schema whose meaning depends on manual-learning
/// QC or conflict relevance. Keeping these exact tokens in one place prevents builder/extractor drift.
enum ManualLearningEvidenceSchema {
    static var semanticRuleTokens: [String] {
        var tokens = [
            "quality_rule:interval_microseconds<=0=>non_positive",
            "quality_rule:0<interval_microseconds<minimum_valid_isi_microseconds=>below_minimum",
            "quality_rule:interval_microseconds>=minimum_valid_isi_microseconds=>valid",
            "conflict_trigger:negative_target=burst_family+positive_event_label_in_burst_family=>positive_and_negative_burst_family",
            "conflict_relevance:positive_and_negative_burst_family:track=event",
        ]
        for label in ManualAnnotationProjector.burstFamilyLabels.sorted() {
            tokens.append("negative_target_membership:burst_family:event_label=\(label)")
            tokens.append(
                "conflict_relevance:positive_and_negative_burst_family:event_label=\(label)"
            )
        }
        return tokens
    }
}

enum ManualLearningDigest {
    static func hex(tokens: [String]) -> String {
        var encoder = ManualLearningDigestEncoder()
        tokens.forEach { encoder.append($0) }
        return encoder.finalizeHex()
    }
}

struct ManualLearningDigestEncoder {
    private var hasher = SHA256()

    mutating func append(_ token: String) {
        let bytes = Array(token.utf8)
        var length = UInt64(bytes.count).bigEndian
        withUnsafeBytes(of: &length) { hasher.update(bufferPointer: $0) }
        hasher.update(data: Data(bytes))
    }

    mutating func finalizeHex() -> String {
        hasher.finalize().map { String(format: "%02x", $0) }.joined()
    }
}
