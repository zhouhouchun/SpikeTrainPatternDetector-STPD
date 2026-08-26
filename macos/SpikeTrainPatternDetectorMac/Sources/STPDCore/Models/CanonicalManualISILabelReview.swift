import CryptoKit
import Foundation

public struct CanonicalManualISILabelDecision: Hashable, Sendable {
    public let trainID: ScientificSpikeTrainID
    public let isiIndex: Int
    public let track: ManualAnnotationSemanticTrack
    public let label: ManualAnnotationLabel

    public init(
        trainID: ScientificSpikeTrainID,
        isiIndex: Int,
        track: ManualAnnotationSemanticTrack,
        label: ManualAnnotationLabel
    ) {
        self.trainID = trainID
        self.isiIndex = isiIndex
        self.track = track
        self.label = label
    }
}

/// Mutable-by-replacement workbench value. It is identity-bound but carries no review or export
/// authority until `ConfirmedCanonicalManualISILabels.confirmComplete` validates and seals it.
public struct CanonicalManualISILabelDraft: Hashable, Sendable {
    public let canonicalFingerprint: CanonicalScientificDatasetFingerprint
    public let decisions: [CanonicalManualISILabelDecision]

    public init(
        canonicalFingerprint: CanonicalScientificDatasetFingerprint,
        decisions: [CanonicalManualISILabelDecision] = []
    ) {
        self.canonicalFingerprint = canonicalFingerprint
        self.decisions = Self.canonical(decisions)
    }

    public func applying(
        label: ManualAnnotationLabel,
        trainID: ScientificSpikeTrainID,
        isiIndices: Set<Int>
    ) -> CanonicalManualISILabelDraft {
        guard label.polarity == .positive else { return self }
        var values = decisions.filter { decision in
            guard decision.trainID == trainID,
                  isiIndices.contains(decision.isiIndex) else { return true }
            if label.semanticTrack == .other {
                return false
            }
            return decision.track != label.semanticTrack && decision.track != .other
        }
        for index in isiIndices {
            values.append(CanonicalManualISILabelDecision(
                trainID: trainID,
                isiIndex: index,
                track: label.semanticTrack,
                label: label
            ))
        }
        return CanonicalManualISILabelDraft(
            canonicalFingerprint: canonicalFingerprint,
            decisions: values
        )
    }

    public func clearing(
        track: ManualAnnotationSemanticTrack,
        trainID: ScientificSpikeTrainID,
        isiIndices: Set<Int>
    ) -> CanonicalManualISILabelDraft {
        CanonicalManualISILabelDraft(
            canonicalFingerprint: canonicalFingerprint,
            decisions: decisions.filter {
                !($0.trainID == trainID && $0.track == track
                    && isiIndices.contains($0.isiIndex))
            }
        )
    }

    private static func canonical(
        _ decisions: [CanonicalManualISILabelDecision]
    ) -> [CanonicalManualISILabelDecision] {
        var lastByKey: [String: CanonicalManualISILabelDecision] = [:]
        for decision in decisions {
            lastByKey[key(decision)] = decision
        }
        return lastByKey.values.sorted(by: ordered)
    }

    private static func key(_ value: CanonicalManualISILabelDecision) -> String {
        "\(value.trainID.semanticID.canonicalText)\u{1}\(value.isiIndex)\u{1}\(value.track.rawValue)"
    }

    fileprivate static func ordered(
        _ lhs: CanonicalManualISILabelDecision,
        _ rhs: CanonicalManualISILabelDecision
    ) -> Bool {
        let left = lhs.trainID.semanticID.canonicalText
        let right = rhs.trainID.semanticID.canonicalText
        if left != right { return left.utf8.lexicographicallyPrecedes(right.utf8) }
        if lhs.isiIndex != rhs.isiIndex { return lhs.isiIndex < rhs.isiIndex }
        return lhs.track.rawValue < rhs.track.rawValue
    }
}

public struct CanonicalManualISILabelRow: Identifiable, Hashable, Sendable {
    public let trainID: ScientificSpikeTrainID
    public let isiIndex: Int
    public let leftTick: MicrosecondTick
    public let rightTick: MicrosecondTick
    public let intervalMicroseconds: Int64
    public let stateLabel: ManualAnnotationLabel?
    public let eventLabel: ManualAnnotationLabel?
    public let otherLabel: ManualAnnotationLabel?

    public var id: String { "\(trainID.semanticID.canonicalText)\u{1}\(isiIndex)" }
    public var isReviewed: Bool { stateLabel != nil || eventLabel != nil || otherLabel != nil }
}

public enum CanonicalManualISILabelReviewError: Error, Equatable, Sendable, LocalizedError {
    case canonicalFingerprintMismatch
    case unknownTrain(ScientificSpikeTrainID)
    case invalidISIIndex(trainID: ScientificSpikeTrainID, isiIndex: Int)
    case invalidLabelTrack(label: ManualAnnotationLabel, track: ManualAnnotationSemanticTrack)
    case otherCoexistsWithBiologicalLabel(trainID: ScientificSpikeTrainID, isiIndex: Int)
    case incompleteReview(unreviewedISICount: Int)
    case emptyReviewer
    case invalidConfirmationTime
    case timestampArithmeticOverflow(trainID: ScientificSpikeTrainID, isiIndex: Int)

    public var errorDescription: String? {
        switch self {
        case .canonicalFingerprintMismatch:
            return "The manual review does not belong to this canonical dataset."
        case .unknownTrain(let trainID):
            return "The manual review refers to unknown spike train \(trainID.semanticID.canonicalText)."
        case .invalidISIIndex(let trainID, let isiIndex):
            return "ISI \(isiIndex) is outside spike train \(trainID.semanticID.canonicalText)."
        case .invalidLabelTrack(let label, let track):
            return "\(label.displayName) cannot be stored on the \(track.displayName) track."
        case .otherCoexistsWithBiologicalLabel(let trainID, let isiIndex):
            return "ISI \(isiIndex) in \(trainID.semanticID.canonicalText) cannot be both Other and a biological State/Event."
        case .incompleteReview(let count):
            return "The review is incomplete: \(count) real ISI row\(count == 1 ? " is" : "s are") still unreviewed."
        case .emptyReviewer:
            return "A nonempty reviewer identity is required to confirm the manual review."
        case .invalidConfirmationTime:
            return "The manual-review confirmation time is invalid."
        case .timestampArithmeticOverflow(let trainID, let isiIndex):
            return "Exact timestamp subtraction overflowed for ISI \(isiIndex) in \(trainID.semanticID.canonicalText)."
        }
    }
}

public enum CanonicalManualISILabelProjector {
    public static func rows(
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        draft: CanonicalManualISILabelDraft
    ) throws -> [CanonicalManualISILabelRow] {
        guard draft.canonicalFingerprint == fingerprint else {
            throw CanonicalManualISILabelReviewError.canonicalFingerprintMismatch
        }
        let trains = Dictionary(uniqueKeysWithValues: dataset.spikeTrains.map { ($0.semanticID, $0) })
        var decisions: [ScientificSpikeTrainID: [Int: [ManualAnnotationSemanticTrack: ManualAnnotationLabel]]] = [:]
        for decision in draft.decisions {
            guard let train = trains[decision.trainID] else {
                throw CanonicalManualISILabelReviewError.unknownTrain(decision.trainID)
            }
            guard decision.isiIndex > 0,
                  decision.isiIndex < train.rawTimestamps.count else {
                throw CanonicalManualISILabelReviewError.invalidISIIndex(
                    trainID: decision.trainID,
                    isiIndex: decision.isiIndex
                )
            }
            guard decision.label.polarity == .positive,
                  decision.label.semanticTrack == decision.track else {
                throw CanonicalManualISILabelReviewError.invalidLabelTrack(
                    label: decision.label,
                    track: decision.track
                )
            }
            decisions[decision.trainID, default: [:]][decision.isiIndex, default: [:]][decision.track] = decision.label
        }

        var rows: [CanonicalManualISILabelRow] = []
        for train in dataset.spikeTrains {
            guard train.rawTimestamps.count >= 2 else { continue }
            for index in 1..<train.rawTimestamps.count {
                let labels = decisions[train.semanticID]?[index] ?? [:]
                if labels[.other] != nil && (labels[.state] != nil || labels[.event] != nil) {
                    throw CanonicalManualISILabelReviewError.otherCoexistsWithBiologicalLabel(
                        trainID: train.semanticID,
                        isiIndex: index
                    )
                }
                let interval: Int64
                do {
                    interval = try train.rawTimestamps[index].interval(
                        since: train.rawTimestamps[index - 1]
                    )
                } catch {
                    throw CanonicalManualISILabelReviewError.timestampArithmeticOverflow(
                        trainID: train.semanticID,
                        isiIndex: index
                    )
                }
                rows.append(CanonicalManualISILabelRow(
                    trainID: train.semanticID,
                    isiIndex: index,
                    leftTick: train.rawTimestamps[index - 1],
                    rightTick: train.rawTimestamps[index],
                    intervalMicroseconds: interval,
                    stateLabel: labels[.state],
                    eventLabel: labels[.event],
                    otherLabel: labels[.other]
                ))
            }
        }
        return rows
    }
}

/// A complete, explicitly confirmed human classification of every real canonical ISI. This is a
/// review-evidence object, not by itself a persisted package or detector authority token.
public struct ConfirmedCanonicalManualISILabels: Hashable, Sendable {
    public let canonicalFingerprint: CanonicalScientificDatasetFingerprint
    public let decisions: [CanonicalManualISILabelDecision]
    public let reviewer: String
    public let confirmedAt: Date
    public let decisionDigest: String

    private init(
        canonicalFingerprint: CanonicalScientificDatasetFingerprint,
        decisions: [CanonicalManualISILabelDecision],
        reviewer: String,
        confirmedAt: Date,
        decisionDigest: String
    ) {
        self.canonicalFingerprint = canonicalFingerprint
        self.decisions = decisions
        self.reviewer = reviewer
        self.confirmedAt = confirmedAt
        self.decisionDigest = decisionDigest
    }

    public static func confirmComplete(
        draft: CanonicalManualISILabelDraft,
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        reviewer: String,
        confirmedAt: Date = Date()
    ) throws -> ConfirmedCanonicalManualISILabels {
        let normalizedReviewer = reviewer.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !normalizedReviewer.isEmpty else {
            throw CanonicalManualISILabelReviewError.emptyReviewer
        }
        guard confirmedAt.timeIntervalSince1970.isFinite else {
            throw CanonicalManualISILabelReviewError.invalidConfirmationTime
        }
        let rows = try CanonicalManualISILabelProjector.rows(
            dataset: dataset,
            fingerprint: fingerprint,
            draft: draft
        )
        let missing = rows.lazy.filter { !$0.isReviewed }.count
        guard missing == 0 else {
            throw CanonicalManualISILabelReviewError.incompleteReview(
                unreviewedISICount: missing
            )
        }
        let decisions = draft.decisions.sorted(by: CanonicalManualISILabelDraft.ordered)
        return ConfirmedCanonicalManualISILabels(
            canonicalFingerprint: fingerprint,
            decisions: decisions,
            reviewer: normalizedReviewer,
            confirmedAt: confirmedAt,
            decisionDigest: digest(
                fingerprint: fingerprint,
                decisions: decisions,
                reviewer: normalizedReviewer,
                confirmedAt: confirmedAt
            )
        )
    }

    private static func digest(
        fingerprint: CanonicalScientificDatasetFingerprint,
        decisions: [CanonicalManualISILabelDecision],
        reviewer: String,
        confirmedAt: Date
    ) -> String {
        var data = Data()
        append("canonical_manual_isi_complete_review", to: &data)
        append(fingerprint.schemaContractID, to: &data)
        append(fingerprint.schemaContractDigest, to: &data)
        append(fingerprint.datasetDigest, to: &data)
        append(reviewer, to: &data)
        append(String(format: "%.6f", confirmedAt.timeIntervalSince1970), to: &data)
        append(String(decisions.count), to: &data)
        for decision in decisions {
            append(decision.trainID.semanticID.canonicalText, to: &data)
            append(String(decision.isiIndex), to: &data)
            append(decision.track.rawValue, to: &data)
            append(decision.label.rawValue, to: &data)
        }
        return SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
    }

    private static func append(_ value: String, to data: inout Data) {
        let bytes = Data(value.utf8)
        var length = UInt64(bytes.count).bigEndian
        withUnsafeBytes(of: &length) { data.append(contentsOf: $0) }
        data.append(bytes)
    }
}
