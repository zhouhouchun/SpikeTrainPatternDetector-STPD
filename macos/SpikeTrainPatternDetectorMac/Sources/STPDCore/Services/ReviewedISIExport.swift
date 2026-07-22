import Foundation

/// One row of the reviewed-final per-ISI / per-timestamp export: a single spike timestamp carrying both
/// the raw automatic per-ISI label (`auto*`) and the reviewed/public per-ISI label (`final*`).
///
/// Indexing contract: one row per spike timestamp. The first spike (`spikeIndex == 0`) has
/// `isiIndex == 0`, `isiSec == 0` and empty patterns. For `spikeIndex == k > 0`, `isiIndex == k` in the
/// app's ISI convention — ISI `k` is the interval `timestampsSec[k-1] → timestampsSec[k]`, the row
/// timestamp is `timestampsSec[k]`, and `isiSec == timestampsSec[k] - timestampsSec[k-1]`.
public struct ReviewedISIExportRow: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let spikeIndex: Int
    public let timestampSec: Double
    public let alignedTimestampSec: Double
    public let isiIndex: Int
    public let isiSec: Double
    /// Raw auto label for this ISI, before manual projection (`""` when no auto label covers it).
    public let autoPattern: String
    public let autoSubtype: String
    public let autoCandidateID: String
    /// Reviewed/public label for this ISI (use this for downstream analysis), `""` when none.
    public let finalPattern: String
    public let finalSubtype: String
    /// `auto_projected` / `manual_positive` / `manual_veto_removed` / `manual_review_rejected` /
    /// `invalid_single_isi_burst_fragment` / `none`.
    public let finalSource: String
    public let manualVetoSuppressed: Bool
    public let reviewNote: String

    public init(
        trainID: String, trainName: String, spikeIndex: Int, timestampSec: Double,
        alignedTimestampSec: Double, isiIndex: Int, isiSec: Double, autoPattern: String,
        autoSubtype: String, autoCandidateID: String, finalPattern: String, finalSubtype: String,
        finalSource: String, manualVetoSuppressed: Bool, reviewNote: String
    ) {
        self.trainID = trainID; self.trainName = trainName; self.spikeIndex = spikeIndex
        self.timestampSec = timestampSec; self.alignedTimestampSec = alignedTimestampSec
        self.isiIndex = isiIndex; self.isiSec = isiSec; self.autoPattern = autoPattern
        self.autoSubtype = autoSubtype; self.autoCandidateID = autoCandidateID
        self.finalPattern = finalPattern; self.finalSubtype = finalSubtype; self.finalSource = finalSource
        self.manualVetoSuppressed = manualVetoSuppressed; self.reviewNote = reviewNote
    }
}

/// Pure builder for the reviewed-final per-ISI export. It NEVER mutates detection: it reads the selected
/// auto annotations + the per-train manual projection and emits a per-timestamp table. The
/// reviewed/final burst-validity rule (a burst needs ≥ 2 ISIs / 3 spikes) is applied to `final` only;
/// `auto` is left untouched for traceability.
public enum ReviewedISIExportBuilder {
    /// A reviewed/final burst-family run must span at least this many ISIs (R / biological minimum).
    public static let burstMinimumISIRunLength = ManualAnnotationProjector.burstMinimumISIRunLength

    public static let sourceAutoProjected = "auto_projected"
    public static let sourceManualPositive = "manual_positive"
    public static let sourceManualVetoRemoved = "manual_veto_removed"
    public static let sourceManualReviewRejected = "manual_review_rejected"
    public static let sourceInvalidBurstFragment = "invalid_single_isi_burst_fragment"
    public static let sourceNone = "none"

    /// - Parameters:
    ///   - autoAnnotations: the RAW selected auto annotations (event + gap + state), **before** manual
    ///     review rejection — so `auto_pattern` preserves the original detector result even for a
    ///     rejected candidate. Exclusive (one candidate per ISI).
    ///   - projectionsByTrain: per-train `ManualAnnotationProjection` (positive labels + burst veto set).
    ///   - reviewRejectedCandidateIDs: candidate ids the reviewer rejected — they keep `auto_pattern`
    ///     but lose `final_pattern` (`final_source = manual_review_rejected`), unless a positive manual
    ///     label overrides the ISI.
    public static func build(
        dataset: SpikeDataset,
        autoAnnotations: [ClassicAnchorEventAnnotation],
        projectionsByTrain: [String: ManualAnnotationProjection],
        reviewRejectedCandidateIDs: Set<String> = [],
        burstFamilyLabels: Set<String> = ManualAnnotationProjector.burstFamilyLabels
    ) -> [ReviewedISIExportRow] {
        let annotationsByTrain = Dictionary(grouping: autoAnnotations, by: \.trainID)
        var rows: [ReviewedISIExportRow] = []

        for train in dataset.trains {
            let n = train.spikeCount
            guard n >= 1, train.timestampsSec.count == n else { continue }
            let aligned = train.alignedTimestampsSec.count == n ? train.alignedTimestampsSec : train.timestampsSec

            // Per-ISI raw auto label (1-based ISI index == spike array index).
            var autoLabel: [Int: String] = [:]
            var autoSubtype: [Int: String] = [:]
            var autoCandidate: [Int: String] = [:]
            for annotation in annotationsByTrain[train.id] ?? [] {
                guard let span = annotation.coveredISIIndices(in: train) else { continue }
                for isi in span {
                    autoLabel[isi] = annotation.label.rawValue
                    autoSubtype[isi] = annotation.auditRecommendedSubtype
                    autoCandidate[isi] = annotation.candidateID
                }
            }

            let projection = projectionsByTrain[train.id]
            let positiveByISI = projection?.manualPositiveLabelByISI ?? [:]
            let vetoedBurstISIs = projection?.autoBurstBlockedByVetoISIs ?? []

            // Per-ISI reviewed/final label (before the burst-minimum rule).
            var finalLabel: [Int: String] = [:]
            var finalSubtype: [Int: String] = [:]
            var finalSource: [Int: String] = [:]
            var reviewNote: [Int: String] = [:]
            if n >= 2 {
                for isi in 1..<n {
                    if let positive = positiveByISI[isi] {
                        // A positive manual label always appears, overriding any auto label — even when
                        // the underlying auto candidate was review-rejected.
                        finalLabel[isi] = positive
                        finalSubtype[isi] = ""
                        finalSource[isi] = sourceManualPositive
                    } else if let candidate = autoCandidate[isi], reviewRejectedCandidateIDs.contains(candidate) {
                        // The reviewer rejected this auto candidate: drop it from final, keep auto.
                        finalLabel[isi] = ""
                        finalSubtype[isi] = ""
                        finalSource[isi] = sourceManualReviewRejected
                    } else if let auto = autoLabel[isi], burstFamilyLabels.contains(auto), vetoedBurstISIs.contains(isi) {
                        finalLabel[isi] = ""
                        finalSubtype[isi] = ""
                        finalSource[isi] = sourceManualVetoRemoved
                    } else if let auto = autoLabel[isi] {
                        finalLabel[isi] = auto
                        finalSubtype[isi] = autoSubtype[isi] ?? ""
                        finalSource[isi] = sourceAutoProjected
                    } else {
                        finalLabel[isi] = ""
                        finalSubtype[isi] = ""
                        finalSource[isi] = sourceNone
                    }
                }
            }

            // Reviewed/final burst-minimum rule: drop any auto-projected or manual-positive
            // burst-family run shorter than 2 ISIs. `auto*` remains untouched for traceability.
            applyBurstMinimumRun(
                finalLabel: &finalLabel,
                finalSubtype: &finalSubtype,
                finalSource: &finalSource,
                reviewNote: &reviewNote,
                burstFamilyLabels: burstFamilyLabels
            )

            for spikeIndex in 0..<n {
                if spikeIndex == 0 {
                    rows.append(ReviewedISIExportRow(
                        trainID: train.id, trainName: train.name, spikeIndex: 0,
                        timestampSec: train.timestampsSec[0], alignedTimestampSec: aligned[0],
                        isiIndex: 0, isiSec: 0, autoPattern: "", autoSubtype: "", autoCandidateID: "",
                        finalPattern: "", finalSubtype: "", finalSource: sourceNone,
                        manualVetoSuppressed: false, reviewNote: ""
                    ))
                } else {
                    let k = spikeIndex
                    rows.append(ReviewedISIExportRow(
                        trainID: train.id, trainName: train.name, spikeIndex: k,
                        timestampSec: train.timestampsSec[k], alignedTimestampSec: aligned[k],
                        isiIndex: k, isiSec: train.timestampsSec[k] - train.timestampsSec[k - 1],
                        autoPattern: autoLabel[k] ?? "", autoSubtype: autoSubtype[k] ?? "",
                        autoCandidateID: autoCandidate[k] ?? "",
                        finalPattern: finalLabel[k] ?? "", finalSubtype: finalSubtype[k] ?? "",
                        finalSource: finalSource[k] ?? sourceNone,
                        manualVetoSuppressed: finalSource[k] == sourceManualVetoRemoved,
                        reviewNote: reviewNote[k] ?? ""
                    ))
                }
            }
        }
        return rows
    }

    /// Drop reviewed/final burst-family runs shorter than `burstMinimumISIRunLength`. Structural
    /// validity follows contiguous final burst-family coverage, independent of whether adjacent ISIs
    /// came from an auto candidate or a manual positive override.
    private static func applyBurstMinimumRun(
        finalLabel: inout [Int: String],
        finalSubtype: inout [Int: String],
        finalSource: inout [Int: String],
        reviewNote: inout [Int: String],
        burstFamilyLabels: Set<String>
    ) {
        let invalid = ManualAnnotationProjector.invalidBurstFragmentISIs(
            in: finalLabel,
            minimumRunLength: burstMinimumISIRunLength,
            burstLabels: burstFamilyLabels
        )
        for isi in invalid {
            finalLabel[isi] = ""
            finalSubtype[isi] = ""
            finalSource[isi] = sourceInvalidBurstFragment
            reviewNote[isi] = "burst_run_below_min_isi"
        }
    }
}
