import Foundation

/// Per-label (or per-family) summary of what the current manual annotations imply about a pattern's
/// ISI range and counts. PREVIEW / AUDIT ONLY: `appliedToDetector` is always false in this phase —
/// these values do not change candidate generation, arbitration, or any detector threshold.
///
/// Percentiles use the canonical `SortedFiniteSample` (linear interpolation) and `sampleCV` uses the
/// canonical `STPDStatistics` sample CV (n-1). No MM is used; no fixed-millisecond biological rule is
/// assumed — the usability gates are evidence-COUNT gates, not biological ms thresholds.
public struct ManualAnnotationCalibrationLabelSummary: Hashable, Sendable {
    public let label: String          // ManualAnnotationLabel.rawValue, or a family name e.g. "burst_family"
    public let displayName: String
    public let isPositive: Bool       // false for the not_burst veto row
    public let isFamily: Bool
    /// Contiguous resolved evidence runs that retain at least one uniquely owned, QC-valid ISI after
    /// version folding, overlap arbitration, and structural validation. Adjacent or overlapping marks
    /// for the same resolved label collapse to one run. This is not the number of serialized records.
    public let evidenceRunCount: Int
    /// Compatibility spelling retained for callers written when this value counted serialized marks.
    /// The value is the number of contiguous resolved evidence runs, not annotation records.
    public var annotationCount: Int { evidenceRunCount }
    public let trainCount: Int
    public let coveredISICount: Int
    public let minISISeconds: Double?
    public let q10ISISeconds: Double?
    public let q40ISISeconds: Double?
    public let medianISISeconds: Double?
    public let q90ISISeconds: Double?
    public let q95ISISeconds: Double?
    public let maxISISeconds: Double?
    public let meanISISeconds: Double?
    public let sampleCV: Double?
    public let isUsableForCalibration: Bool
    public let source: String
    public let appliedToDetector: Bool
    public let method: String
    public let recommendationText: String

    /// Whether more than one disjoint resolved evidence run contributes. This does not imply biological
    /// independence: two runs can come from the same train or recording episode.
    public var hasMultipleEvidenceRuns: Bool { evidenceRunCount >= 2 }

    /// Compatibility spelling retained for callers written before evidence was de-duplicated by run.
    public var hasMultipleEvidenceBearingAnnotations: Bool { hasMultipleEvidenceRuns }

    /// Compatibility spelling retained for callers that previously displayed "independent" replication.
    /// Independence cannot be established without biological-unit/session identity, so this is always false.
    /// Use `hasCrossTrainReplication` for the train-level fact that is actually observed.
    public var hasIndependentAnnotationReplication: Bool { false }

    /// Whether evidence for this label was observed in more than one train. Useful for audit/support scoring,
    /// but deliberately not required for producing a provisional, preview-only soft threshold.
    public var hasCrossTrainReplication: Bool { trainCount >= 2 }

    public init(
        label: String,
        displayName: String,
        isPositive: Bool,
        isFamily: Bool,
        annotationCount: Int,
        trainCount: Int,
        coveredISICount: Int,
        minISISeconds: Double?,
        q10ISISeconds: Double?,
        q40ISISeconds: Double?,
        medianISISeconds: Double?,
        q90ISISeconds: Double?,
        q95ISISeconds: Double?,
        maxISISeconds: Double?,
        meanISISeconds: Double?,
        sampleCV: Double?,
        isUsableForCalibration: Bool,
        source: String,
        appliedToDetector: Bool,
        method: String,
        recommendationText: String
    ) {
        self.label = label
        self.displayName = displayName
        self.isPositive = isPositive
        self.isFamily = isFamily
        self.evidenceRunCount = annotationCount
        self.trainCount = trainCount
        self.coveredISICount = coveredISICount
        self.minISISeconds = minISISeconds
        self.q10ISISeconds = q10ISISeconds
        self.q40ISISeconds = q40ISISeconds
        self.medianISISeconds = medianISISeconds
        self.q90ISISeconds = q90ISISeconds
        self.q95ISISeconds = q95ISISeconds
        self.maxISISeconds = maxISISeconds
        self.meanISISeconds = meanISISeconds
        self.sampleCV = sampleCV
        self.isUsableForCalibration = isUsableForCalibration
        self.source = source
        self.appliedToDetector = appliedToDetector
        self.method = method
        self.recommendationText = recommendationText
    }
}

/// The full manual-derived calibration summary across all labels/families present. Preview-only.
public struct ManualAnnotationCalibrationSummary: Hashable, Sendable {
    public let rows: [ManualAnnotationCalibrationLabelSummary]
    /// Serialized annotation records that were superseded, did not resolve to the current dataset,
    /// lost all final ISI ownership, failed a structural minimum, or retained no QC-valid ISI.
    public let skippedAnnotationCount: Int
    public let source: String
    public let appliedToDetector: Bool

    public init(
        rows: [ManualAnnotationCalibrationLabelSummary],
        skippedAnnotationCount: Int,
        source: String = ManualAnnotationCalibrationSummarizer.source,
        appliedToDetector: Bool = false
    ) {
        self.rows = rows
        self.skippedAnnotationCount = skippedAnnotationCount
        self.source = source
        self.appliedToDetector = appliedToDetector
    }

    public var isEmpty: Bool { rows.isEmpty }
    public var usableRows: [ManualAnnotationCalibrationLabelSummary] { rows.filter(\.isUsableForCalibration) }
    public func row(label: String) -> ManualAnnotationCalibrationLabelSummary? { rows.first { $0.label == label } }
}

/// Pure summarizer: turns the current manual annotations + dataset trains into a calibration preview.
/// No detector state is read or changed. Mirrors the R *idea* (manual-derived ISI percentiles and
/// counts as evidence) without porting MM or any fixed-ms biological rule.
public enum ManualAnnotationCalibrationSummarizer {
    public static let source = "manual_annotations"
    public static let familyBurst = "burst_family"

    public static func summarize(
        trains: [SpikeTrain],
        annotations: [ManualAnnotation],
        minValidISISeconds: Double = 0.001
    ) -> ManualAnnotationCalibrationSummary {
        let trainsByID = Dictionary(trains.map { ($0.id, $0) }, uniquingKeysWith: { first, _ in first })
        let minValid = minValidISISeconds.isFinite ? max(0, minValidISISeconds) : 0.001

        struct Accumulator {
            var values: [Double] = []
            var trainIDs: Set<String> = []
            var evidenceIndicesByTrain: [String: Set<Int>] = [:]
        }
        struct ResolvedEvidence {
            let annotation: ManualAnnotation
            let train: SpikeTrain
            let coveredIndices: [Int]
        }
        var byLabel: [ManualAnnotationLabel: Accumulator] = [:]
        let canonicalAnnotations = ManualAnnotationProjector.canonicalizedAnnotations(annotations)
        var skipped = annotations.count - canonicalAnnotations.count
        var resolvedEvidence: [ResolvedEvidence] = []
        resolvedEvidence.reserveCapacity(canonicalAnnotations.count)

        for annotation in canonicalAnnotations {
            guard let train = trainsByID[annotation.trainID] else {
                skipped += 1
                continue
            }
            let geometry = ManualAnnotationGeometryResolver.resolve(annotation: annotation, in: train)
            guard geometry.isWithinTrain, let covered = geometry.coveredISIIndices else {
                skipped += 1
                continue
            }
            let coveredIndices = covered.filter { train.isiSec.indices.contains($0) }
            guard !coveredIndices.isEmpty else {
                skipped += 1
                continue
            }
            resolvedEvidence.append(ResolvedEvidence(
                annotation: annotation,
                train: train,
                coveredIndices: coveredIndices
            ))
        }

        // Resolve ownership independently for positive labels and negative vetoes. Positive labels
        // compete with other positive labels old-to-new; veto annotations compete only with vetoes.
        // This mirrors projection semantics while keeping the two biological channels distinct.
        var positiveOwnerByTrain: [String: [Int: UUID]] = [:]
        var positiveLabelByTrain: [String: [Int: String]] = [:]
        var vetoOwnerByTrain: [String: [Int: UUID]] = [:]
        for evidence in resolvedEvidence {
            let annotation = evidence.annotation
            switch annotation.label.polarity {
            case .positive:
                guard let pattern = annotation.label.finalPatternString else { continue }
                for index in evidence.coveredIndices {
                    positiveOwnerByTrain[annotation.trainID, default: [:]][index] = annotation.id
                    positiveLabelByTrain[annotation.trainID, default: [:]][index] = pattern
                }
            case .negative:
                guard ManualAnnotationLabel.consumedVetoLabels.contains(annotation.label) else { continue }
                for index in evidence.coveredIndices {
                    vetoOwnerByTrain[annotation.trainID, default: [:]][index] = annotation.id
                }
            }
        }

        // Apply QC to the ownership maps before the burst minimum. Otherwise a raw two-ISI burst with
        // one sub-floor/non-finite interval would retain a single QC-valid ISI and incorrectly teach a
        // burst threshold. The value extraction below repeats the checks defensively.
        for trainID in Array(positiveLabelByTrain.keys) {
            guard let train = trainsByID[trainID] else { continue }
            for index in Array(positiveLabelByTrain[trainID]?.keys ?? Dictionary<Int, String>().keys) {
                guard train.isiSec.indices.contains(index),
                      let value = train.isiSec[index],
                      value.isFinite,
                      value >= minValid else {
                    positiveLabelByTrain[trainID]?.removeValue(forKey: index)
                    positiveOwnerByTrain[trainID]?.removeValue(forKey: index)
                    continue
                }
            }
        }
        for trainID in Array(vetoOwnerByTrain.keys) {
            guard let train = trainsByID[trainID] else { continue }
            for index in Array(vetoOwnerByTrain[trainID]?.keys ?? Dictionary<Int, UUID>().keys) {
                guard train.isiSec.indices.contains(index),
                      let value = train.isiSec[index],
                      value.isFinite,
                      value >= minValid else {
                    vetoOwnerByTrain[trainID]?.removeValue(forKey: index)
                    continue
                }
            }
        }

        // Re-apply the burst minimum after both overlap ownership and QC. Removing an edge from a
        // two-ISI mark must not leave a singleton burst contributing calibration evidence or support.
        for trainID in Array(positiveLabelByTrain.keys) {
            let invalid = ManualAnnotationProjector.invalidBurstFragmentISIs(
                in: positiveLabelByTrain[trainID] ?? [:]
            )
            for index in invalid {
                positiveLabelByTrain[trainID]?.removeValue(forKey: index)
                positiveOwnerByTrain[trainID]?.removeValue(forKey: index)
            }
        }

        for evidence in resolvedEvidence {
            let annotation = evidence.annotation
            let ownerByISI: [Int: UUID]
            switch annotation.label.polarity {
            case .positive:
                ownerByISI = positiveOwnerByTrain[annotation.trainID] ?? [:]
            case .negative:
                ownerByISI = vetoOwnerByTrain[annotation.trainID] ?? [:]
            }

            let validEvidence = evidence.coveredIndices.compactMap { index -> (Int, Double)? in
                guard ownerByISI[index] == annotation.id,
                      let value = evidence.train.isiSec[index],
                      value.isFinite,
                      value >= minValid else {
                    return nil
                }
                return (index, value)
            }

            // Only uniquely-owned QC-valid evidence can contribute to a resolved run. Superseded,
            // fully-overwritten, structurally-invalid, and QC-empty records cannot inflate support.
            guard !validEvidence.isEmpty else {
                skipped += 1
                continue
            }

            var accumulator = byLabel[annotation.label] ?? Accumulator()
            accumulator.trainIDs.insert(annotation.trainID)
            accumulator.values.append(contentsOf: validEvidence.map { $0.1 })
            accumulator.evidenceIndicesByTrain[annotation.trainID, default: []]
                .formUnion(validEvidence.map { $0.0 })
            byLabel[annotation.label] = accumulator
        }

        var rows: [ManualAnnotationCalibrationLabelSummary] = []

        // Burst family (including the independently reviewable HFB event) first, if present.
        let burstFamilyLabels: [ManualAnnotationLabel] = [
            .burst, .highFrequencyBurst, .longBurst
        ]
        if burstFamilyLabels.contains(where: { byLabel[$0] != nil }) {
            var combined = Accumulator()
            for label in burstFamilyLabels {
                if let accumulator = byLabel[label] {
                    combined.values.append(contentsOf: accumulator.values)
                    combined.trainIDs.formUnion(accumulator.trainIDs)
                    for (trainID, indices) in accumulator.evidenceIndicesByTrain {
                        combined.evidenceIndicesByTrain[trainID, default: []].formUnion(indices)
                    }
                }
            }
            rows.append(makePositiveRow(
                label: familyBurst,
                displayName: "Burst family",
                isFamily: true,
                values: combined.values,
                annotationCount: evidenceRunCount(combined.evidenceIndicesByTrain),
                trainCount: combined.trainIDs.count,
                minUsableCount: 3,
                recommendationText: "Preview: seed/profile upper ≈ q90, bridge/profile upper ≈ q95, dense-core ref ≈ q40 (ms). Cross-train evidence is reported separately; the support score is capped by contributing-train coverage, not raw ISI or within-train run count, and does not claim biological independence. Not applied to the detector."
            ))
        }

        // Per positive label, in a stable order.
        for label in positiveLabelOrder {
            guard let accumulator = byLabel[label] else { continue }
            rows.append(makePositiveRow(
                label: label.rawValue,
                displayName: label.displayName,
                isFamily: false,
                values: accumulator.values,
                annotationCount: evidenceRunCount(accumulator.evidenceIndicesByTrain),
                trainCount: accumulator.trainIDs.count,
                minUsableCount: minUsableCount(for: label),
                recommendationText: recommendation(for: label)
            ))
        }

        // Negative / veto coverage (not_burst): counts only, never a positive calibration range.
        if let vetoAccumulator = byLabel[.notBurst] {
            rows.append(ManualAnnotationCalibrationLabelSummary(
                label: ManualAnnotationLabel.notBurst.rawValue,
                displayName: ManualAnnotationLabel.notBurst.displayName,
                isPositive: false,
                isFamily: false,
                annotationCount: evidenceRunCount(vetoAccumulator.evidenceIndicesByTrain),
                trainCount: vetoAccumulator.trainIDs.count,
                coveredISICount: vetoAccumulator.values.count,
                minISISeconds: nil,
                q10ISISeconds: nil,
                q40ISISeconds: nil,
                medianISISeconds: nil,
                q90ISISeconds: nil,
                q95ISISeconds: nil,
                maxISISeconds: nil,
                meanISISeconds: nil,
                sampleCV: nil,
                isUsableForCalibration: false,
                source: source,
                appliedToDetector: false,
                method: "negative_veto_coverage",
                recommendationText: "Negative/veto coverage; excluded from positive calibration ranges. Not applied to the detector."
            ))
        }

        return ManualAnnotationCalibrationSummary(rows: rows, skippedAnnotationCount: skipped)
    }

    // MARK: - Helpers

    private static let positiveLabelOrder: [ManualAnnotationLabel] = [
        .burst, .highFrequencyBurst, .longBurst, .tonic,
        .highFrequencyTonic, .highFrequencySpiking, .pause, .other
    ]

    private static func minUsableCount(for label: ManualAnnotationLabel) -> Int {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst: return 3
        case .highFrequencyTonic: return 4
        case .highFrequencySpiking: return 8
        case .tonic: return 4
        case .pause: return 2
        case .other, .notBurst: return Int.max   // summarize but never usable for calibration
        }
    }

    private static func recommendation(for label: ManualAnnotationLabel) -> String {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst:
            return "Preview: seed/profile upper ≈ q90, bridge/profile upper ≈ q95, dense-core ref ≈ q40 (ms). Not applied to the detector."
        case .highFrequencyTonic:
            return "Preview: lower ≈ q10, upper ≈ q90 (ms). Not applied to the detector."
        case .tonic:
            return "Preview: lower ≈ q10, upper ≈ q90 (ms). Not applied to the detector."
        case .highFrequencySpiking:
            return "Preview: max-ISI ≈ q90 (ms). Not applied to the detector."
        case .pause:
            return "Preview: summary only (median / q10 / q90 ms). Not applied to the detector."
        case .other:
            return "Summary only; not used for calibration. Not applied to the detector."
        case .notBurst:
            return "Negative/veto coverage; excluded from positive calibration ranges. Not applied to the detector."
        }
    }

    private static func evidenceRunCount(_ indicesByTrain: [String: Set<Int>]) -> Int {
        indicesByTrain.values.reduce(into: 0) { total, indices in
            var prior: Int?
            for index in indices.sorted() {
                if prior.map({ index != $0 + 1 }) ?? true {
                    total += 1
                }
                prior = index
            }
        }
    }

    private static func makePositiveRow(
        label: String,
        displayName: String,
        isFamily: Bool,
        values: [Double],
        annotationCount: Int,
        trainCount: Int,
        minUsableCount: Int,
        recommendationText: String
    ) -> ManualAnnotationCalibrationLabelSummary {
        let sample = SortedFiniteSample(values)
        let sorted = sample.values
        let coveredISICount = sorted.count
        return ManualAnnotationCalibrationLabelSummary(
            label: label,
            displayName: displayName,
            isPositive: true,
            isFamily: isFamily,
            annotationCount: annotationCount,
            trainCount: trainCount,
            coveredISICount: coveredISICount,
            minISISeconds: sorted.first,
            q10ISISeconds: sample.quantile(0.10),
            q40ISISeconds: sample.quantile(0.40),
            medianISISeconds: sample.quantile(0.50),
            q90ISISeconds: sample.quantile(0.90),
            q95ISISeconds: sample.quantile(0.95),
            maxISISeconds: sorted.last,
            meanISISeconds: STPDStatistics.mean(values),
            sampleCV: STPDStatistics.coefficientOfVariation(values),
            isUsableForCalibration: coveredISICount >= minUsableCount,
            source: source,
            appliedToDetector: false,
            method: isFamily ? "manual_isi_percentiles_family" : "manual_isi_percentiles",
            recommendationText: recommendationText
        )
    }
}
