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
    public let annotationCount: Int
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
        self.annotationCount = annotationCount
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
    /// Annotations that did not resolve to the current dataset (wrong train / out of range).
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
        let minValid = minValidISISeconds.isFinite ? minValidISISeconds : 0.001

        struct Accumulator {
            var values: [Double] = []
            var annotationCount = 0
            var trainIDs: Set<String> = []
        }
        var byLabel: [ManualAnnotationLabel: Accumulator] = [:]
        var skipped = 0

        for annotation in annotations {
            guard let resolved = ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(annotation, in: trains),
                  let train = trainsByID[resolved.trainID] else {
                skipped += 1
                continue
            }
            var accumulator = byLabel[resolved.label] ?? Accumulator()
            accumulator.annotationCount += 1
            accumulator.trainIDs.insert(resolved.trainID)
            if let start = resolved.startISIIndex, let end = resolved.endISIIndex, start <= end {
                for index in start...end where train.isiSec.indices.contains(index) {
                    if let value = train.isiSec[index], value.isFinite, value > minValid {
                        accumulator.values.append(value)
                    }
                }
            }
            byLabel[resolved.label] = accumulator
        }

        var rows: [ManualAnnotationCalibrationLabelSummary] = []

        // Burst family (combine .burst + .longBurst positives) first, if present.
        let burstFamilyLabels: [ManualAnnotationLabel] = [.burst, .longBurst]
        if burstFamilyLabels.contains(where: { byLabel[$0] != nil }) {
            var combined = Accumulator()
            for label in burstFamilyLabels {
                if let accumulator = byLabel[label] {
                    combined.values.append(contentsOf: accumulator.values)
                    combined.annotationCount += accumulator.annotationCount
                    combined.trainIDs.formUnion(accumulator.trainIDs)
                }
            }
            rows.append(makePositiveRow(
                label: familyBurst,
                displayName: "Burst family",
                isFamily: true,
                values: combined.values,
                annotationCount: combined.annotationCount,
                trainCount: combined.trainIDs.count,
                minUsableCount: 3,
                recommendationText: "Preview: seed/profile upper ≈ q90, bridge/profile upper ≈ q95, dense-core ref ≈ q40 (ms). Not applied to the detector."
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
                annotationCount: accumulator.annotationCount,
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
                annotationCount: vetoAccumulator.annotationCount,
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
        .burst, .longBurst, .tonic, .highFrequencyTonic, .highFrequencySpiking, .pause, .other
    ]

    private static func minUsableCount(for label: ManualAnnotationLabel) -> Int {
        switch label {
        case .burst, .longBurst: return 3
        case .highFrequencyTonic: return 4
        case .highFrequencySpiking: return 8
        case .tonic: return 4
        case .pause: return 2
        case .other, .notBurst: return Int.max   // summarize but never usable for calibration
        }
    }

    private static func recommendation(for label: ManualAnnotationLabel) -> String {
        switch label {
        case .burst, .longBurst:
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
