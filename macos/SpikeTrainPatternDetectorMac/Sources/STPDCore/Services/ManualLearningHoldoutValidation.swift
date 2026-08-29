import Foundation

/// An explicit train-level separation between evidence used to learn a proposal and evidence used
/// only to evaluate it. Stable semantic train IDs are required; ISIs from one train can never be
/// divided between the two roles.
public struct ManualLearningHoldoutSplit: Hashable, Sendable {
    public let calibrationTrainIDs: [ScientificSpikeTrainID]
    public let heldOutTrainIDs: [ScientificSpikeTrainID]

    public init(
        calibrationTrainIDs: some Sequence<ScientificSpikeTrainID>,
        heldOutTrainIDs: some Sequence<ScientificSpikeTrainID>
    ) {
        self.calibrationTrainIDs = Self.canonical(calibrationTrainIDs)
        self.heldOutTrainIDs = Self.canonical(heldOutTrainIDs)
    }

    private static func canonical(
        _ values: some Sequence<ScientificSpikeTrainID>
    ) -> [ScientificSpikeTrainID] {
        Array(Set(values)).sorted {
            $0.semanticID.canonicalText.utf8.lexicographicallyPrecedes(
                $1.semanticID.canonicalText.utf8
            )
        }
    }
}

public enum ManualLearningHoldoutValidationError: Error, Equatable, Sendable, LocalizedError {
    case canonicalFingerprintMismatch
    case emptyCalibrationSet
    case emptyHeldOutSet
    case overlappingTrainRoles([ScientificSpikeTrainID])
    case unknownTrainIDs([ScientificSpikeTrainID])
    case noCalibrationManualEvidence
    case invalidMinimumValidISI
    case minimumValidISINotExactlyRepresentable

    public var errorDescription: String? {
        switch self {
        case .canonicalFingerprintMismatch:
            return "The holdout validation inputs do not belong to the supplied canonical dataset."
        case .emptyCalibrationSet:
            return "At least one calibration spike train is required."
        case .emptyHeldOutSet:
            return "At least one held-out spike train is required."
        case .overlappingTrainRoles(let values):
            return "A spike train cannot be both calibration and held out: \(Self.names(values))."
        case .unknownTrainIDs(let values):
            return "The split contains unknown spike trains: \(Self.names(values))."
        case .noCalibrationManualEvidence:
            return "The calibration spike trains contain no manual pattern evidence."
        case .invalidMinimumValidISI:
            return "The effective minimum-valid-ISI boundary must be finite and nonnegative."
        case .minimumValidISINotExactlyRepresentable:
            return "The effective minimum-valid-ISI boundary is not exactly representable in integer microseconds."
        }
    }

    private static func names(_ values: [ScientificSpikeTrainID]) -> String {
        values.map(\.semanticID.canonicalText).joined(separator: ", ")
    }
}

/// Detector settings frozen for both arms of one holdout comparison. The only intentional
/// difference between the two runs is the manual-threshold profile: automatic versus the proposal
/// learned exclusively from calibration trains.
public struct ManualLearningHoldoutValidationConfiguration: Equatable, Sendable {
    public let bandSettings: TrainAdaptiveBandSettings
    public let qualitySettings: SpikeQualitySettings
    public let refractoryAction: ClassicAnchorRefractoryAction
    public let stateTuning: StatePatternDetectorTuning
    public let detectorParameters: PatternDetectionParameterSettings
    public let useAdaptiveV2Canonicalization: Bool
    /// The exact manual profile already active when validation starts. Every family-specific
    /// learned arm retains all unrelated fields from this baseline.
    public let baselineManualThresholdProfile: ManualThresholdProfile
    /// The exact resolved scope used by both baseline and learned arms. This is material for HFS
    /// because its learned minimum-support fields are hard gates.
    public let manualThresholdScope: ManualThresholdScope

    public init(
        bandSettings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        refractoryAction: ClassicAnchorRefractoryAction = .warnOnly,
        stateTuning: StatePatternDetectorTuning = StatePatternDetectorTuning(),
        detectorParameters: PatternDetectionParameterSettings = .defaults,
        useAdaptiveV2Canonicalization: Bool = false,
        baselineManualThresholdProfile: ManualThresholdProfile = .automatic,
        manualThresholdScope: ManualThresholdScope = .allTrains
    ) {
        self.bandSettings = bandSettings
        self.qualitySettings = qualitySettings
        self.refractoryAction = refractoryAction
        self.stateTuning = stateTuning
        self.detectorParameters = detectorParameters
        self.useAdaptiveV2Canonicalization = useAdaptiveV2Canonicalization
        self.baselineManualThresholdProfile = baselineManualThresholdProfile
        self.manualThresholdScope = manualThresholdScope
    }

    func exactMinimumValidISIMicroseconds() throws -> Int64 {
        let seconds = max(bandSettings.minValidISISec, qualitySettings.artifactThresholdSec)
        guard seconds.isFinite, seconds >= 0,
              seconds <= Double(Int64.max) / 1_000_000 else {
            throw ManualLearningHoldoutValidationError.invalidMinimumValidISI
        }
        let scaled = seconds * 1_000_000
        let rounded = scaled.rounded()
        let tolerance = max(1e-9, abs(scaled) * 1e-12)
        guard abs(scaled - rounded) <= tolerance else {
            throw ManualLearningHoldoutValidationError
                .minimumValidISINotExactlyRepresentable
        }
        return Int64(rounded)
    }
}

/// One reviewed ISI used by the pure metric calculator. Missing/unreviewed rows never appear in
/// this table, so detector predictions on blank manual rows cannot silently become false positives.
public struct ManualLearningHoldoutEvaluationPoint: Hashable, Sendable {
    public let trainID: String
    public let isiIndex: Int
    public let truthIsPositive: Bool
    public let baselinePredictedPositive: Bool
    public let learnedPredictedPositive: Bool

    public init(
        trainID: String,
        isiIndex: Int,
        truthIsPositive: Bool,
        baselinePredictedPositive: Bool,
        learnedPredictedPositive: Bool
    ) {
        self.trainID = trainID
        self.isiIndex = isiIndex
        self.truthIsPositive = truthIsPositive
        self.baselinePredictedPositive = baselinePredictedPositive
        self.learnedPredictedPositive = learnedPredictedPositive
    }
}

public struct ManualLearningDetectorMetrics: Hashable, Sendable {
    public let assessedISICount: Int
    public let truthPositiveISICount: Int
    public let predictedPositiveISICount: Int
    public let truePositiveISICount: Int
    public let falsePositiveISICount: Int
    public let falseNegativeISICount: Int
    public let trueNegativeISICount: Int
    public let precision: Double?
    public let recall: Double?
    public let f1: Double?
    public let truthSegmentCount: Int
    public let predictedSegmentCount: Int
    public let meanBestSegmentIoU: Double?
    public let meanMatchedBoundaryErrorISI: Double?
    public let falseSplitCount: Int
    public let falseMergeCount: Int

    public init(
        assessedISICount: Int,
        truthPositiveISICount: Int,
        predictedPositiveISICount: Int,
        truePositiveISICount: Int,
        falsePositiveISICount: Int,
        falseNegativeISICount: Int,
        trueNegativeISICount: Int,
        precision: Double?,
        recall: Double?,
        f1: Double?,
        truthSegmentCount: Int,
        predictedSegmentCount: Int,
        meanBestSegmentIoU: Double?,
        meanMatchedBoundaryErrorISI: Double?,
        falseSplitCount: Int,
        falseMergeCount: Int
    ) {
        self.assessedISICount = assessedISICount
        self.truthPositiveISICount = truthPositiveISICount
        self.predictedPositiveISICount = predictedPositiveISICount
        self.truePositiveISICount = truePositiveISICount
        self.falsePositiveISICount = falsePositiveISICount
        self.falseNegativeISICount = falseNegativeISICount
        self.trueNegativeISICount = trueNegativeISICount
        self.precision = precision
        self.recall = recall
        self.f1 = f1
        self.truthSegmentCount = truthSegmentCount
        self.predictedSegmentCount = predictedSegmentCount
        self.meanBestSegmentIoU = meanBestSegmentIoU
        self.meanMatchedBoundaryErrorISI = meanMatchedBoundaryErrorISI
        self.falseSplitCount = falseSplitCount
        self.falseMergeCount = falseMergeCount
    }
}

public struct ManualLearningHoldoutFamilyComparison: Hashable, Sendable {
    public let family: ManualPatternLearningFamily
    /// True only when validation actually materialized a one-family detector intervention.
    public let hasCompatibleIntervention: Bool
    public let assessedTrainCount: Int
    public let truthPositiveTrainCount: Int
    public let explicitNegativeTrainCount: Int
    public let geometryCompleteTrainCount: Int
    public let heldOutEvaluationDigest: String
    public let learnedSettingsDigest: String
    public let learnedSettingsEntries: [DetectionSettingEntry]
    public let trainComparisons: [ManualLearningHoldoutTrainComparison]
    public let baseline: ManualLearningDetectorMetrics
    public let learned: ManualLearningDetectorMetrics

    public init(
        family: ManualPatternLearningFamily,
        hasCompatibleIntervention: Bool = false,
        assessedTrainCount: Int,
        truthPositiveTrainCount: Int,
        explicitNegativeTrainCount: Int,
        geometryCompleteTrainCount: Int,
        heldOutEvaluationDigest: String,
        learnedSettingsDigest: String = "",
        learnedSettingsEntries: [DetectionSettingEntry] = [],
        trainComparisons: [ManualLearningHoldoutTrainComparison],
        baseline: ManualLearningDetectorMetrics,
        learned: ManualLearningDetectorMetrics
    ) {
        self.family = family
        self.hasCompatibleIntervention = hasCompatibleIntervention
        self.assessedTrainCount = assessedTrainCount
        self.truthPositiveTrainCount = truthPositiveTrainCount
        self.explicitNegativeTrainCount = explicitNegativeTrainCount
        self.geometryCompleteTrainCount = geometryCompleteTrainCount
        self.heldOutEvaluationDigest = heldOutEvaluationDigest
        self.learnedSettingsDigest = learnedSettingsDigest
        self.learnedSettingsEntries = learnedSettingsEntries
        self.trainComparisons = trainComparisons
        self.baseline = baseline
        self.learned = learned
    }

    public var f1Delta: Double? {
        guard let baseline = baseline.f1, let learned = learned.f1 else { return nil }
        return learned - baseline
    }

    public var meanBestSegmentIoUDelta: Double? {
        guard let baseline = baseline.meanBestSegmentIoU,
              let learned = learned.meanBestSegmentIoU else { return nil }
        return learned - baseline
    }

    public var meanMatchedBoundaryErrorDelta: Double? {
        guard let baseline = baseline.meanMatchedBoundaryErrorISI,
              let learned = learned.meanMatchedBoundaryErrorISI else { return nil }
        return learned - baseline
    }

    public var falsePositiveDelta: Int {
        learned.falsePositiveISICount - baseline.falsePositiveISICount
    }

    public var falseNegativeDelta: Int {
        learned.falseNegativeISICount - baseline.falseNegativeISICount
    }

    public var falseSplitDelta: Int { learned.falseSplitCount - baseline.falseSplitCount }
    public var falseMergeDelta: Int { learned.falseMergeCount - baseline.falseMergeCount }
}

/// A held-out train is the independent evaluation unit. Admission checks every one of these rows;
/// family-level pooling is retained only as a descriptive summary and cannot hide a local regression.
public struct ManualLearningHoldoutTrainComparison: Hashable, Sendable {
    public let trainID: String
    public let geometryIsComplete: Bool
    public let baseline: ManualLearningDetectorMetrics
    public let learned: ManualLearningDetectorMetrics

    public init(
        trainID: String,
        geometryIsComplete: Bool,
        baseline: ManualLearningDetectorMetrics,
        learned: ManualLearningDetectorMetrics
    ) {
        self.trainID = trainID
        self.geometryIsComplete = geometryIsComplete
        self.baseline = baseline
        self.learned = learned
    }

    public var f1Delta: Double? {
        guard let baseline = baseline.f1, let learned = learned.f1 else { return nil }
        return learned - baseline
    }
    public var meanBestSegmentIoUDelta: Double? {
        guard let baseline = baseline.meanBestSegmentIoU,
              let learned = learned.meanBestSegmentIoU else { return nil }
        return learned - baseline
    }
    public var meanMatchedBoundaryErrorDelta: Double? {
        guard let baseline = baseline.meanMatchedBoundaryErrorISI,
              let learned = learned.meanMatchedBoundaryErrorISI else { return nil }
        return learned - baseline
    }
    public var falsePositiveDelta: Int {
        learned.falsePositiveISICount - baseline.falsePositiveISICount
    }
    public var falseNegativeDelta: Int {
        learned.falseNegativeISICount - baseline.falseNegativeISICount
    }
    public var falseSplitDelta: Int { learned.falseSplitCount - baseline.falseSplitCount }
    public var falseMergeDelta: Int { learned.falseMergeCount - baseline.falseMergeCount }
}

/// This is a conservative workflow disposition, not a biological truth label or a statistical
/// significance claim. Only the last state permits an explicit user-confirmed parameter write.
public enum ManualLearningAdmissionDisposition: String, CaseIterable, Hashable, Sendable {
    case insufficientEvidence = "insufficient_evidence"
    case reportOnly = "report_only"
    case eligibleForExplicitApplication = "eligible_for_explicit_application"
}

public enum ManualLearningAdmissionReason: String, CaseIterable, Hashable, Sendable {
    case noCompatibleThreshold = "no_compatible_threshold"
    case calibrationEvidenceBelowTwoTrains = "calibration_evidence_below_two_trains"
    case calibrationCrossTrainValidationUnavailable =
        "calibration_cross_train_validation_unavailable"
    case calibrationCrossTrainValidationNotPassed =
        "calibration_cross_train_validation_not_passed"
    case noReviewedHeldOutISIs = "no_reviewed_held_out_isis"
    case noHeldOutPositiveSupport = "no_held_out_positive_support"
    case noHeldOutExplicitNegativeSupport = "no_held_out_explicit_negative_support"
    case heldOutGeometryReviewIncomplete = "held_out_geometry_review_incomplete"
    case oneOrMoreHeldOutTrainsRegressed = "one_or_more_held_out_trains_regressed"
    case f1Regressed = "f1_regressed"
    case segmentIoURegressed = "segment_iou_regressed"
    case boundaryErrorIncreased = "boundary_error_increased"
    case falsePositivesIncreased = "false_positives_increased"
    case falseNegativesIncreased = "false_negatives_increased"
    case falseSplitsIncreased = "false_splits_increased"
    case falseMergesIncreased = "false_merges_increased"
    case noObservedImprovement = "no_observed_improvement"
    case observedImprovementWithoutMeasuredRegression =
        "observed_improvement_without_measured_regression"
}

public struct ManualLearningFamilyAdmission: Hashable, Sendable {
    public let family: ManualPatternLearningFamily
    public let disposition: ManualLearningAdmissionDisposition
    public let reasons: [ManualLearningAdmissionReason]

    public init(
        family: ManualPatternLearningFamily,
        disposition: ManualLearningAdmissionDisposition,
        reasons: [ManualLearningAdmissionReason]
    ) {
        self.family = family
        self.disposition = disposition
        self.reasons = reasons
    }
}

public struct ManualLearningHoldoutValidationReport: Hashable, Sendable {
    public let schemaContractID: String
    public let schemaContractDigest: String
    public let admissionContractDigest: String
    public let sourceDatasetDigest: String
    public let heldOutEvidenceDigest: String
    public let heldOutEvaluationDigest: String
    public let calibrationTrainIDs: [String]
    public let heldOutTrainIDs: [String]
    public let excludedTrainIDs: [String]
    public let calibrationProposal: ManualPatternLearningProposal
    public let baselineSettingsDigest: String
    public let baselineSettingsEntries: [DetectionSettingEntry]
    public let comparisons: [ManualLearningHoldoutFamilyComparison]
    public let admissions: [ManualLearningFamilyAdmission]
    public let reportDigest: String

    init(
        schemaContractID: String,
        schemaContractDigest: String,
        admissionContractDigest: String,
        sourceDatasetDigest: String,
        heldOutEvidenceDigest: String,
        heldOutEvaluationDigest: String,
        calibrationTrainIDs: [String],
        heldOutTrainIDs: [String],
        excludedTrainIDs: [String],
        calibrationProposal: ManualPatternLearningProposal,
        baselineSettingsDigest: String,
        baselineSettingsEntries: [DetectionSettingEntry],
        comparisons: [ManualLearningHoldoutFamilyComparison],
        admissions: [ManualLearningFamilyAdmission],
        reportDigest: String
    ) {
        self.schemaContractID = schemaContractID
        self.schemaContractDigest = schemaContractDigest
        self.admissionContractDigest = admissionContractDigest
        self.sourceDatasetDigest = sourceDatasetDigest
        self.heldOutEvidenceDigest = heldOutEvidenceDigest
        self.heldOutEvaluationDigest = heldOutEvaluationDigest
        self.calibrationTrainIDs = calibrationTrainIDs
        self.heldOutTrainIDs = heldOutTrainIDs
        self.excludedTrainIDs = excludedTrainIDs
        self.calibrationProposal = calibrationProposal
        self.baselineSettingsDigest = baselineSettingsDigest
        self.baselineSettingsEntries = baselineSettingsEntries
        self.comparisons = comparisons
        self.admissions = admissions
        self.reportDigest = reportDigest
    }

    public var assessedFamilyCount: Int {
        comparisons.lazy.filter { $0.baseline.assessedISICount > 0 }.count
    }

    public var explicitlyApplicableFamilies: [ManualPatternLearningFamily] {
        admissions.compactMap {
            $0.disposition == .eligibleForExplicitApplication ? $0.family : nil
        }
    }
}

/// Deterministic, intentionally conservative admission policy. It does not optimize a weighted
/// score and therefore cannot hide a precision/recall or geometry trade-off behind one number.
public enum ManualLearningAdmissionEvaluator {
    private static let tolerance = 1e-12

    public static func assess(
        comparison: ManualLearningHoldoutFamilyComparison,
        calibrationSummary: ManualPatternFamilyLearningSummary?
    ) -> ManualLearningFamilyAdmission {
        var insufficient: [ManualLearningAdmissionReason] = []
        if !comparison.hasCompatibleIntervention {
            insufficient.append(.noCompatibleThreshold)
        }
        guard let calibrationSummary else {
            insufficient.append(.calibrationEvidenceBelowTwoTrains)
            return admission(comparison.family, .insufficientEvidence, insufficient)
        }
        switch calibrationSummary.standing {
        case .insufficient, .exploratorySingleTrain:
            insufficient.append(.calibrationEvidenceBelowTwoTrains)
        case .provisionalTwoTrains, .supportedMultiTrain:
            break
        }
        if comparison.baseline.assessedISICount == 0 {
            insufficient.append(.noReviewedHeldOutISIs)
        }
        if comparison.baseline.truthPositiveISICount == 0 {
            insufficient.append(.noHeldOutPositiveSupport)
        }
        let explicitNegativeCount = comparison.baseline.assessedISICount
            - comparison.baseline.truthPositiveISICount
        if explicitNegativeCount == 0 {
            insufficient.append(.noHeldOutExplicitNegativeSupport)
        }
        if calibrationSummary.validation.standing == .unavailable {
            insufficient.append(.calibrationCrossTrainValidationUnavailable)
        }
        if !insufficient.isEmpty {
            return admission(comparison.family, .insufficientEvidence, insufficient)
        }

        var reportOnly: [ManualLearningAdmissionReason] = []
        if calibrationSummary.validation.standing != .passed {
            reportOnly.append(.calibrationCrossTrainValidationNotPassed)
        }
        if comparison.geometryCompleteTrainCount != comparison.assessedTrainCount {
            reportOnly.append(.heldOutGeometryReviewIncomplete)
        }
        if comparison.trainComparisons.contains(where: hasMeasuredRegression) {
            reportOnly.append(.oneOrMoreHeldOutTrainsRegressed)
        }
        if let delta = comparison.f1Delta, delta < -tolerance {
            reportOnly.append(.f1Regressed)
        }
        if let delta = comparison.meanBestSegmentIoUDelta, delta < -tolerance {
            reportOnly.append(.segmentIoURegressed)
        }
        if let delta = comparison.meanMatchedBoundaryErrorDelta, delta > tolerance {
            reportOnly.append(.boundaryErrorIncreased)
        }
        if comparison.falsePositiveDelta > 0 {
            reportOnly.append(.falsePositivesIncreased)
        }
        if comparison.falseNegativeDelta > 0 {
            reportOnly.append(.falseNegativesIncreased)
        }
        if comparison.falseSplitDelta > 0 {
            reportOnly.append(.falseSplitsIncreased)
        }
        if comparison.falseMergeDelta > 0 {
            reportOnly.append(.falseMergesIncreased)
        }
        if !reportOnly.isEmpty {
            return admission(comparison.family, .reportOnly, reportOnly)
        }

        let improved = comparison.trainComparisons.contains(where: hasMeasuredImprovement)
        guard improved else {
            return admission(comparison.family, .reportOnly, [.noObservedImprovement])
        }
        return admission(
            comparison.family,
            .eligibleForExplicitApplication,
            [.observedImprovementWithoutMeasuredRegression]
        )
    }

    private static func admission(
        _ family: ManualPatternLearningFamily,
        _ disposition: ManualLearningAdmissionDisposition,
        _ reasons: [ManualLearningAdmissionReason]
    ) -> ManualLearningFamilyAdmission {
        ManualLearningFamilyAdmission(
            family: family,
            disposition: disposition,
            reasons: Array(Set(reasons)).sorted { $0.rawValue < $1.rawValue }
        )
    }

    private static func hasMeasuredRegression(
        _ comparison: ManualLearningHoldoutTrainComparison
    ) -> Bool {
        (comparison.f1Delta.map { $0 < -tolerance } ?? false)
            || (comparison.meanBestSegmentIoUDelta.map { $0 < -tolerance } ?? false)
            || (comparison.meanMatchedBoundaryErrorDelta.map { $0 > tolerance } ?? false)
            || comparison.falsePositiveDelta > 0
            || comparison.falseNegativeDelta > 0
            || comparison.falseSplitDelta > 0
            || comparison.falseMergeDelta > 0
    }

    private static func hasMeasuredImprovement(
        _ comparison: ManualLearningHoldoutTrainComparison
    ) -> Bool {
        (comparison.f1Delta.map { $0 > tolerance } ?? false)
            || (comparison.meanBestSegmentIoUDelta.map { $0 > tolerance } ?? false)
            || (comparison.meanMatchedBoundaryErrorDelta.map { $0 < -tolerance } ?? false)
            || comparison.falsePositiveDelta < 0
            || comparison.falseNegativeDelta < 0
            || comparison.falseSplitDelta < 0
            || comparison.falseMergeDelta < 0
    }
}

/// Pure support-level and event/state-segment metrics. The calculator deliberately issues no
/// automatic "pass" verdict: precision/recall, geometry, and fragmentation may move in different
/// directions, and that scientific trade-off must remain visible.
public enum ManualLearningHoldoutMetricCalculator {
    public static func compare(
        family: ManualPatternLearningFamily,
        points: [ManualLearningHoldoutEvaluationPoint],
        heldOutTrainIDs: Set<String>? = nil,
        geometryCompleteTrainIDs: Set<String>? = nil,
        hasCompatibleIntervention: Bool = false,
        learnedSettingsDigest: String = "",
        learnedSettingsEntries: [DetectionSettingEntry] = []
    ) -> ManualLearningHoldoutFamilyComparison {
        let canonical = canonicalPoints(points)
        let assessedTrainIDs = Set(canonical.map(\.trainID))
        let evaluationTrainIDs = heldOutTrainIDs ?? assessedTrainIDs
        let complete = geometryCompleteTrainIDs ?? assessedTrainIDs
        let grouped = Dictionary(grouping: canonical, by: \.trainID)
        let trainComparisons = evaluationTrainIDs.sorted {
            $0.utf8.lexicographicallyPrecedes($1.utf8)
        }.map { trainID in
            let trainPoints = grouped[trainID, default: []]
            let geometryPoints = complete.contains(trainID) ? trainPoints : []
            return ManualLearningHoldoutTrainComparison(
                trainID: trainID,
                geometryIsComplete: complete.contains(trainID),
                baseline: metrics(
                    points: trainPoints,
                    geometryPoints: geometryPoints,
                    prediction: \.baselinePredictedPositive
                ),
                learned: metrics(
                    points: trainPoints,
                    geometryPoints: geometryPoints,
                    prediction: \.learnedPredictedPositive
                )
            )
        }
        let aggregateGeometryPoints = canonical.filter { complete.contains($0.trainID) }
        return ManualLearningHoldoutFamilyComparison(
            family: family,
            hasCompatibleIntervention: hasCompatibleIntervention,
            assessedTrainCount: assessedTrainIDs.count,
            truthPositiveTrainCount: Set(canonical.lazy.filter(\.truthIsPositive).map(\.trainID)).count,
            explicitNegativeTrainCount: Set(canonical.lazy.filter { !$0.truthIsPositive }.map(\.trainID)).count,
            geometryCompleteTrainCount: assessedTrainIDs.intersection(complete).count,
            heldOutEvaluationDigest: evaluationDigest(
                family: family,
                heldOutTrainIDs: evaluationTrainIDs,
                points: canonical
            ),
            learnedSettingsDigest: learnedSettingsDigest,
            learnedSettingsEntries: learnedSettingsEntries,
            trainComparisons: trainComparisons,
            baseline: metrics(
                points: canonical,
                geometryPoints: aggregateGeometryPoints,
                prediction: \.baselinePredictedPositive
            ),
            learned: metrics(
                points: canonical,
                geometryPoints: aggregateGeometryPoints,
                prediction: \.learnedPredictedPositive
            )
        )
    }

    private static func metrics(
        points: [ManualLearningHoldoutEvaluationPoint],
        geometryPoints: [ManualLearningHoldoutEvaluationPoint],
        prediction: KeyPath<ManualLearningHoldoutEvaluationPoint, Bool>
    ) -> ManualLearningDetectorMetrics {
        let unique = canonicalPoints(points)
        let geometryUnique = canonicalPoints(geometryPoints)
        let truthPositive = unique.filter(\.truthIsPositive)
        let predictedPositive = unique.filter { $0[keyPath: prediction] }
        let truePositive = unique.filter {
            $0.truthIsPositive && $0[keyPath: prediction]
        }.count
        let falsePositive = unique.filter {
            !$0.truthIsPositive && $0[keyPath: prediction]
        }.count
        let falseNegative = unique.filter {
            $0.truthIsPositive && !$0[keyPath: prediction]
        }.count
        let trueNegative = unique.count - truePositive - falsePositive - falseNegative

        let precision = predictedPositive.isEmpty
            ? nil
            : Double(truePositive) / Double(predictedPositive.count)
        let recall = truthPositive.isEmpty
            ? nil
            : Double(truePositive) / Double(truthPositive.count)
        let f1Denominator = 2 * truePositive + falsePositive + falseNegative
        let f1 = f1Denominator == 0
            ? nil
            : Double(2 * truePositive) / Double(f1Denominator)

        let truthSegments = segments(geometryUnique.filter(\.truthIsPositive))
        let predictedSegments = segments(geometryUnique.filter { $0[keyPath: prediction] })
        let geometry = segmentGeometry(truth: truthSegments, predicted: predictedSegments)

        return ManualLearningDetectorMetrics(
            assessedISICount: unique.count,
            truthPositiveISICount: truthPositive.count,
            predictedPositiveISICount: predictedPositive.count,
            truePositiveISICount: truePositive,
            falsePositiveISICount: falsePositive,
            falseNegativeISICount: falseNegative,
            trueNegativeISICount: trueNegative,
            precision: precision,
            recall: recall,
            f1: f1,
            truthSegmentCount: truthSegments.count,
            predictedSegmentCount: predictedSegments.count,
            meanBestSegmentIoU: geometry.meanBestIoU,
            meanMatchedBoundaryErrorISI: geometry.meanMatchedBoundaryError,
            falseSplitCount: geometry.falseSplits,
            falseMergeCount: geometry.falseMerges
        )
    }

    private struct PointKey: Hashable {
        let trainID: String
        let isiIndex: Int
    }

    private struct Segment {
        let trainID: String
        let lower: Int
        let upper: Int

        var count: Int { upper - lower + 1 }

        func intersectionCount(with other: Segment) -> Int {
            guard trainID == other.trainID else { return 0 }
            return max(0, min(upper, other.upper) - max(lower, other.lower) + 1)
        }
    }

    private static func canonicalPoints(
        _ points: [ManualLearningHoldoutEvaluationPoint]
    ) -> [ManualLearningHoldoutEvaluationPoint] {
        var byKey: [PointKey: ManualLearningHoldoutEvaluationPoint] = [:]
        for point in points where point.isiIndex > 0 {
            byKey[PointKey(trainID: point.trainID, isiIndex: point.isiIndex)] = point
        }
        return byKey.values.sorted {
            if $0.trainID != $1.trainID {
                return $0.trainID.utf8.lexicographicallyPrecedes($1.trainID.utf8)
            }
            return $0.isiIndex < $1.isiIndex
        }
    }

    private static func evaluationDigest(
        family: ManualPatternLearningFamily,
        heldOutTrainIDs: Set<String>,
        points: [ManualLearningHoldoutEvaluationPoint]
    ) -> String {
        ManualLearningDigest.hex(tokens: [
            "manual_learning_holdout_evaluation_points_v2",
            family.rawValue,
            String(heldOutTrainIDs.count),
        ] + heldOutTrainIDs.sorted {
            $0.utf8.lexicographicallyPrecedes($1.utf8)
        }.map { "held_out_train:\($0)" } + [
            String(points.count),
        ] + points.flatMap {
            [
                $0.trainID,
                String($0.isiIndex),
                $0.truthIsPositive ? "truth:positive" : "truth:negative",
                $0.baselinePredictedPositive ? "baseline:positive" : "baseline:negative",
                $0.learnedPredictedPositive ? "learned:positive" : "learned:negative",
            ]
        })
    }

    private static func segments(
        _ points: [ManualLearningHoldoutEvaluationPoint]
    ) -> [Segment] {
        let grouped = Dictionary(grouping: points, by: \.trainID)
        return grouped.keys.sorted().flatMap { trainID -> [Segment] in
            let indices = Set(grouped[trainID, default: []].map(\.isiIndex)).sorted()
            guard let first = indices.first else { return [] }
            var result: [Segment] = []
            var lower = first
            var upper = first
            for index in indices.dropFirst() {
                if index == upper + 1 {
                    upper = index
                } else {
                    result.append(Segment(trainID: trainID, lower: lower, upper: upper))
                    lower = index
                    upper = index
                }
            }
            result.append(Segment(trainID: trainID, lower: lower, upper: upper))
            return result
        }
    }

    private static func segmentGeometry(
        truth: [Segment],
        predicted: [Segment]
    ) -> (
        meanBestIoU: Double?,
        meanMatchedBoundaryError: Double?,
        falseSplits: Int,
        falseMerges: Int
    ) {
        var bestIoUs: [Double] = []
        var matchedBoundaryErrors: [Double] = []
        var falseSplits = 0
        for truthSegment in truth {
            let overlaps = predicted.compactMap { predictedSegment -> (Segment, Int)? in
                let count = truthSegment.intersectionCount(with: predictedSegment)
                return count > 0 ? (predictedSegment, count) : nil
            }
            falseSplits += max(0, overlaps.count - 1)
            guard let best = overlaps.max(by: {
                let leftUnion = truthSegment.count + $0.0.count - $0.1
                let rightUnion = truthSegment.count + $1.0.count - $1.1
                return Double($0.1) / Double(leftUnion) < Double($1.1) / Double(rightUnion)
            }) else {
                bestIoUs.append(0)
                continue
            }
            let union = truthSegment.count + best.0.count - best.1
            bestIoUs.append(Double(best.1) / Double(union))
            matchedBoundaryErrors.append(
                Double(abs(truthSegment.lower - best.0.lower)
                    + abs(truthSegment.upper - best.0.upper)) / 2
            )
        }

        let falseMerges = predicted.reduce(into: 0) { result, predictedSegment in
            let overlapCount = truth.lazy.filter {
                predictedSegment.intersectionCount(with: $0) > 0
            }.count
            result += max(0, overlapCount - 1)
        }
        return (
            meanBestIoU: mean(bestIoUs),
            meanMatchedBoundaryError: mean(matchedBoundaryErrors),
            falseSplits: falseSplits,
            falseMerges: falseMerges
        )
    }

    private static func mean(_ values: [Double]) -> Double? {
        guard !values.isEmpty else { return nil }
        return values.reduce(0, +) / Double(values.count)
    }
}

/// Compose the exact one-family intervention evaluated by holdout validation. Unrelated current
/// manual fields and their provenance remain active, while an existing hard setting in the same
/// family is never overwritten. The app uses this same composition contract before application.
public enum ManualLearningHoldoutProfileComposer {
    public static func applying(
        family: ManualPatternLearningFamily,
        proposal: ManualPatternLearningProposal,
        to baseline: ManualThresholdProfile
    ) -> ManualThresholdProfile? {
        guard let familyKey = family.compatibleThresholdFamilyKey,
              proposal.compatibleThresholdProposal.contributions.contains(where: {
                  $0.family == familyKey
              }) else { return nil }

        var result = baseline
        switch family {
        case .burstFamily:
            guard !containsHardMode([
                baseline.burst.seedLowerISI.mode,
                baseline.burst.seedUpperISI.mode,
                baseline.burst.bridgeUpperISI.mode,
                baseline.burst.minSpikes.mode,
                baseline.burst.classicMaxSpikes.mode,
                baseline.burst.longMinSpikes.mode,
                baseline.burst.longMaxSpikes.mode,
            ]) else { return nil }
            result.burst = proposal.compatibleThresholdProposal.profile.burst
        case .highFrequencySpiking:
            guard !containsHardMode([
                baseline.hfs.minSpikes.mode,
                baseline.hfs.minDurationSec.mode,
            ]) else { return nil }
            result.hfs = proposal.compatibleThresholdProposal.profile.hfs
        case .highFrequencyTonic:
            guard !containsHardMode([
                baseline.hfTonic.minSpikes.mode,
                baseline.hfTonic.isiFloor.mode,
                baseline.hfTonic.isiUpper.mode,
            ]) else { return nil }
            result.hfTonic = proposal.compatibleThresholdProposal.profile.hfTonic
        case .tonic:
            guard !containsHardMode([
                baseline.tonic.minSpikes.mode,
                baseline.tonic.isiLower.mode,
                baseline.tonic.isiUpper.mode,
            ]) else { return nil }
            result.tonic = proposal.compatibleThresholdProposal.profile.tonic
        case .pause:
            guard !containsHardMode([baseline.pause.isiLower.mode]) else { return nil }
            result.pause = proposal.compatibleThresholdProposal.profile.pause
        }

        var provenance = baseline.learnedProvenanceByKey.filter {
            !$0.key.hasPrefix(familyKey + ".")
        }
        for contribution in proposal.compatibleThresholdProposal.contributions
            where contribution.family == familyKey {
            let key = LearnedManualThresholdApplier.resolvedProvenanceKey(
                family: contribution.family,
                field: contribution.field
            )
            provenance[key] = proposal.identityBoundProvenanceNote(
                contribution.provenanceNote
            )
        }
        result.learnedProvenanceByKey = provenance
        return result
    }

    private static func containsHardMode(_ values: [ThresholdMode]) -> Bool {
        values.contains(.hardGate)
    }
}

/// End-to-end, label-leakage-safe validation. It learns once from calibration trains, runs the
/// detector twice on held-out raw timestamps, and compares only manually reviewed held-out rows.
/// Neither detector output is fed back into feature extraction or threshold learning.
public enum ManualLearningHoldoutValidator {
    public static let schemaContractID = "manual_learning_train_holdout_validation_v3"
    public static let schemaContractDigest = ManualLearningDigest.hex(tokens: [
        schemaContractID,
        "split_unit:whole_scientific_spike_train",
        "calibration_only:manual_learning_evidence_snapshot",
        "held_out_only:detector_evaluation",
        "baseline:exact_current_manual_profile_and_resolved_scope",
        "intervention:one_family_at_a_time",
        "support_metrics:reviewed_rows_only",
        "geometry:complete_reviewed_train_only",
        "evaluation_unit:held_out_train",
        "report_train_matrix:every_family_x_every_held_out_train",
        "compatible_intervention:materialized_validation_fact",
        "single_train_id_export:formula_safe_json_scalar",
        "report_identity:raw_held_out_decisions_plus_evaluation_points",
    ])
    public static let admissionContractDigest = ManualLearningDigest.hex(tokens: [
        "manual_learning_admission_contract_v2",
        "floating_tolerance:1e-12",
        "truth_event:burst_family|pause",
        "truth_state:tonic|high_frequency_tonic|high_frequency_spiking",
        "truth_other:explicit_negative_all_families",
        "blank:unassessed_never_negative",
        "requires:compatible_one_family_intervention",
        "requires:calibration_standing>=provisional_two_trains",
        "requires:calibration_cross_train_validation=passed",
        "requires:held_out_positive_and_explicit_negative_support",
        "requires:complete_geometry_review_for_every_assessed_train",
        "blocks:any_per_train_or_aggregate_f1_iou_boundary_fp_fn_split_merge_regression",
        "requires:at_least_one_per_train_measured_improvement",
        "dispositions:insufficient_evidence|report_only|eligible_for_explicit_application",
        "application:single_family_then_revalidate",
    ] + ManualLearningAdmissionReason.allCases.map { "reason:\($0.rawValue)" })

    public static func validate(
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        draft: CanonicalManualISILabelDraft,
        split: ManualLearningHoldoutSplit,
        configuration: ManualLearningHoldoutValidationConfiguration = .init()
    ) throws -> ManualLearningHoldoutValidationReport {
        let recomputed = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)
        guard recomputed == fingerprint,
              draft.canonicalFingerprint == fingerprint else {
            throw ManualLearningHoldoutValidationError.canonicalFingerprintMismatch
        }
        guard !split.calibrationTrainIDs.isEmpty else {
            throw ManualLearningHoldoutValidationError.emptyCalibrationSet
        }
        guard !split.heldOutTrainIDs.isEmpty else {
            throw ManualLearningHoldoutValidationError.emptyHeldOutSet
        }

        let calibrationSet = Set(split.calibrationTrainIDs)
        let heldOutSet = Set(split.heldOutTrainIDs)
        let overlap = calibrationSet.intersection(heldOutSet).sorted(by: semanticIDOrdered)
        guard overlap.isEmpty else {
            throw ManualLearningHoldoutValidationError.overlappingTrainRoles(overlap)
        }
        let known = Set(dataset.spikeTrains.map(\.semanticID))
        let unknown = calibrationSet.union(heldOutSet).subtracting(known)
            .sorted(by: semanticIDOrdered)
        guard unknown.isEmpty else {
            throw ManualLearningHoldoutValidationError.unknownTrainIDs(unknown)
        }

        let calibrationDataset = subset(dataset, trainIDs: calibrationSet)
        let calibrationFingerprint = try CanonicalScientificDatasetFingerprinter.fingerprint(
            calibrationDataset
        )
        let calibrationDecisions = draft.decisions.filter {
            calibrationSet.contains($0.trainID)
        }
        guard !calibrationDecisions.isEmpty else {
            throw ManualLearningHoldoutValidationError.noCalibrationManualEvidence
        }
        let calibrationDraft = CanonicalManualISILabelDraft(
            canonicalFingerprint: calibrationFingerprint,
            decisions: calibrationDecisions
        )
        let minimumValidISI = try configuration.exactMinimumValidISIMicroseconds()
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: calibrationDataset,
            fingerprint: calibrationFingerprint,
            draft: calibrationDraft,
            minimumValidISIMicroseconds: minimumValidISI
        )
        let features = ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        let proposal = ManualPatternLearningProposalBuilder.build(from: features)

        let heldOutCanonical = subset(dataset, trainIDs: heldOutSet)
        let heldOutFingerprint = try CanonicalScientificDatasetFingerprinter.fingerprint(
            heldOutCanonical
        )
        let heldOutDraft = CanonicalManualISILabelDraft(
            canonicalFingerprint: heldOutFingerprint,
            decisions: draft.decisions.filter { heldOutSet.contains($0.trainID) }
        )
        let heldOutRows = try CanonicalManualISILabelProjector.rows(
            dataset: heldOutCanonical,
            fingerprint: heldOutFingerprint,
            draft: heldOutDraft
        )
        let detectorDataset = displayDataset(from: heldOutCanonical, fingerprint: heldOutFingerprint)

        let baseline = runDetector(
            dataset: detectorDataset,
            manualThresholdProfile: configuration.baselineManualThresholdProfile,
            configuration: configuration,
            buildCommit: "manual_learning_holdout_validation"
        )
        let baselineSnapshot = baseline.invocationSettingsSnapshot
        let heldOutTrainNames = Set(heldOutCanonical.spikeTrains.map {
            $0.semanticID.semanticID.canonicalText
        })
        let allRowIndicesByTrain = Dictionary(grouping: heldOutRows, by: {
            $0.trainID.semanticID.canonicalText
        }).mapValues { Set($0.map(\.isiIndex)) }
        var comparisons: [ManualLearningHoldoutFamilyComparison] = []
        comparisons.reserveCapacity(ManualPatternLearningFamily.allCases.count)
        for family in ManualPatternLearningFamily.allCases {
            let intervention = ManualLearningHoldoutProfileComposer.applying(
                family: family,
                proposal: proposal,
                to: configuration.baselineManualThresholdProfile
            )
            let learned = intervention.map {
                runDetector(
                    dataset: detectorDataset,
                    manualThresholdProfile: $0,
                    configuration: configuration,
                    buildCommit: "manual_learning_holdout_validation"
                )
            } ?? baseline
            let baselineSupport = predictedSupport(
                family: family,
                run: baseline,
                dataset: detectorDataset
            )
            let learnedSupport = predictedSupport(
                family: family,
                run: learned,
                dataset: detectorDataset
            )
            let points = heldOutRows.compactMap { row -> ManualLearningHoldoutEvaluationPoint? in
                guard let truth = assessedTruth(family: family, row: row) else { return nil }
                let key = SupportKey(
                    trainID: row.trainID.semanticID.canonicalText,
                    isiIndex: row.isiIndex
                )
                return ManualLearningHoldoutEvaluationPoint(
                    trainID: key.trainID,
                    isiIndex: key.isiIndex,
                    truthIsPositive: truth,
                    baselinePredictedPositive: baselineSupport.contains(key),
                    learnedPredictedPositive: learnedSupport.contains(key)
                )
            }
            let assessedByTrain = Dictionary(grouping: points, by: \.trainID)
            let geometryCompleteTrainIDs = Set(assessedByTrain.compactMap {
                trainID, trainPoints -> String? in
                let assessed = Set(trainPoints.map(\.isiIndex))
                guard let all = allRowIndicesByTrain[trainID], assessed == all else {
                    return nil
                }
                return trainID
            })
            let learnedSnapshot = learned.invocationSettingsSnapshot
            comparisons.append(ManualLearningHoldoutMetricCalculator.compare(
                family: family,
                points: points,
                heldOutTrainIDs: heldOutTrainNames,
                geometryCompleteTrainIDs: geometryCompleteTrainIDs,
                hasCompatibleIntervention: intervention != nil,
                learnedSettingsDigest: learnedSnapshot?.digest ?? "",
                learnedSettingsEntries: learnedSnapshot?.entries ?? []
            ))
        }
        let summaryByFamily = Dictionary(uniqueKeysWithValues: proposal.summaries.map {
            ($0.family, $0)
        })
        let admissions = comparisons.map { comparison in
            ManualLearningAdmissionEvaluator.assess(
                comparison: comparison,
                calibrationSummary: summaryByFamily[comparison.family]
            )
        }

        let assigned = calibrationSet.union(heldOutSet)
        let excluded = known.subtracting(assigned).sorted(by: semanticIDOrdered)
        let calibrationNames = split.calibrationTrainIDs.map(semanticText)
        let heldOutNames = split.heldOutTrainIDs.map(semanticText)
        let excludedNames = excluded.map(semanticText)
        let baselineDigest = baselineSnapshot?.digest ?? ""
        let heldOutEvidenceDigest = heldOutDecisionDigest(
            fingerprint: heldOutFingerprint,
            decisions: heldOutDraft.decisions
        )
        let heldOutEvaluationDigest = ManualLearningDigest.hex(tokens: [
            "manual_learning_holdout_all_family_evaluation_v1",
        ] + comparisons.flatMap {
            [$0.family.rawValue, $0.heldOutEvaluationDigest]
        })
        let digest = ManualLearningDigest.hex(tokens: [
            schemaContractID,
            schemaContractDigest,
            admissionContractDigest,
            fingerprint.datasetDigest,
            heldOutEvidenceDigest,
            heldOutEvaluationDigest,
            proposal.sourceIdentityDigest,
            proposal.proposalComputationDigest,
            baselineDigest,
        ] + calibrationNames.map { "calibration:\($0)" }
            + heldOutNames.map { "held_out:\($0)" }
            + excludedNames.map { "excluded:\($0)" }
            + comparisons.flatMap(comparisonTokens)
            + admissions.flatMap(admissionTokens))

        return ManualLearningHoldoutValidationReport(
            schemaContractID: schemaContractID,
            schemaContractDigest: schemaContractDigest,
            admissionContractDigest: admissionContractDigest,
            sourceDatasetDigest: fingerprint.datasetDigest,
            heldOutEvidenceDigest: heldOutEvidenceDigest,
            heldOutEvaluationDigest: heldOutEvaluationDigest,
            calibrationTrainIDs: calibrationNames,
            heldOutTrainIDs: heldOutNames,
            excludedTrainIDs: excludedNames,
            calibrationProposal: proposal,
            baselineSettingsDigest: baselineDigest,
            baselineSettingsEntries: baselineSnapshot?.entries ?? [],
            comparisons: comparisons,
            admissions: admissions,
            reportDigest: digest
        )
    }

    private struct SupportKey: Hashable {
        let trainID: String
        let isiIndex: Int
    }

    private static func runDetector(
        dataset: SpikeDataset,
        manualThresholdProfile: ManualThresholdProfile,
        configuration: ManualLearningHoldoutValidationConfiguration,
        buildCommit: String
    ) -> ClassicAnchorDetectionRun {
        HybridPatternDetectionFramework.run(
            dataset: dataset,
            bandSettings: configuration.bandSettings,
            qualitySettings: configuration.qualitySettings,
            refractoryAction: configuration.refractoryAction,
            stateTuning: configuration.stateTuning,
            detectorParameters: configuration.detectorParameters,
            manualThresholdProfile: manualThresholdProfile,
            useAdaptiveV2Canonicalization: configuration.useAdaptiveV2Canonicalization,
            manualThresholdScope: configuration.manualThresholdScope,
            buildCommit: buildCommit
        )
    }

    private static func predictedSupport(
        family: ManualPatternLearningFamily,
        run: ClassicAnchorDetectionRun,
        dataset: SpikeDataset
    ) -> Set<SupportKey> {
        let tracks: Set<ClassicAnchorSemanticTrack> = switch family {
        case .burstFamily, .pause: [.event, .gap]
        case .tonic, .highFrequencyTonic, .highFrequencySpiking: [.state]
        }
        let support: [SupportKey] = run.eventAnnotations(
            in: dataset,
            tracks: tracks
        ).flatMap { annotation -> [SupportKey] in
            guard predictedFamily(annotation.label) == family else { return [] }
            return annotation.automaticSupportISIIndices.map {
                SupportKey(trainID: annotation.trainID, isiIndex: $0)
            }
        }
        return Set(support)
    }

    private static func assessedTruth(
        family: ManualPatternLearningFamily,
        row: CanonicalManualISILabelRow
    ) -> Bool? {
        if row.otherLabel != nil { return false }
        let label: ManualAnnotationLabel?
        switch family {
        case .burstFamily, .pause:
            label = row.eventLabel
        case .tonic, .highFrequencyTonic, .highFrequencySpiking:
            label = row.stateLabel
        }
        guard let label else { return nil }
        return manualFamily(label) == family
    }

    private static func manualFamily(
        _ label: ManualAnnotationLabel
    ) -> ManualPatternLearningFamily? {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst:
            return .burstFamily
        case .tonic:
            return .tonic
        case .highFrequencyTonic:
            return .highFrequencyTonic
        case .highFrequencySpiking:
            return .highFrequencySpiking
        case .pause:
            return .pause
        case .other, .notBurst:
            return nil
        }
    }

    private static func predictedFamily(
        _ label: ClassicAnchorLabel
    ) -> ManualPatternLearningFamily? {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
            return .burstFamily
        case .tonic:
            return .tonic
        case .highFrequencyTonic:
            return .highFrequencyTonic
        case .highFrequencySpiking:
            return .highFrequencySpiking
        case .pause:
            return .pause
        case .profile, .reject:
            return nil
        }
    }

    private static func subset(
        _ dataset: CanonicalScientificDataset,
        trainIDs: Set<ScientificSpikeTrainID>
    ) -> CanonicalScientificDataset {
        let trains = dataset.spikeTrains.filter { trainIDs.contains($0.semanticID) }
        let groups = dataset.eventScopeGroups.compactMap { group -> CanonicalEventScopeGroup? in
            let references = group.spikeTrainReferences.filter(trainIDs.contains)
            guard !references.isEmpty else { return nil }
            return CanonicalEventScopeGroup(
                semanticID: group.semanticID,
                timeBasis: group.timeBasis,
                spikeTrainReferences: references,
                eventDefinitions: group.eventDefinitions
            )
        }
        return CanonicalScientificDataset(
            recordingSegment: dataset.recordingSegment,
            activityMode: dataset.activityMode,
            spikeTrains: trains,
            eventScopeGroups: groups,
            scientificAttributeDefinitions: dataset.scientificAttributeDefinitions
        )
    }

    private static func displayDataset(
        from dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint
    ) -> SpikeDataset {
        SpikeDataset(
            name: dataset.recordingSegment.semanticID.semanticID.canonicalText,
            sourceDescription: "manual-learning holdout \(fingerprint.datasetDigest)",
            trains: dataset.spikeTrains.map { train in
                SpikeTrain(
                    name: train.semanticID.semanticID.canonicalText,
                    timestampsSec: train.rawTimestamps.map {
                        Double($0.microseconds) / 1_000_000
                    },
                    duplicateTimestampPolicy: .errorKeep
                )
            }
        )
    }

    private static func heldOutDecisionDigest(
        fingerprint: CanonicalScientificDatasetFingerprint,
        decisions: [CanonicalManualISILabelDecision]
    ) -> String {
        ManualLearningDigest.hex(tokens: [
            "manual_learning_holdout_raw_decisions_v1",
            fingerprint.schemaContractID,
            fingerprint.schemaContractDigest,
            fingerprint.datasetDigest,
            String(decisions.count),
        ] + decisions.flatMap {
            [
                $0.trainID.semanticID.canonicalText,
                String($0.isiIndex),
                $0.track.rawValue,
                $0.label.rawValue,
            ]
        })
    }

    private static func comparisonTokens(
        _ comparison: ManualLearningHoldoutFamilyComparison
    ) -> [String] {
        [
            "family", comparison.family.rawValue,
            "has_compatible_intervention",
            comparison.hasCompatibleIntervention ? "true" : "false",
            "assessed_train_count", String(comparison.assessedTrainCount),
            "truth_positive_train_count", String(comparison.truthPositiveTrainCount),
            "explicit_negative_train_count", String(comparison.explicitNegativeTrainCount),
            "geometry_complete_train_count", String(comparison.geometryCompleteTrainCount),
            "held_out_evaluation_digest", comparison.heldOutEvaluationDigest,
            "learned_settings_digest", comparison.learnedSettingsDigest,
        ]
            + metricTokens("baseline", comparison.baseline)
            + metricTokens("learned", comparison.learned)
            + comparison.trainComparisons.flatMap(trainComparisonTokens)
    }

    private static func trainComparisonTokens(
        _ comparison: ManualLearningHoldoutTrainComparison
    ) -> [String] {
        [
            "held_out_train", comparison.trainID,
            "geometry_complete", comparison.geometryIsComplete ? "true" : "false",
        ] + metricTokens("train_baseline", comparison.baseline)
            + metricTokens("train_learned", comparison.learned)
    }

    private static func admissionTokens(
        _ admission: ManualLearningFamilyAdmission
    ) -> [String] {
        ["admission", admission.family.rawValue, admission.disposition.rawValue]
            + admission.reasons.map(\.rawValue)
    }

    private static func metricTokens(
        _ prefix: String,
        _ value: ManualLearningDetectorMetrics
    ) -> [String] {
        func optionalDouble(_ value: Double?) -> String {
            value.map { String($0) } ?? ""
        }
        return [
            prefix,
            String(value.assessedISICount),
            String(value.truthPositiveISICount),
            String(value.predictedPositiveISICount),
            String(value.truePositiveISICount),
            String(value.falsePositiveISICount),
            String(value.falseNegativeISICount),
            String(value.trueNegativeISICount),
            optionalDouble(value.precision),
            optionalDouble(value.recall),
            optionalDouble(value.f1),
            String(value.truthSegmentCount),
            String(value.predictedSegmentCount),
            optionalDouble(value.meanBestSegmentIoU),
            optionalDouble(value.meanMatchedBoundaryErrorISI),
            String(value.falseSplitCount),
            String(value.falseMergeCount),
        ]
    }

    private static func semanticIDOrdered(
        _ lhs: ScientificSpikeTrainID,
        _ rhs: ScientificSpikeTrainID
    ) -> Bool {
        lhs.semanticID.canonicalText.utf8.lexicographicallyPrecedes(
            rhs.semanticID.canonicalText.utf8
        )
    }

    private static func semanticText(_ value: ScientificSpikeTrainID) -> String {
        value.semanticID.canonicalText
    }
}
