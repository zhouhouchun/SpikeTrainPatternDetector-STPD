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
public struct ManualLearningHoldoutValidationConfiguration: Hashable, Sendable {
    public let bandSettings: TrainAdaptiveBandSettings
    public let qualitySettings: SpikeQualitySettings
    public let refractoryAction: ClassicAnchorRefractoryAction
    public let stateTuning: StatePatternDetectorTuning
    public let detectorParameters: PatternDetectionParameterSettings
    public let useAdaptiveV2Canonicalization: Bool

    public init(
        bandSettings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        refractoryAction: ClassicAnchorRefractoryAction = .warnOnly,
        stateTuning: StatePatternDetectorTuning = StatePatternDetectorTuning(),
        detectorParameters: PatternDetectionParameterSettings = .defaults,
        useAdaptiveV2Canonicalization: Bool = false
    ) {
        self.bandSettings = bandSettings
        self.qualitySettings = qualitySettings
        self.refractoryAction = refractoryAction
        self.stateTuning = stateTuning
        self.detectorParameters = detectorParameters
        self.useAdaptiveV2Canonicalization = useAdaptiveV2Canonicalization
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
}

public struct ManualLearningHoldoutFamilyComparison: Hashable, Sendable {
    public let family: ManualPatternLearningFamily
    public let baseline: ManualLearningDetectorMetrics
    public let learned: ManualLearningDetectorMetrics

    public var f1Delta: Double? {
        guard let baseline = baseline.f1, let learned = learned.f1 else { return nil }
        return learned - baseline
    }

    public var meanBestSegmentIoUDelta: Double? {
        guard let baseline = baseline.meanBestSegmentIoU,
              let learned = learned.meanBestSegmentIoU else { return nil }
        return learned - baseline
    }

    public var falseSplitDelta: Int { learned.falseSplitCount - baseline.falseSplitCount }
    public var falseMergeDelta: Int { learned.falseMergeCount - baseline.falseMergeCount }
}

public struct ManualLearningHoldoutValidationReport: Hashable, Sendable {
    public let schemaContractID: String
    public let sourceDatasetDigest: String
    public let calibrationTrainIDs: [String]
    public let heldOutTrainIDs: [String]
    public let excludedTrainIDs: [String]
    public let calibrationProposal: ManualPatternLearningProposal
    public let baselineSettingsDigest: String
    public let learnedSettingsDigest: String
    public let comparisons: [ManualLearningHoldoutFamilyComparison]
    public let reportDigest: String

    public var assessedFamilyCount: Int {
        comparisons.lazy.filter { $0.baseline.assessedISICount > 0 }.count
    }
}

/// Pure support-level and event/state-segment metrics. The calculator deliberately issues no
/// automatic "pass" verdict: precision/recall, geometry, and fragmentation may move in different
/// directions, and that scientific trade-off must remain visible.
public enum ManualLearningHoldoutMetricCalculator {
    public static func compare(
        family: ManualPatternLearningFamily,
        points: [ManualLearningHoldoutEvaluationPoint]
    ) -> ManualLearningHoldoutFamilyComparison {
        ManualLearningHoldoutFamilyComparison(
            family: family,
            baseline: metrics(points: points, prediction: \.baselinePredictedPositive),
            learned: metrics(points: points, prediction: \.learnedPredictedPositive)
        )
    }

    private static func metrics(
        points: [ManualLearningHoldoutEvaluationPoint],
        prediction: KeyPath<ManualLearningHoldoutEvaluationPoint, Bool>
    ) -> ManualLearningDetectorMetrics {
        let unique = canonicalPoints(points)
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

        let truthSegments = segments(unique.filter(\.truthIsPositive))
        let predictedSegments = segments(unique.filter { $0[keyPath: prediction] })
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

/// End-to-end, label-leakage-safe validation. It learns once from calibration trains, runs the
/// detector twice on held-out raw timestamps, and compares only manually reviewed held-out rows.
/// Neither detector output is fed back into feature extraction or threshold learning.
public enum ManualLearningHoldoutValidator {
    public static let schemaContractID = "manual_learning_train_holdout_validation_v1"

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
            manualThresholdProfile: .automatic,
            configuration: configuration,
            buildCommit: "manual_learning_holdout_validation"
        )
        let learned = runDetector(
            dataset: detectorDataset,
            manualThresholdProfile: proposal.compatibleThresholdProposal.profile,
            configuration: configuration,
            buildCommit: "manual_learning_holdout_validation"
        )

        let comparisons = ManualPatternLearningFamily.allCases.map { family in
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
            return ManualLearningHoldoutMetricCalculator.compare(family: family, points: points)
        }

        let assigned = calibrationSet.union(heldOutSet)
        let excluded = known.subtracting(assigned).sorted(by: semanticIDOrdered)
        let calibrationNames = split.calibrationTrainIDs.map(semanticText)
        let heldOutNames = split.heldOutTrainIDs.map(semanticText)
        let excludedNames = excluded.map(semanticText)
        let baselineDigest = baseline.invocationSettingsSnapshot?.digest ?? ""
        let learnedDigest = learned.invocationSettingsSnapshot?.digest ?? ""
        let digest = ManualLearningDigest.hex(tokens: [
            schemaContractID,
            fingerprint.datasetDigest,
            proposal.sourceIdentityDigest,
            proposal.proposalComputationDigest,
            baselineDigest,
            learnedDigest,
        ] + calibrationNames.map { "calibration:\($0)" }
            + heldOutNames.map { "held_out:\($0)" }
            + excludedNames.map { "excluded:\($0)" }
            + comparisons.flatMap(comparisonTokens))

        return ManualLearningHoldoutValidationReport(
            schemaContractID: schemaContractID,
            sourceDatasetDigest: fingerprint.datasetDigest,
            calibrationTrainIDs: calibrationNames,
            heldOutTrainIDs: heldOutNames,
            excludedTrainIDs: excludedNames,
            calibrationProposal: proposal,
            baselineSettingsDigest: baselineDigest,
            learnedSettingsDigest: learnedDigest,
            comparisons: comparisons,
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
            manualThresholdScope: .allTrains,
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

    private static func comparisonTokens(
        _ comparison: ManualLearningHoldoutFamilyComparison
    ) -> [String] {
        ["family", comparison.family.rawValue]
            + metricTokens("baseline", comparison.baseline)
            + metricTokens("learned", comparison.learned)
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
