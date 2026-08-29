import Testing
@testable import STPDCore

@Suite("Manual-learning train holdout validation")
struct ManualLearningHoldoutValidationTests {
    @Test("Effective minimum-valid-ISI conversion is exact and never silently rounded")
    func effectiveMinimumValidISIIsExact() throws {
        let exact = ManualLearningHoldoutValidationConfiguration(
            bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001234),
            qualitySettings: SpikeQualitySettings(artifactThresholdSec: 0.0009)
        )
        #expect(try exact.exactMinimumValidISIMicroseconds() == 1_234)

        let fractional = ManualLearningHoldoutValidationConfiguration(
            bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.0012345),
            qualitySettings: SpikeQualitySettings(artifactThresholdSec: 0.0009)
        )
        #expect(throws: ManualLearningHoldoutValidationError
            .minimumValidISINotExactlyRepresentable) {
            try fractional.exactMinimumValidISIMicroseconds()
        }
    }

    @Test("Support and segment metrics retain precision, geometry, and fragmentation separately")
    func metricCalculatorReportsExactSupportAndGeometry() throws {
        let truth = Set([1, 2, 3, 6, 7])
        let baseline = Set([1, 2, 6, 7, 8])
        let learned = truth
        let points = (1...8).map {
            ManualLearningHoldoutEvaluationPoint(
                trainID: "held_out",
                isiIndex: $0,
                truthIsPositive: truth.contains($0),
                baselinePredictedPositive: baseline.contains($0),
                learnedPredictedPositive: learned.contains($0)
            )
        }

        let result = ManualLearningHoldoutMetricCalculator.compare(
            family: .highFrequencySpiking,
            points: points
        )

        #expect(result.baseline.assessedISICount == 8)
        #expect(result.baseline.truePositiveISICount == 4)
        #expect(result.baseline.falsePositiveISICount == 1)
        #expect(result.baseline.falseNegativeISICount == 1)
        #expect(result.baseline.trueNegativeISICount == 2)
        #expect(abs(try #require(result.baseline.f1) - 0.8) < 1e-12)
        #expect(result.baseline.truthSegmentCount == 2)
        #expect(result.baseline.predictedSegmentCount == 2)
        #expect(abs(try #require(result.baseline.meanBestSegmentIoU) - 2.0 / 3.0) < 1e-12)
        #expect(try #require(result.baseline.meanMatchedBoundaryErrorISI) == 0.5)
        #expect(result.baseline.falseSplitCount == 0)
        #expect(result.baseline.falseMergeCount == 0)

        #expect(result.learned.truePositiveISICount == 5)
        #expect(result.learned.falsePositiveISICount == 0)
        #expect(result.learned.falseNegativeISICount == 0)
        #expect(result.learned.f1 == 1)
        #expect(result.learned.meanBestSegmentIoU == 1)
        #expect(result.learned.meanMatchedBoundaryErrorISI == 0)
        #expect(abs(try #require(result.f1Delta) - 0.2) < 1e-12)
    }

    @Test("Unreviewed ISIs are absent rather than silently treated as negatives")
    func unreviewedRowsCannotBecomeFalsePositives() {
        let points = [
            ManualLearningHoldoutEvaluationPoint(
                trainID: "held_out",
                isiIndex: 4,
                truthIsPositive: true,
                baselinePredictedPositive: false,
                learnedPredictedPositive: true
            ),
        ]

        let result = ManualLearningHoldoutMetricCalculator.compare(
            family: .tonic,
            points: points
        )

        #expect(result.baseline.assessedISICount == 1)
        #expect(result.baseline.falsePositiveISICount == 0)
        #expect(result.learned.assessedISICount == 1)
        #expect(result.learned.falsePositiveISICount == 0)
    }

    @Test("Calibration labels learn once and held-out labels only evaluate detector output")
    func endToEndValidationHasNoHeldOutLabelLeakage() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [
                8_000, 8_000, 8_000, 8_000, 8_000, 8_000,
                100_000,
                9_000, 9_000, 9_000, 9_000, 9_000, 9_000,
            ],
            "unit_b": [
                10_000, 10_000, 10_000, 10_000,
                10_000, 10_000, 10_000, 10_000,
            ],
            "unit_c": [
                11_000, 11_000, 11_000, 11_000,
                11_000, 11_000, 11_000, 11_000,
            ],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...6, .state, .highFrequencySpiking),
                ("unit_a", 8...13, .state, .highFrequencySpiking),
                ("unit_b", 1...8, .state, .highFrequencySpiking),
                ("unit_c", 1...8, .state, .highFrequencySpiking),
            ]
        )
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint,
            decisions: decisions
        )
        let split = ManualLearningHoldoutSplit(
            calibrationTrainIDs: [
                try #require(context.trainIDs["unit_a"]),
                try #require(context.trainIDs["unit_b"]),
            ],
            heldOutTrainIDs: [try #require(context.trainIDs["unit_c"])]
        )

        let first = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft,
            split: split
        )
        let second = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft,
            split: split
        )
        let hfsSummary = try #require(first.calibrationProposal.summaries.first {
            $0.family == .highFrequencySpiking
        })
        let hfsComparison = try #require(first.comparisons.first {
            $0.family == .highFrequencySpiking
        })

        // The held-out fourth HFS segment never enters the proposal.
        #expect(hfsSummary.segmentCount == 3)
        #expect(hfsSummary.usableTrainCount == 2)
        #expect(hfsSummary.directSupportISICount == 20)
        #expect(first.calibrationProposal.compatibleThresholdProposal.profile.hfs.minSpikes.mode == .hardGate)
        #expect(first.calibrationProposal.compatibleThresholdProposal.profile.hfs.minDurationSec.mode == .hardGate)
        #expect(first.calibrationTrainIDs == ["unit_a", "unit_b"])
        #expect(first.heldOutTrainIDs == ["unit_c"])
        #expect(first.excludedTrainIDs.isEmpty)
        #expect(hfsComparison.baseline.assessedISICount == 8)
        #expect(hfsComparison.baseline.truthPositiveISICount == 8)
        #expect(hfsComparison.learned.assessedISICount == 8)
        #expect(first.baselineSettingsDigest != first.learnedSettingsDigest)
        #expect(first.reportDigest == second.reportDigest)
        #expect(first.comparisons == second.comparisons)
    }

    @Test("Changing held-out labels changes evaluation only, never learning or detector settings")
    func heldOutLabelsCannotChangeLearnedInputs() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": Array(repeating: 8_000, count: 8),
            "unit_b": Array(repeating: 10_000, count: 8),
            "unit_c": Array(repeating: 11_000, count: 8),
        ])
        let calibration = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...8, .state, .highFrequencySpiking),
                ("unit_b", 1...8, .state, .highFrequencySpiking),
            ]
        )
        let heldOutHFS = try labels(
            context: context,
            specifications: [("unit_c", 1...8, .state, .highFrequencySpiking)]
        )
        let heldOutTonic = try labels(
            context: context,
            specifications: [("unit_c", 1...8, .state, .tonic)]
        )
        let split = ManualLearningHoldoutSplit(
            calibrationTrainIDs: [
                try #require(context.trainIDs["unit_a"]),
                try #require(context.trainIDs["unit_b"]),
            ],
            heldOutTrainIDs: [try #require(context.trainIDs["unit_c"])]
        )

        let hfsTruth = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: calibration + heldOutHFS
            ),
            split: split
        )
        let tonicTruth = try ManualLearningHoldoutValidator.validate(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: calibration + heldOutTonic
            ),
            split: split
        )

        #expect(hfsTruth.calibrationProposal == tonicTruth.calibrationProposal)
        #expect(hfsTruth.baselineSettingsDigest == tonicTruth.baselineSettingsDigest)
        #expect(hfsTruth.learnedSettingsDigest == tonicTruth.learnedSettingsDigest)
        #expect(hfsTruth.comparisons != tonicTruth.comparisons)
        #expect(hfsTruth.reportDigest != tonicTruth.reportDigest)
    }

    @Test("One train cannot be both calibration and held out")
    func overlappingSplitFailsClosed() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 10_000, 10_000],
            "unit_b": [20_000, 20_000, 20_000],
        ])
        let unitA = try #require(context.trainIDs["unit_a"])
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint,
            decisions: [
                CanonicalManualISILabelDecision(
                    trainID: unitA,
                    isiIndex: 1,
                    track: .event,
                    label: .pause
                ),
            ]
        )

        #expect(throws: ManualLearningHoldoutValidationError.overlappingTrainRoles([unitA])) {
            try ManualLearningHoldoutValidator.validate(
                dataset: context.dataset,
                fingerprint: context.fingerprint,
                draft: draft,
                split: ManualLearningHoldoutSplit(
                    calibrationTrainIDs: [unitA],
                    heldOutTrainIDs: [unitA]
                )
            )
        }
    }

    private typealias Context = (
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        trainIDs: [String: ScientificSpikeTrainID]
    )

    private func labels(
        context: Context,
        specifications: [(String, ClosedRange<Int>, ManualAnnotationSemanticTrack,
                          ManualAnnotationLabel)]
    ) throws -> [CanonicalManualISILabelDecision] {
        try specifications.flatMap { name, range, track, label in
            let trainID = try #require(context.trainIDs[name])
            return range.map {
                CanonicalManualISILabelDecision(
                    trainID: trainID,
                    isiIndex: $0,
                    track: track,
                    label: label
                )
            }
        }
    }

    private func makeContext(intervalsByTrain: [String: [Int64]]) throws -> Context {
        var trainIDs: [String: ScientificSpikeTrainID] = [:]
        let orderedNames = intervalsByTrain.keys.sorted()
        let trains = try orderedNames.map { name -> CanonicalSpikeTrain in
            let trainID = ScientificSpikeTrainID(try ScientificSemanticID(validating: name))
            trainIDs[name] = trainID
            var tick: Int64 = 0
            var ticks = [MicrosecondTick(microseconds: tick)]
            for interval in intervalsByTrain[name] ?? [] {
                tick += interval
                ticks.append(MicrosecondTick(microseconds: tick))
            }
            return CanonicalSpikeTrain(semanticID: trainID, rawTimestamps: ticks)
        }
        let groups = try orderedNames.map { name -> CanonicalEventScopeGroup in
            let trainID = try #require(trainIDs[name])
            return CanonicalEventScopeGroup(
                semanticID: ScientificEventScopeGroupID(
                    try ScientificSemanticID(validating: "group_\(name)")
                ),
                timeBasis: .recordingElapsed,
                spikeTrainReferences: [trainID],
                eventDefinitions: []
            )
        }
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: "recording")
                ),
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: trains,
            eventScopeGroups: groups,
            scientificAttributeDefinitions: []
        )
        return (
            dataset,
            try CanonicalScientificDatasetFingerprinter.fingerprint(dataset),
            trainIDs
        )
    }
}
