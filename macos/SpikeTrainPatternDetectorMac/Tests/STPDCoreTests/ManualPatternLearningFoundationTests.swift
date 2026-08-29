import Testing
@testable import STPDCore

@Suite("Manual pattern learning foundation")
struct ManualPatternLearningFoundationTests {
    @Test("Partial evidence snapshot preserves unknown rows, dual tracks, QC, negatives, and identity")
    func snapshotIsDualTrackAndDeterministic() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 0, 12_000, 15_000, 20_000, 30_000],
        ])
        let trainID = try #require(context.trainIDs["unit_a"])
        let decisions = [
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: 1, track: .state, label: .highFrequencySpiking
            ),
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: 1, track: .event, label: .highFrequencyBurst
            ),
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: 3, track: .other, label: .other
            ),
        ]
        let negatives = [
            ManualLearningNegativeDecision(
                trainID: trainID, isiIndex: 1, target: .burstFamily
            ),
        ]

        let first = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900,
            negativeDecisions: negatives
        )
        let reordered = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: Array(decisions.reversed())
            ),
            minimumValidISIMicroseconds: 900,
            negativeDecisions: Array(negatives.reversed())
        )

        #expect(first.digest == reordered.digest)
        #expect(first.digest.count == 64)
        #expect(first.schemaContractDigest ==
            "89a35cfa35ee108bc503218632db78dcf07896efaf378ca2e6c8dd48482aa338")
        #expect(first.digest ==
            "4c374410ca829b512d5f8446be9b8dc033eb020dfda18c8d5ffaa3c3cfca0aeb")
        #expect(first.rows.count == 6)
        #expect(first.rows[0].stateLabel == .highFrequencySpiking)
        #expect(first.rows[0].eventLabel == .highFrequencyBurst)
        #expect(first.rows[0].negativeTargets == [.burstFamily])
        #expect(first.rows[0].conflicts == [.positiveAndNegativeBurstFamily])
        #expect(first.rows[1].quality == .nonPositive)
        #expect(first.rows[2].otherLabel == .other)
        #expect(first.rows[3].isUnknown)
    }

    @Test("Segment features exclude embedded events from state support without inventing adjacency")
    func segmentFeaturesRespectOverlaysAndRealAdjacency() throws {
        var intervals = (0..<60).map { Int64(8_000 + ($0 % 10) * 1_000) }
        for index in 9...17 { intervals[index] = 10_000 }
        let context = try makeContext(intervalsByTrain: ["unit_a": intervals])
        let trainID = try #require(context.trainIDs["unit_a"])
        var decisions: [CanonicalManualISILabelDecision] = []
        for index in 10...18 {
            decisions.append(CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: index, track: .state, label: .tonic
            ))
        }
        for index in 13...14 {
            decisions.append(CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: index, track: .event, label: .burst
            ))
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )

        let first = ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        let second = ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        let tonic = try #require(first.records.first { $0.label == .tonic })
        let burst = try #require(first.records.first { $0.label == .burst })

        #expect(first.sourceIdentityDigest == second.sourceIdentityDigest)
        #expect(first.featureComputationDigest == second.featureComputationDigest)
        #expect(first.schemaContractDigest ==
            "019b3b996c603ad7a0caf621702f7bd55019f89d259d04c091dcce82f9e163b5")
        #expect(first.records.count == 2)
        #expect(tonic.rawISICount == 9)
        #expect(tonic.rawEnvelopeSpikeCount == 10)
        #expect(tonic.validISICount == 9)
        #expect(tonic.directSupportISICount == 7)
        #expect(tonic.directSupportSpikeCount == 9)
        #expect(tonic.envelopeDurationMicroseconds == 90_000)
        #expect(tonic.directSupportDurationMicroseconds == 70_000)
        #expect(tonic.excludedEmbeddedEventISICount == 2)
        #expect(tonic.directSupportSpanCount == 2)
        #expect(tonic.regularity.adjacentPairCount == 5)
        #expect(tonic.regularity.mmStanding == .insufficient)
        #expect(tonic.regularity.cvStanding == .computable)
        #expect(tonic.regularity.cv2LVStanding == .computable)
        #expect(tonic.regularity.cv == 0)
        #expect(tonic.regularity.cv2 == 0)
        #expect(tonic.regularity.lv == 0)
        #expect(tonic.overlayEventCounts == [
            ManualPatternOverlayCount(label: .burst, isiCount: 2),
        ])
        #expect(tonic.trainRelative.percentileStanding == .computable)
        #expect(tonic.trainRelative.robustLogZStanding == .computable)
        #expect(tonic.trainRelative.trainReferenceISICount == 51)
        #expect(tonic.trainRelative.medianPercentile != nil)
        #expect(tonic.trainRelative.medianRobustLogZ != nil)

        #expect(burst.rawISICount == 2)
        #expect(burst.rawEnvelopeSpikeCount == 3)
        #expect(burst.directSupportISICount == 2)
        #expect(burst.excludedEmbeddedEventISICount == 0)
        #expect(burst.regularity.mmStanding == .computable)
        #expect(burst.regularity.cvStanding == .insufficient)
        #expect(burst.regularity.mm == 1)
    }

    @Test("The minimum-valid-ISI boundary is exact and not silently rounded")
    func minimumValidISIBoundaryIsExact() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [899, 900],
        ])
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: []
            ),
            minimumValidISIMicroseconds: 900
        )

        #expect(snapshot.rows.map(\.quality) == [.belowMinimum, .valid])
    }

    @Test("Snapshot construction fails closed at every public identity and index boundary")
    func snapshotValidationFailsClosed() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 20_000],
        ])
        let other = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 21_000],
        ])
        let trainID = try #require(context.trainIDs["unit_a"])
        let unknownTrainID = ScientificSpikeTrainID(
            try ScientificSemanticID(validating: "unknown_unit")
        )
        let validDraft = CanonicalManualISILabelDraft(
            canonicalFingerprint: context.fingerprint,
            decisions: []
        )

        #expect(throws: ManualLearningEvidenceSnapshotError.invalidMinimumValidISI(-1)) {
            try ManualLearningEvidenceSnapshotBuilder.build(
                dataset: context.dataset,
                fingerprint: context.fingerprint,
                draft: validDraft,
                minimumValidISIMicroseconds: -1
            )
        }
        #expect(throws:
            ManualLearningEvidenceSnapshotError.suppliedFingerprintDoesNotMatchDataset
        ) {
            try ManualLearningEvidenceSnapshotBuilder.build(
                dataset: context.dataset,
                fingerprint: other.fingerprint,
                draft: CanonicalManualISILabelDraft(
                    canonicalFingerprint: other.fingerprint,
                    decisions: []
                ),
                minimumValidISIMicroseconds: 900
            )
        }
        #expect(throws: CanonicalManualISILabelReviewError.canonicalFingerprintMismatch) {
            try ManualLearningEvidenceSnapshotBuilder.build(
                dataset: context.dataset,
                fingerprint: context.fingerprint,
                draft: CanonicalManualISILabelDraft(
                    canonicalFingerprint: other.fingerprint,
                    decisions: []
                ),
                minimumValidISIMicroseconds: 900
            )
        }
        #expect(throws: ManualLearningEvidenceSnapshotError.unknownNegativeTrain(unknownTrainID)) {
            try ManualLearningEvidenceSnapshotBuilder.build(
                dataset: context.dataset,
                fingerprint: context.fingerprint,
                draft: validDraft,
                minimumValidISIMicroseconds: 900,
                negativeDecisions: [ManualLearningNegativeDecision(
                    trainID: unknownTrainID, isiIndex: 1, target: .burstFamily
                )]
            )
        }
        for invalidIndex in [0, 3] {
            #expect(throws: ManualLearningEvidenceSnapshotError.invalidNegativeISIIndex(
                trainID: trainID,
                isiIndex: invalidIndex
            )) {
                try ManualLearningEvidenceSnapshotBuilder.build(
                    dataset: context.dataset,
                    fingerprint: context.fingerprint,
                    draft: validDraft,
                    minimumValidISIMicroseconds: 900,
                    negativeDecisions: [ManualLearningNegativeDecision(
                        trainID: trainID, isiIndex: invalidIndex, target: .burstFamily
                    )]
                )
            }
        }
    }

    @Test("QC-invalid intervals stay in geometry but split direct-support metrics")
    func invalidIntervalsSplitMetrics() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 10_000, 0, 10_000, 10_000, 10_000],
        ])
        let trainID = try #require(context.trainIDs["unit_a"])
        let decisions = (1...6).map {
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: $0, track: .state, label: .tonic
            )
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        let feature = try #require(
            ManualPatternSegmentFeatureExtractor.extract(from: snapshot).records.first
        )

        #expect(feature.rawISICount == 6)
        #expect(feature.validISICount == 5)
        #expect(feature.excludedQCISICount == 1)
        #expect(feature.directSupportSpanCount == 2)
        #expect(feature.regularity.adjacentPairCount == 3)
        #expect(feature.regularity.cvStanding == .computable)
        #expect(feature.regularity.cv2LVStanding == .insufficient)
        #expect(feature.regularity.cv2 == nil)
        #expect(feature.regularity.lv == nil)
    }

    @Test("Train-relative features use an exact leave-segment-out reference")
    func trainRelativeReferenceExcludesCurrentSegment() throws {
        let intervals = Array(repeating: Int64(10_000), count: 5)
            + (1...30).map { Int64($0 * 1_000) }
        let context = try makeContext(intervalsByTrain: ["unit_a": intervals])
        let trainID = try #require(context.trainIDs["unit_a"])
        let decisions = (1...5).map {
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: $0, track: .state, label: .tonic
            )
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        let feature = try #require(
            ManualPatternSegmentFeatureExtractor.extract(from: snapshot).records.first
        )

        #expect(feature.trainRelative.percentileStanding == .computable)
        #expect(feature.trainRelative.robustLogZStanding == .computable)
        #expect(feature.trainRelative.trainReferenceISICount == 30)
        let percentile = try #require(feature.trainRelative.medianPercentile)
        #expect(abs(percentile - (9.5 / 30.0)) < 1e-12)
        #expect(feature.trainRelative.medianRobustLogZ != nil)
    }

    @Test("Twenty-nine reference ISIs remain below the declared minimum")
    func trainRelativeReferenceMinimumIsExact() throws {
        let intervals = Array(repeating: Int64(10_000), count: 5)
            + (1...29).map { Int64($0 * 1_000) }
        let context = try makeContext(intervalsByTrain: ["unit_a": intervals])
        let trainID = try #require(context.trainIDs["unit_a"])
        let decisions = (1...5).map {
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: $0, track: .state, label: .tonic
            )
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        let feature = try #require(
            ManualPatternSegmentFeatureExtractor.extract(from: snapshot).records.first
        )

        #expect(feature.trainRelative.trainReferenceISICount == 29)
        #expect(feature.trainRelative.percentileStanding == .insufficientReference)
        #expect(feature.trainRelative.robustLogZStanding == .insufficientReference)
        #expect(feature.trainRelative.medianPercentile == nil)
        #expect(feature.trainRelative.medianRobustLogZ == nil)
    }

    @Test("Degenerate train-reference scale does not masquerade as a robust z score")
    func zeroMADHasIndependentStanding() throws {
        let intervals = Array(repeating: Int64(10_000), count: 5)
            + Array(repeating: Int64(20_000), count: 30)
        let context = try makeContext(intervalsByTrain: ["unit_a": intervals])
        let trainID = try #require(context.trainIDs["unit_a"])
        let decisions = (1...5).map {
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: $0, track: .state, label: .tonic
            )
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        let feature = try #require(
            ManualPatternSegmentFeatureExtractor.extract(from: snapshot).records.first
        )

        #expect(feature.trainRelative.percentileStanding == .computable)
        #expect(feature.trainRelative.robustLogZStanding == .degenerateScale)
        #expect(feature.trainRelative.medianPercentile == 0)
        #expect(feature.trainRelative.medianRobustLogZ == nil)
    }

    @Test("Short MM never joins direct-support islands across an embedded event")
    func shortMMRequiresOneContiguousSpan() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 11_000, 80_000, 9_000, 10_000],
        ])
        let trainID = try #require(context.trainIDs["unit_a"])
        var decisions = (1...5).map {
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: $0, track: .state, label: .tonic
            )
        }
        decisions.append(CanonicalManualISILabelDecision(
            trainID: trainID, isiIndex: 3, track: .event, label: .pause
        ))
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        let tonic = try #require(
            ManualPatternSegmentFeatureExtractor.extract(from: snapshot).records.first {
                $0.label == .tonic
            }
        )

        #expect(tonic.directSupportISICount == 4)
        #expect(tonic.directSupportSpanCount == 2)
        #expect(tonic.regularity.mmStanding == .insufficient)
        #expect(tonic.regularity.mm == nil)
    }

    @Test("Burst-family conflict does not invalidate the independent HFS state track")
    func targetedConflictPreservesStateEvidence() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": Array(repeating: Int64(10_000), count: 6),
        ])
        let trainID = try #require(context.trainIDs["unit_a"])
        var decisions = (1...6).map {
            CanonicalManualISILabelDecision(
                trainID: trainID,
                isiIndex: $0,
                track: .state,
                label: .highFrequencySpiking
            )
        }
        for index in 3...4 {
            decisions.append(CanonicalManualISILabelDecision(
                trainID: trainID,
                isiIndex: index,
                track: .event,
                label: .highFrequencyBurst
            ))
        }
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900,
            negativeDecisions: [
                ManualLearningNegativeDecision(
                    trainID: trainID, isiIndex: 3, target: .burstFamily
                ),
            ]
        )
        let features = ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        let hfs = try #require(features.records.first { $0.label == .highFrequencySpiking })
        let hfb = try #require(features.records.first { $0.label == .highFrequencyBurst })

        #expect(hfs.excludedConflictISICount == 0)
        #expect(hfs.excludedEmbeddedEventISICount == 2)
        #expect(hfs.directSupportISICount == 4)
        #expect(hfs.hasUsableDirectSupport)
        #expect(hfb.excludedConflictISICount == 1)
        #expect(hfb.directSupportISICount == 1)
        #expect(hfb.hasUsableDirectSupport)
    }

    @Test("A fully event-covered state reports missing direct support rather than zero duration")
    func noDirectSupportIsExplicit() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": Array(repeating: Int64(10_000), count: 5),
        ])
        let trainID = try #require(context.trainIDs["unit_a"])
        var decisions = (1...5).map {
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: $0, track: .state, label: .tonic
            )
        }
        decisions.append(contentsOf: (1...5).map {
            CanonicalManualISILabelDecision(
                trainID: trainID, isiIndex: $0, track: .event, label: .burst
            )
        })
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: decisions
            ),
            minimumValidISIMicroseconds: 900
        )
        let tonic = try #require(
            ManualPatternSegmentFeatureExtractor.extract(from: snapshot).records.first {
                $0.label == .tonic
            }
        )

        #expect(tonic.directSupportISICount == 0)
        #expect(tonic.directSupportSpikeCount == 0)
        #expect(tonic.directSupportDurationMicroseconds == nil)
        #expect(tonic.directSupportDurationStanding == .insufficientSupport)
        #expect(!tonic.hasUsableDirectSupport)
        #expect(tonic.regularity.mmStanding == .insufficient)
        #expect(tonic.regularity.cvStanding == .insufficient)
        #expect(tonic.regularity.cv2LVStanding == .insufficient)
    }

    private func makeContext(
        intervalsByTrain: [String: [Int64]]
    ) throws -> (
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        trainIDs: [String: ScientificSpikeTrainID]
    ) {
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
