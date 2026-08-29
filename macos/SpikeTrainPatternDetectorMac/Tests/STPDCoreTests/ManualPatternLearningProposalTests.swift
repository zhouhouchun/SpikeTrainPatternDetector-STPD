import Testing
@testable import STPDCore

@Suite("Manual pattern learning proposal")
struct ManualPatternLearningProposalTests {
    @Test("Segments vote within train before trains receive equal weight")
    func aggregationIsTrainBalanced() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 10_000, 90_000, 12_000, 12_000],
            "unit_b": [30_000, 30_000],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...2, .event, .burst),
                ("unit_a", 4...5, .event, .burst),
                ("unit_b", 1...2, .event, .burst),
            ]
        )
        let proposal = try proposal(context: context, decisions: decisions)
        let burst = try #require(proposal.summaries.first { $0.family == .burstFamily })

        // unit_a votes 11 ms (median of its two segments); unit_b votes 30 ms. Their equal-train
        // median is 20.5 ms. Pooling the three segment medians would incorrectly yield 12 ms.
        #expect(burst.isiMedianMicroseconds.contributingSegmentCount == 3)
        #expect(burst.isiMedianMicroseconds.contributingTrainCount == 2)
        #expect(burst.isiMedianMicroseconds.trainBalancedMedian == 20_500)
        #expect(burst.segmentCount == 3)
        #expect(burst.trainCount == 2)
        #expect(burst.standing == .provisionalTwoTrains)
        #expect(burst.validation.heldOutTrainCount == 2)
        #expect(burst.validation.standing == .failed)
    }

    @Test("Single-train HFS evidence remains report-only")
    func mapsSafeFieldsAndKeepsSingleTrainHFSReportOnly() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [
                8_000, 10_000, 90_000,
                12_000, 14_000, 90_000,
                35_000, 40_000, 45_000, 50_000, 55_000, 90_000,
                30_000, 32_000, 34_000, 36_000, 38_000, 90_000,
                100_000, 90_000, 110_000, 90_000, 120_000,
                6_000, 7_000, 8_000, 9_000,
                90_000,
                7_000, 8_000, 9_000, 10_000,
            ],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...2, .event, .burst),
                ("unit_a", 4...5, .event, .longBurst),
                ("unit_a", 7...11, .state, .tonic),
                ("unit_a", 13...17, .state, .tonic),
                ("unit_a", 19...19, .event, .pause),
                ("unit_a", 21...21, .event, .pause),
                ("unit_a", 23...23, .event, .pause),
                ("unit_a", 24...27, .state, .highFrequencySpiking),
                ("unit_a", 29...32, .state, .highFrequencySpiking),
            ]
        )
        let result = try proposal(context: context, decisions: decisions)
        let mappedFamilies = Set(result.compatibleThresholdProposal.contributions.map(\.family))
        let hfs = try #require(result.summaries.first {
            $0.family == .highFrequencySpiking
        })

        #expect(mappedFamilies == ["burst", "tonic", "pause"])
        #expect(result.compatibleThresholdProposal.profile.hfs.minDurationSec == .automatic)
        #expect(result.compatibleThresholdProposal.profile.hfs.minSpikes == .automatic)
        #expect(hfs.standing == .exploratorySingleTrain)
        #expect(hfs.directDurationMicroseconds.trainBalancedMedian != nil)
        #expect(hfs.directSpikeCount.trainBalancedMedian == 5)
        #expect(!result.diagnostics.contains {
            $0.code == .hfsMinimumSupportGatesRequireConfirmation
        })
        #expect(result.applicableFamilies == [.burstFamily, .tonic, .pause])
        #expect(result.hasApplicableThresholds)
    }

    @Test("Cross-train HFS evidence proposes confirmed conservative support minima")
    func hfsMinimumSupportLearningIsBoundedAndAuditable() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [
                8_000, 8_000, 8_000, 8_000, 8_000, 8_000,
                100_000,
                9_000, 9_000, 9_000, 9_000, 9_000, 9_000, 9_000,
            ],
            "unit_b": [
                10_000, 10_000, 10_000, 10_000,
                10_000, 10_000, 10_000, 10_000,
            ],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...6, .state, .highFrequencySpiking),
                ("unit_a", 8...14, .state, .highFrequencySpiking),
                ("unit_b", 1...8, .state, .highFrequencySpiking),
            ]
        )
        let result = try proposal(context: context, decisions: decisions)
        let hfs = try #require(result.summaries.first {
            $0.family == .highFrequencySpiking
        })
        let profile = result.compatibleThresholdProposal.profile.hfs

        #expect(hfs.usableSegmentCount == 3)
        #expect(hfs.usableTrainCount == 2)
        #expect(hfs.directSupportISICount == 21)
        #expect(profile.minSpikes.mode == .hardGate)
        #expect(profile.minSpikes.value == 7)
        #expect(profile.minDurationSec.mode == .hardGate)
        #expect(profile.minDurationSec.valueSec == 0.0555)
        #expect(result.compatibleThresholdProposal.contributions.contains {
            $0.family == "hfs" && $0.field == "min_spikes" && $0.valueCount == 7
        })
        #expect(result.compatibleThresholdProposal.contributions.contains {
            $0.family == "hfs" && $0.field == "min_duration_sec"
                && $0.valueSec == 0.0555
        })
        #expect(result.diagnostics.contains {
            $0.code == .hfsMinimumSupportGatesRequireConfirmation
                && $0.severity == .warning
        })
        #expect(result.applicableFamilies == [.highFrequencySpiking])
        #expect(result.warnings(relevantTo: [.highFrequencySpiking]).contains {
            $0.code == .hfsMinimumSupportGatesRequireConfirmation
        })
    }

    @Test("Insufficient evidence is visible and never manufactures a threshold")
    func insufficientEvidenceDoesNotMap() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [100_000, 50_000, 110_000],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...1, .event, .pause),
                ("unit_a", 3...3, .event, .pause),
            ]
        )
        let result = try proposal(context: context, decisions: decisions)
        let pause = try #require(result.summaries.first { $0.family == .pause })

        #expect(pause.segmentCount == 2)
        #expect(pause.standing == .insufficient)
        #expect(result.compatibleThresholdProposal.profile.pause.isiLower == .automatic)
        #expect(result.diagnostics.contains {
            $0.code == .insufficientFamilyEvidence && $0.family == .pause
        })
    }

    @Test("Annotated but unusable segments remain visible in the audit summary")
    func unusableSegmentsAreNotReportedAsUnlabeled() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 10_000, 10_000, 10_000, 10_000],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...5, .state, .tonic),
                ("unit_a", 1...5, .event, .burst),
            ]
        )
        let result = try proposal(context: context, decisions: decisions)
        let tonic = try #require(result.summaries.first { $0.family == .tonic })

        #expect(tonic.segmentCount == 1)
        #expect(tonic.usableSegmentCount == 0)
        #expect(tonic.trainCount == 1)
        #expect(tonic.usableTrainCount == 0)
        #expect(tonic.excludedISICount == 5)
        #expect(tonic.standing == .insufficient)
        #expect(tonic.isiMedianMicroseconds.trainBalancedMedian == nil)
    }

    @Test("Soft physiological ordering is diagnosed without deleting evidence")
    func softOrderProducesWarningOnly() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [30_000, 30_000, 90_000, 32_000, 32_000, 90_000,
                       20_000, 20_000, 20_000, 20_000, 20_000, 90_000,
                       22_000, 22_000, 22_000, 22_000, 22_000],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...2, .event, .burst),
                ("unit_a", 4...5, .event, .burst),
                ("unit_a", 7...11, .state, .tonic),
                ("unit_a", 13...17, .state, .tonic),
            ]
        )
        let result = try proposal(context: context, decisions: decisions)

        #expect(result.diagnostics.contains {
            $0.code == .expectedBurstTonicOrderNotObserved
                && $0.severity == .warning
        })
        #expect(result.compatibleThresholdProposal.contributions.contains {
            $0.family == "burst"
        })
        #expect(result.compatibleThresholdProposal.contributions.contains {
            $0.family == "tonic"
        })
        #expect(result.warnings(relevantTo: [.burstFamily]).contains {
            $0.code == .expectedBurstTonicOrderNotObserved
        })
        #expect(result.warnings(relevantTo: [.tonic]).contains {
            $0.code == .expectedBurstTonicOrderNotObserved
        })
        #expect(result.warnings(relevantTo: [.pause]).isEmpty)
    }

    @Test("Proposal identity is deterministic and bound to feature source")
    func proposalIdentityIsDeterministic() throws {
        let context = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 11_000, 80_000, 12_000, 13_000],
        ])
        let decisions = try labels(
            context: context,
            specifications: [
                ("unit_a", 1...2, .event, .burst),
                ("unit_a", 4...5, .event, .burst),
            ]
        )
        let first = try proposal(context: context, decisions: decisions)
        let second = try proposal(context: context, decisions: decisions.reversed())

        #expect(first == second)
        #expect(first.schemaContractDigest == "5799e6c3df8081b13a3dc07c73916105495185356239c9ca4b8182c80aefb198")
        #expect(first.sourceIdentityDigest == "c217912774030d94faec637338fb72b446b69f11fb8d1c2dc8188e22e4eeb748")
        #expect(first.proposalComputationDigest == "a4ec3c31e4937a403dd5d74a4854d5f1ae476ca0d7d8b1669fe69e40292e82e3")
        #expect(first.sourceIdentityDigest.count == 64)
        #expect(first.proposalComputationDigest.count == 64)
    }

    @Test("Identical derived statistics from different exact sources keep distinct identities")
    func exactSourceIdentityCannotCollapse() throws {
        let firstContext = try makeContext(intervalsByTrain: [
            "unit_a": [10_000, 10_000, 80_000, 12_000, 12_000],
        ])
        let secondContext = try makeContext(intervalsByTrain: [
            "unit_b": [10_000, 10_000, 80_000, 12_000, 12_000],
        ])
        let first = try proposal(
            context: firstContext,
            decisions: labels(context: firstContext, specifications: [
                ("unit_a", 1...2, .event, .burst),
                ("unit_a", 4...5, .event, .burst),
            ])
        )
        let second = try proposal(
            context: secondContext,
            decisions: labels(context: secondContext, specifications: [
                ("unit_b", 1...2, .event, .burst),
                ("unit_b", 4...5, .event, .burst),
            ])
        )
        let firstBurst = try #require(first.summaries.first { $0.family == .burstFamily })
        let secondBurst = try #require(second.summaries.first { $0.family == .burstFamily })

        #expect(firstBurst.isiMedianMicroseconds == secondBurst.isiMedianMicroseconds)
        #expect(first.compatibleThresholdProposal == second.compatibleThresholdProposal)
        #expect(first.sourceIdentityDigest != second.sourceIdentityDigest)
        #expect(first.proposalComputationDigest != second.proposalComputationDigest)
    }

    private typealias Context = (
        dataset: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint,
        trainIDs: [String: ScientificSpikeTrainID]
    )

    private func proposal(
        context: Context,
        decisions: some Sequence<CanonicalManualISILabelDecision>
    ) throws -> ManualPatternLearningProposal {
        let snapshot = try ManualLearningEvidenceSnapshotBuilder.build(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: CanonicalManualISILabelDraft(
                canonicalFingerprint: context.fingerprint,
                decisions: Array(decisions)
            ),
            minimumValidISIMicroseconds: 900
        )
        let features = ManualPatternSegmentFeatureExtractor.extract(from: snapshot)
        return ManualPatternLearningProposalBuilder.build(from: features)
    }

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
