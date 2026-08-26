import Testing
@testable import STPDCore

@Suite("Positive tiny-ISI exact hypotheses")
struct PositiveTinyISIHypothesisResolverTests {
    @Test("H_earlier and H_later remove the correct spike and retain exact time")
    func exactVirtualSpikeRemoval() throws {
        let train = SpikeTrain(
            name: "positive-tiny",
            timestampsSec: [0, 0.1000, 0.1005, 0.2000, 0.3000]
        )
        let plan = try #require(
            PositiveTinyISIHypothesisResolver.plans(
                train: train,
                artifactThresholdSec: 0.0009
            ).only
        )
        #expect(plan.status == .eligibleForExactHypotheses)
        #expect(plan.originalISIIndex == 2)

        let earlier = try #require(
            plan.scenarios.first { $0.kind == .earlierSpikeErroneous }
        )
        let later = try #require(
            plan.scenarios.first { $0.kind == .laterSpikeErroneous }
        )
        #expect(earlier.virtualTrain.timestampsSec == [0, 0.1005, 0.2000, 0.3000])
        #expect(later.virtualTrain.timestampsSec == [0, 0.1000, 0.2000, 0.3000])
        #expect(earlier.virtualTrain.id == train.id)
        #expect(later.virtualTrain.id == train.id)
        #expect(earlier.virtualTrain.isiSec[1] == 0.1005)
        #expect(later.virtualTrain.isiSec[2] == 0.1000)
    }

    @Test("Virtual candidate geometry maps back to untouched original ISI geometry")
    func originalGeometryMapping() throws {
        let train = SpikeTrain(
            name: "mapping",
            timestampsSec: [0, 0.1000, 0.1005, 0.2000, 0.3000, 0.4000]
        )
        let plan = try #require(
            PositiveTinyISIHypothesisResolver.plans(
                train: train,
                artifactThresholdSec: 0.0009
            ).only
        )
        let earlier = try #require(
            plan.scenarios.first { $0.kind == .earlierSpikeErroneous }
        )
        let later = try #require(
            plan.scenarios.first { $0.kind == .laterSpikeErroneous }
        )

        #expect(earlier.mergedVirtualISIIndex == 1)
        #expect(earlier.originalISISpan(forVirtualStart: 1, end: 3) == 1...4)
        #expect(later.mergedVirtualISIIndex == 2)
        #expect(later.originalISISpan(forVirtualStart: 1, end: 3) == 1...4)
        #expect(earlier.containsAmbiguitySupport(startISIIndex: 1, endISIIndex: 2))
        #expect(later.containsAmbiguitySupport(startISIIndex: 2, endISIIndex: 3))
    }

    @Test("Two independent ambiguities produce all four exact joint hypotheses")
    func twoIndependentAmbiguitiesUseCartesianProduct() throws {
        let isis = [0.1, 0.1, 0.0005, 0.1, 0.1, 0.1, 0.0004, 0.1, 0.1]
        let train = SpikeTrain(name: "joint", timestampsSec: timestamps(from: isis))
        let plans = PositiveTinyISIHypothesisResolver.plans(
            train: train,
            artifactThresholdSec: 0.0009
        )
        #expect(plans.map(\.originalISIIndex) == [3, 7])
        #expect(plans.allSatisfy { $0.status == .eligibleForExactHypotheses })

        let scenarios = PositiveTinyISIHypothesisResolver.jointScenarios(
            train: train,
            plans: plans
        )
        #expect(scenarios.count == 4)
        #expect(Set(scenarios.map(\.key)).count == 4)
        #expect(scenarios.allSatisfy { $0.removedOriginalSpikeIndices.count == 2 })
        #expect(scenarios.allSatisfy {
            $0.containsAllAmbiguitySupport(startISIIndex: 1, endISIIndex: 7)
        })
        #expect(scenarios.allSatisfy {
            $0.originalISISpan(forVirtualStart: 1, end: 7) == 1...9
        })
    }

    @Test("Equal Burst envelopes do not conceal different mapped core geometry")
    func mappedBurstCoreGeometryRemainsHypothesisSpecific() throws {
        let train = SpikeTrain(
            name: "burst-core-geometry",
            timestampsSec: timestamps(from: [0.010, 0.010, 0.0005, 0.010])
        )
        let plans = PositiveTinyISIHypothesisResolver.plans(
            train: train,
            artifactThresholdSec: 0.0009
        )
        let scenarios = PositiveTinyISIHypothesisResolver.jointScenarios(
            train: train,
            plans: plans
        )
        let earlier = try #require(scenarios.first {
            $0.kindsByOriginalISIIndex[3] == .earlierSpikeErroneous
        })
        let later = try #require(scenarios.first {
            $0.kindsByOriginalISIIndex[3] == .laterSpikeErroneous
        })

        var earlierBurst = makeAuditCandidate(
            id: "earlier-burst",
            train: earlier.virtualTrain,
            finalLabel: .burst,
            start: 1,
            end: 3,
            score: 10,
            priority: 1_000
        )
        earlierBurst.burstSeedRunStartISI = earlier.mergedVirtualISIIndicesByOriginalISIIndex[3]
        earlierBurst.burstSeedRunEndISI = earlier.mergedVirtualISIIndicesByOriginalISIIndex[3]
        var laterBurst = makeAuditCandidate(
            id: "later-burst",
            train: later.virtualTrain,
            finalLabel: .burst,
            start: 1,
            end: 3,
            score: 10,
            priority: 1_000
        )
        laterBurst.burstSeedRunStartISI = later.mergedVirtualISIIndicesByOriginalISIIndex[3]
        laterBurst.burstSeedRunEndISI = later.mergedVirtualISIIndicesByOriginalISIIndex[3]

        #expect(earlier.originalISISpan(forVirtualStart: 1, end: 3) == 1...4)
        #expect(later.originalISISpan(forVirtualStart: 1, end: 3) == 1...4)
        #expect(ClassicAnchorDetectionPipeline.positiveTinyMappedBurstCoreSpan(
            scenario: earlier,
            candidate: earlierBurst
        ) == 2...3)
        #expect(ClassicAnchorDetectionPipeline.positiveTinyMappedBurstCoreSpan(
            scenario: later,
            candidate: laterBurst
        ) == 3...4)
    }

    @Test("Edge, overlapping, consecutive, and more than two ambiguities require review")
    func unsafeAmbiguityTopologiesRequireReview() {
        let edge = SpikeTrain(name: "edge", timestampsSec: [0, 0.0005, 0.1005, 0.2005])
        #expect(
            PositiveTinyISIHypothesisResolver.plans(
                train: edge,
                artifactThresholdSec: 0.0009
            ).allSatisfy { $0.status == .requiresReview }
        )

        let overlapping = SpikeTrain(
            name: "overlap",
            timestampsSec: [0, 0.1000, 0.1005, 0.1010, 0.2000]
        )
        #expect(
            PositiveTinyISIHypothesisResolver.plans(
                train: overlapping,
                artifactThresholdSec: 0.0009
            ).allSatisfy { $0.status == .requiresReview }
        )

        let three = SpikeTrain(
            name: "three",
            timestampsSec: [
                0, 0.1000, 0.1005, 0.2000, 0.3000, 0.3005,
                0.4000, 0.5000, 0.5005, 0.6000,
            ]
        )
        let plans = PositiveTinyISIHypothesisResolver.plans(
            train: three,
            artifactThresholdSec: 0.0009
        )
        #expect(plans.count == 3)
        #expect(plans.allSatisfy { $0.status == .requiresReview })
        #expect(plans.allSatisfy { $0.reviewReason == "more_than_two_positive_tiny_isi" })
    }

    @Test("Zero ISI is excluded from the positive-tiny hypothesis contract")
    func zeroISIUsesDuplicateTimestampPolicyInstead() {
        let train = SpikeTrain(name: "duplicate", timestampsSec: [0, 0.1, 0.1, 0.2])
        #expect(
            PositiveTinyISIHypothesisResolver.plans(
                train: train,
                artifactThresholdSec: 0.0009
            ).isEmpty
        )
    }

    @Test("Pre-hypothesis HFS conflict audit is retained only outside affected geometry")
    func staleHFSConflictAuditIsRemovedLocally() throws {
        let train = SpikeTrain(
            name: "audit-filter",
            timestampsSec: timestamps(from: [
                0.010, 0.010, 0.010, 0.0005, 0.010,
                0.010, 0.010, 0.010, 0.010, 0.010,
            ])
        )
        let near = try #require(makeHFSConflictAuditRow(
            train: train,
            id: "near",
            start: 1,
            end: 6,
            conflictStart: 2,
            conflictEnd: 5
        ))
        let far = try #require(makeHFSConflictAuditRow(
            train: train,
            id: "far",
            start: 8,
            end: 10,
            conflictStart: 8,
            conflictEnd: 10
        ))

        let retained = PositiveTinyISIHypothesisResolver.retainingUnaffectedHFSBurstAuditRows(
            [near, far],
            train: train,
            artifactThresholdSec: 0.0009
        )
        #expect(retained.map(\.hfsCandidateID) == ["far"])
    }

    @Test("Affected HFS conflict audit is rebuilt only from final hypothesis consensus")
    func hfsConflictAuditIsRebuiltFromFinalConsensus() throws {
        let train = SpikeTrain(
            name: "audit-rebuild",
            timestampsSec: timestamps(from: [
                0.010, 0.010, 0.010, 0.0005, 0.010,
                0.010, 0.010, 0.010, 0.010, 0.010,
            ])
        )
        let stale = try #require(makeHFSConflictAuditRow(
            train: train,
            id: "stale-pre-hypothesis",
            start: 1,
            end: 8,
            conflictStart: 2,
            conflictEnd: 5
        ))
        var consensusHFS = makeAuditCandidate(
            id: "\(train.id)-positive-tiny-consensus-4-high_frequency_spiking-1-8",
            train: train,
            finalLabel: .highFrequencySpiking,
            start: 1,
            end: 8,
            score: 11,
            priority: 1_200
        ).withDiagnosticOverride(
            gateStatus: "positive_tiny_isi_hypothesis_consensus",
            selectedForAuto: true,
            selectionStatus: "selected"
        )
        consensusHFS.hfSpikingBurstPacketLike = true
        let consensusBurst = makeAuditCandidate(
            id: "\(train.id)-positive-tiny-consensus-4-burst-2-5",
            train: train,
            finalLabel: .burst,
            start: 2,
            end: 5,
            score: 12,
            priority: 1_300
        ).withDiagnosticOverride(
            gateStatus: "positive_tiny_isi_hypothesis_consensus",
            selectedForAuto: true,
            selectionStatus: "selected"
        )

        let reconciled = PositiveTinyISIHypothesisResolver.reconcilingHFSBurstAuditRows(
            [stale],
            train: train,
            finalCandidates: [consensusHFS, consensusBurst],
            artifactThresholdSec: 0.0009,
            settings: HFSBurstArbitrationAuditSettings()
        )
        let row = try #require(reconciled.only)
        #expect(row.hfsCandidateID == consensusHFS.id)
        #expect(row.strongestBurstCandidateID == consensusBurst.id)
        #expect(row.pipelineStage == "dataset_seed_aware_positive_tiny_isi_reconciliation")
        #expect(!reconciled.contains { $0.hfsCandidateID == "stale-pre-hypothesis" })
    }

    @Test("Production pipeline materializes only two-hypothesis label and geometry consensus")
    func productionPipelineMaterializesConsensus() throws {
        let isis = [
            0.099, 0.101, 0.100, 0.100,
            0.0005,
            0.100, 0.101, 0.099, 0.100, 0.100,
        ]
        let train = SpikeTrain(
            name: "tonic-positive-tiny",
            timestampsSec: timestamps(from: isis)
        )
        let dataset = SpikeDataset(
            name: "positive-tiny-production",
            sourceDescription: "unit test",
            trains: [train]
        )

        let run = ClassicAnchorDetectionPipeline.run(dataset: dataset)
        let result = try #require(run.results.first)
        let consensus = try #require(result.candidates.first {
            $0.id.contains("-positive-tiny-consensus-")
        })
        #expect(consensus.finalLabel == .tonic)
        #expect(consensus.selectedForAuto)
        #expect(consensus.startISIIndex == 1)
        #expect(consensus.endISIIndex == isis.count)
        #expect(consensus.nISI == isis.count)
        #expect(consensus.nSpikes == isis.count + 1)
        #expect(consensus.decisionPath.contains("positive_tiny_isi_hypothesis=consensus"))
        #expect(consensus.decisionPath.contains("hypothesis_labels=tonic|tonic"))
        #expect(consensus.decisionPath.contains("hypothesis_cv_range="))
        #expect(consensus.decisionPath.contains("positive_tiny_original_state_support_revalidated=true"))
        #expect(consensus.decisionPath.contains("positive_tiny_original_state_n_support=9"))
        #expect(consensus.stateInterruptionSpans == [
            ISISpan(trainID: train.id, startISIIndex: 5, endISIIndex: 5)
        ])
        #expect(consensus.stateDirectSupportISICount == isis.count - 1)
    }

    @Test("Positive-tiny reconciliation preserves approved short Tonic support floor")
    func shortTonicSupportFloorIsNotReplacedByLongSeedMinimum() {
        let settings = StatePatternDetectorSettings(
            tonicMinSpikes: 5,
            highFrequencyTonicMinSpikes: 6
        )
        #expect(ClassicAnchorDetectionPipeline.positiveTinyOriginalSupportMeetsFamilyMinimum(
            label: .tonic,
            nSupport: 3,
            settings: settings
        ))
        #expect(!ClassicAnchorDetectionPipeline.positiveTinyOriginalSupportMeetsFamilyMinimum(
            label: .highFrequencyTonic,
            nSupport: 3,
            settings: settings
        ))
        #expect(ClassicAnchorDetectionPipeline.positiveTinyOriginalSupportMeetsFamilyMinimum(
            label: .highFrequencyTonic,
            nSupport: 5,
            settings: settings
        ))
    }

    @Test("Unsafe production ambiguity is represented as audit-only review")
    func productionPipelineRetainsUnsafeAmbiguityForReview() throws {
        let train = SpikeTrain(
            name: "edge-positive-tiny",
            timestampsSec: timestamps(from: [0.0005, 0.100, 0.100, 0.100, 0.100])
        )
        let run = ClassicAnchorDetectionPipeline.run(
            dataset: SpikeDataset(
                name: "edge-review",
                sourceDescription: "unit test",
                trains: [train]
            )
        )
        let review = try #require(run.results.first?.candidates.first {
            $0.candidateLayer == "positive_tiny_isi_hypothesis_review"
        })
        #expect(review.finalLabel == .reject)
        #expect(review.action == "audit_only")
        #expect(review.gateStatus == "positive_tiny_isi_hypothesis_review_required")
        #expect(review.decisionPath.contains("positive_tiny_isi_review_reason=edge_positive_tiny_isi"))

        let affectedFootprint = 1...2
        let candidates = try #require(run.results.first?.candidates)
        #expect(!candidates.contains { candidate in
            candidate.selectedForAuto && candidate.isEligibleForAutoSelection &&
                max(candidate.startISIIndex, affectedFootprint.lowerBound) <=
                    min(candidate.endISIIndex, affectedFootprint.upperBound)
        })
        #expect(candidates.contains { candidate in
            candidate.decisionPath.contains("positive_tiny_isi_authority_blocked_by_review=true") &&
                candidate.action == "audit_only" &&
                !candidate.selectedForAuto
        })
    }

    @Test("Two independent production ambiguities require four-way consensus")
    func productionPipelineUsesFourWayConsensus() throws {
        let isis = [
            0.099, 0.101, 0.100, 0.100,
            0.0005,
            0.100, 0.101, 0.099, 0.100,
            0.0004,
            0.100, 0.101, 0.099, 0.100,
        ]
        let train = SpikeTrain(
            name: "tonic-two-positive-tiny",
            timestampsSec: timestamps(from: isis)
        )
        let run = ClassicAnchorDetectionPipeline.run(
            dataset: SpikeDataset(
                name: "two-positive-tiny-production",
                sourceDescription: "unit test",
                trains: [train]
            )
        )
        let consensus = try #require(run.results.first?.candidates.first {
            $0.id.contains("-positive-tiny-consensus-5-10-")
        })
        #expect(consensus.finalLabel == .tonic)
        #expect(consensus.selectedForAuto)
        #expect(consensus.startISIIndex == 1)
        #expect(consensus.endISIIndex == isis.count)
        #expect(consensus.decisionPath.contains("hypothesis_scenario_count=4"))
        #expect(consensus.decisionPath.contains("positive_tiny_original_state_support_revalidated=true"))
        #expect(consensus.stateInterruptionSpans == [
            ISISpan(trainID: train.id, startISIIndex: 5, endISIIndex: 5),
            ISISpan(trainID: train.id, startISIIndex: 10, endISIIndex: 10),
        ])
        #expect(consensus.stateDirectSupportISICount == isis.count - 2)
        // Three original direct-support runs remain, so each run contributes n - 1 pairs.
        #expect(consensus.stateDirectSupportAdjacentPairCount == isis.count - 5)
    }

    @Test("Two independent ambiguities can resolve as two local four-way consensuses")
    func productionPipelinePreservesIndependentLocalConsensus() throws {
        let isis = [
            0.099, 0.101, 0.100, 0.100, 0.0005, 0.100, 0.101, 0.099,
            0.500,
            0.099, 0.101, 0.100, 0.0004, 0.100, 0.101, 0.099, 0.100,
        ]
        let train = SpikeTrain(
            name: "two-local-positive-tiny",
            timestampsSec: timestamps(from: isis)
        )
        let run = ClassicAnchorDetectionPipeline.run(
            dataset: SpikeDataset(
                name: "two-local-positive-tiny-production",
                sourceDescription: "unit test",
                trains: [train]
            )
        )
        let consensuses = try #require(run.results.first?.candidates.filter {
            $0.id.contains("-positive-tiny-consensus-") && $0.finalLabel == .tonic
        })
        let left = try #require(consensuses.first {
            $0.startISIIndex <= 5 && $0.endISIIndex >= 5 && $0.endISIIndex < 9
        })
        let right = try #require(consensuses.first {
            $0.startISIIndex > 9 && $0.startISIIndex <= 13 && $0.endISIIndex >= 13
        })
        #expect(left.id.contains("-positive-tiny-consensus-5-"))
        #expect(right.id.contains("-positive-tiny-consensus-13-"))
        #expect(left.selectedForAuto)
        #expect(right.selectedForAuto)
        #expect(left.decisionPath.contains("hypothesis_scenario_count=4"))
        #expect(right.decisionPath.contains("hypothesis_scenario_count=4"))
        #expect(!run.results[0].candidates.contains {
            $0.candidateLayer == "positive_tiny_isi_hypothesis_review"
        })
    }

    @Test("More than two ambiguities produce one batch-review record")
    func productionPipelineBatchesUnsafeAmbiguities() throws {
        let isis = [
            0.100, 0.0005, 0.100, 0.100,
            0.0004, 0.100, 0.100, 0.0003, 0.100,
        ]
        let train = SpikeTrain(name: "batch-review", timestampsSec: timestamps(from: isis))
        let run = ClassicAnchorDetectionPipeline.run(
            dataset: SpikeDataset(
                name: "batch-review-production",
                sourceDescription: "unit test",
                trains: [train]
            )
        )
        let review = try #require(run.results.first?.candidates.filter {
            $0.candidateLayer == "positive_tiny_isi_hypothesis_review"
        })
        #expect(review.count == 1)
        #expect(review[0].decisionPath.contains("positive_tiny_isi_indices=2,5,8"))
        #expect(review[0].decisionPath.contains("more_than_two_positive_tiny_isi"))
    }
}

private extension Array {
    var only: Element? { count == 1 ? self[0] : nil }
}

private func timestamps(from isis: [Double]) -> [Double] {
    isis.reduce(into: [0.0]) { values, isi in
        values.append((values.last ?? 0) + isi)
    }
}

private func makeHFSConflictAuditRow(
    train: SpikeTrain,
    id: String,
    start: Int,
    end: Int,
    conflictStart: Int,
    conflictEnd: Int
) -> HFSBurstArbitrationAuditRow? {
    var hfs = makeAuditCandidate(
        id: id,
        train: train,
        finalLabel: .highFrequencySpiking,
        start: start,
        end: end,
        score: 10,
        priority: 1_000
    )
    hfs.hfSpikingBurstPacketLike = true
    let burst = makeAuditCandidate(
        id: "\(id)-burst",
        train: train,
        finalLabel: .burst,
        start: conflictStart,
        end: conflictEnd,
        score: 5,
        priority: 1_100
    )
    return HFSBurstArbitrationAudit.build(
        train: train,
        preProtectionCandidates: [hfs, burst],
        selectedEventsUsedForPacketization: [burst],
        protectedCandidates: [hfs, burst],
        finalCandidates: [hfs, burst],
        pipelineStage: "positive_tiny_filter_test"
    ).first
}

private func makeAuditCandidate(
    id: String,
    train: SpikeTrain,
    finalLabel: ClassicAnchorLabel,
    start: Int,
    end: Int,
    score: Double,
    priority: Int
) -> ClassicAnchorCandidate {
    ClassicAnchorCandidate(
        id: id,
        trainID: train.id,
        trainName: train.name,
        candidateLayer: "positive_tiny_filter_test",
        candidateClass: finalLabel.rawValue,
        finalLabel: finalLabel,
        gateStatus: "pass",
        decisionPath: "test",
        action: "accept",
        score: score,
        priority: priority,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: start,
        endISIIndex: end,
        startSpikeIndex: start,
        endSpikeIndex: end + 1,
        nISI: end - start + 1,
        nValidISI: end - start + 1,
        nSpikes: end - start + 2,
        durationSec: nil,
        intraQ10Sec: nil,
        intraQ40Sec: nil,
        intraQ50Sec: nil,
        intraQ90Sec: nil,
        intraQ95Sec: nil,
        maxIntraISISec: nil,
        meanIntraISISec: nil,
        cv: nil,
        lv: nil,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: "test",
        anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.010,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
}
