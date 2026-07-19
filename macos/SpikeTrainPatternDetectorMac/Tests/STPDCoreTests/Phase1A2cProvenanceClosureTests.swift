@testable import STPDCore
import Testing

@Test
func phase1A2cHybridTaggingPreservesRunProvenanceAndCandidateOutcomes() {
    let dataset = phase1A2cDataset()
    let bandSettings = TrainAdaptiveBandSettings(
        minValidISISec: 0.001,
        histogramBinWidthSec: 0.005
    )

    let direct = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: bandSettings,
        frameworkPolicy: HybridPatternDetectionFramework.plan.policy
    )
    let tagged = HybridPatternDetectionFramework.run(
        dataset: dataset,
        bandSettings: bandSettings
    )

    #expect(direct.datasetRerunProvenance != .empty)
    #expect(!direct.candidates.isEmpty)
    #expect(tagged.datasetRerunProvenance == direct.datasetRerunProvenance)
    #expect(phase1A2cCandidateOutcomeSignature(tagged) == phase1A2cCandidateOutcomeSignature(direct))
    #expect(tagged.candidates.allSatisfy { $0.decisionPath.contains("hybrid_framework=") })
}

@Test
func phase1A2cDatasetAppliedSummaryRecordsOriginAndSelfInclusionExactly() {
    let local = phase1A2cTrainLocalResolution(
        trainID: "provenance_target",
        burstSeedUpperSec: 0.008
    )
    #expect(local.structuralSeedSummary.origin == .trainLocal)
    #expect(local.structuralSeedSummary.datasetSummaryIncludedTargetTrain == false)

    let selfInclusive = StructuralSeedBandResolver.applyingDatasetSummary(
        to: local,
        datasetSummary: StructuralDatasetSeedSummary(
            trainCount: 2,
            seededTrainCount: 2,
            burstAnchorCount: 2,
            burstSeedUpperSec: 0.012,
            isSelfInclusive: true
        )
    )
    #expect(selfInclusive.structuralSeedSummary.origin == .datasetApplied)
    #expect(selfInclusive.structuralSeedSummary.datasetSummaryIncludedTargetTrain == true)

    let leaveOneOut = StructuralSeedBandResolver.applyingDatasetSummary(
        to: local,
        datasetSummary: StructuralDatasetSeedSummary(
            trainCount: 1,
            seededTrainCount: 1,
            burstAnchorCount: 1,
            burstSeedUpperSec: 0.010,
            isSelfInclusive: false
        )
    )
    #expect(leaveOneOut.structuralSeedSummary.origin == .datasetApplied)
    #expect(leaveOneOut.structuralSeedSummary.datasetSummaryIncludedTargetTrain == false)
}

@Test
func phase1A2cDatasetAggregationFirewallRejectsAppliedPriorReentry() {
    let local = phase1A2cTrainLocalResolution(
        trainID: "firewall_local",
        burstSeedUpperSec: 0.008
    )
    let applied = StructuralSeedBandResolver.applyingDatasetSummary(
        to: phase1A2cTrainLocalResolution(
            trainID: "firewall_applied",
            burstSeedUpperSec: 0.040
        ),
        datasetSummary: StructuralDatasetSeedSummary(
            trainCount: 3,
            seededTrainCount: 3,
            burstAnchorCount: 3,
            burstSeedUpperSec: 0.080,
            isSelfInclusive: true
        )
    )
    #expect(applied.structuralSeedSummary.origin == .datasetApplied)

    let localOnly = StructuralDatasetSeedAggregator.aggregate(resolutions: [local])
    let mixed = StructuralDatasetSeedAggregator.aggregate(resolutions: [local, applied])

    #expect(mixed.trainCount == localOnly.trainCount)
    #expect(mixed.seededTrainCount == localOnly.seededTrainCount)
    #expect(mixed.burstSeedUpperSec == localOnly.burstSeedUpperSec)
    #expect(mixed.burstBridgeUpperSec == localOnly.burstBridgeUpperSec)
}

private func phase1A2cDataset() -> SpikeDataset {
    SpikeDataset(
        name: "phase 1A.2c provenance",
        sourceDescription: "unit-test",
        trains: [
            SpikeTrain(
                name: "burst_context_a",
                timestampsSec: phase1A2cTimestamps(
                    fromISI: [0.004, 0.004, 0.004, 0.120, 0.004, 0.004, 0.004, 0.120]
                )
            ),
            SpikeTrain(
                name: "burst_context_b",
                timestampsSec: phase1A2cTimestamps(
                    fromISI: [0.005, 0.005, 0.005, 0.140, 0.005, 0.005, 0.005, 0.140]
                )
            )
        ]
    )
}

private func phase1A2cTrainLocalResolution(
    trainID: String,
    burstSeedUpperSec: Double
) -> TrainAdaptiveBandResolution {
    let train = SpikeTrain(
        name: trainID,
        timestampsSec: phase1A2cTimestamps(fromISI: Array(repeating: 0.010, count: 8))
    )
    let base = TrainAdaptiveBandResolver.resolve(train: train)
    return StructuralSeedBandResolver.attachingSummary(
        to: base,
        summary: StructuralSeedBandSummary(
            burstAnchorCount: 3,
            burstSeedUpperSec: burstSeedUpperSec,
            burstBridgeUpperSec: burstSeedUpperSec * 1.5
        )
    )
}

private func phase1A2cTimestamps(fromISI intervals: [Double]) -> [Double] {
    intervals.reduce(into: [0.0]) { timestamps, interval in
        timestamps.append((timestamps.last ?? 0) + interval)
    }
}

private func phase1A2cCandidateOutcomeSignature(
    _ run: ClassicAnchorDetectionRun
) -> [String] {
    run.candidates.map { candidate in
        [
            candidate.id,
            candidate.trainID,
            candidate.finalLabel.rawValue,
            String(candidate.startISIIndex),
            String(candidate.endISIIndex),
            candidate.selectedForAuto ? "selected" : "not_selected",
            candidate.selectionStatus
        ].joined(separator: "|")
    }.sorted()
}
