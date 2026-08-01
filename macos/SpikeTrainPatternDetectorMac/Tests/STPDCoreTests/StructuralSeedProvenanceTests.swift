@testable import STPDCore
import Testing

@Test
func structuralSeedSummaryOriginDistinguishesTrainLocalFromDatasetApplied() {
    // A purely train-local summary is .trainLocal and carries no dataset prior.
    let local = StructuralSeedBandSummary(burstAnchorCount: 2, burstSeedUpperSec: 0.010)
    #expect(local.origin == .trainLocal)
    #expect(local.datasetSummaryIncludedTargetTrain == false)

    let train = SpikeTrain(
        name: "provenance_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.008, count: 12)
    )
    let base = TrainAdaptiveBandResolver.resolve(train: train)
    let resolutionWithLocal = StructuralSeedBandResolver.attachingSummary(to: base, summary: local)
    #expect(resolutionWithLocal.structuralSeedSummary.origin == .trainLocal)
    #expect(resolutionWithLocal.structuralSeedSummary.datasetSummaryIncludedTargetTrain == false)

    // Applying a self-inclusive dataset summary marks the resulting per-train summary
    // as dataset-applied and records that the dataset prior included this train.
    let datasetSummary = StructuralDatasetSeedSummary(
        trainCount: 3,
        seededTrainCount: 3,
        burstAnchorCount: 3,
        burstSeedUpperSec: 0.012,
        burstBridgeUpperSec: 0.018,
        isSelfInclusive: true
    )
    let applied = StructuralSeedBandResolver.applyingDatasetSummary(
        to: resolutionWithLocal,
        datasetSummary: datasetSummary
    )
    #expect(applied.structuralSeedSummary.origin == .datasetApplied)
    #expect(applied.structuralSeedSummary.datasetSummaryIncludedTargetTrain == true)
    // The two structural sources are now distinguishable from a field alone.
    #expect(resolutionWithLocal.structuralSeedSummary.origin
            != applied.structuralSeedSummary.origin)
}

@Test
func datasetSeedAggregateIsExplicitlySelfInclusive() {
    // An empty/default dataset summary is not marked self-inclusive.
    #expect(StructuralDatasetSeedSummary.empty.isSelfInclusive == false)

    let train = SpikeTrain(
        name: "provenance_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.008, count: 12)
    )
    let base = TrainAdaptiveBandResolver.resolve(train: train)

    // Aggregation over a seeded resolution is self-inclusive (no leave-one-out).
    let seeded = StructuralSeedBandResolver.attachingSummary(
        to: base,
        summary: StructuralSeedBandSummary(burstAnchorCount: 2, burstSeedUpperSec: 0.010)
    )
    #expect(StructuralDatasetSeedAggregator.aggregate(resolutions: [seeded]).isSelfInclusive == true)

    // The guard path (no seeded anchors) still considered all trains → self-inclusive.
    #expect(StructuralDatasetSeedAggregator.aggregate(resolutions: [base]).isSelfInclusive == true)
}

@Test
func pipelineReportsSelfInclusiveDatasetProvenanceWithoutChangingOutputs() {
    let tonicTrain = SpikeTrain(
        name: "provenance_tonic",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 8)
    )
    let hfsTrain = SpikeTrain(
        name: "provenance_hfs",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )
    let dataset = SpikeDataset(
        name: "provenance pipeline",
        sourceDescription: "unit-test",
        trains: [tonicTrain, hfsTrain]
    )

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    // Self-inclusion is explicitly reported on the dataset summary and the rerun provenance.
    #expect(run.datasetStructuralSeedSummary.isSelfInclusive == true)
    #expect(run.datasetRerunProvenance.initialDatasetSummary.isSelfInclusive == true)
    #expect(run.datasetRerunProvenance.finalDatasetSummary.isSelfInclusive == true)

    // Leave-one-out: the per-train applied prior never includes the target train,
    // so the run-level provenance flag is false even though the full summary above
    // remains self-inclusive for audit/display.
    #expect(run.datasetRerunProvenance.datasetSummaryIncludedTargetTrain == false)

    // Outputs still bounded: the existing fixture invariants still hold.
    #expect(run.tonicCount >= 1)
    #expect(run.highFrequencySpikingCount >= 1)
    #expect(run.selectedAutoCount >= 2)
    #expect(run.candidates.contains { $0.finalLabel == .tonic })
    #expect(run.candidates.contains { $0.finalLabel == .highFrequencySpiking })
}

@Test
func datasetAggregationFirewallExcludesDatasetAppliedSummaries() {
    // A dataset-applied (origin == .datasetApplied) summary must NOT feed dataset
    // structural aggregation; only train-local evidence is eligible.
    let local = makeTrainLocalResolution(trainID: "fw_local", burstSeedUpperSec: 0.008, burstAnchorCount: 3)
    let datasetApplied = StructuralSeedBandResolver.applyingDatasetSummary(
        to: makeTrainLocalResolution(trainID: "fw_applied", burstSeedUpperSec: 0.020, burstAnchorCount: 3),
        datasetSummary: StructuralDatasetSeedSummary(
            trainCount: 2,
            seededTrainCount: 2,
            burstAnchorCount: 2,
            burstSeedUpperSec: 0.012,
            isSelfInclusive: true
        )
    )
    #expect(datasetApplied.structuralSeedSummary.origin == .datasetApplied)

    let mixed = StructuralDatasetSeedAggregator.aggregate(resolutions: [local, datasetApplied])
    let onlyLocal = StructuralDatasetSeedAggregator.aggregate(resolutions: [local])
    #expect(mixed.burstSeedUpperSec == onlyLocal.burstSeedUpperSec)
    #expect(mixed.trainCount == onlyLocal.trainCount)
}

@Test
func pipelineNeverAppliesSelfInclusiveDatasetPriorPerTrain() {
    let trainA = SpikeTrain(name: "loo_a", timestampsSec: burstThenGapTimestamps())
    let trainB = SpikeTrain(name: "loo_b", timestampsSec: burstThenGapTimestamps(offset: 0.0005))
    let dataset = SpikeDataset(
        name: "leave one out pipeline",
        sourceDescription: "unit-test",
        trains: [trainA, trainB]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    // No train may carry a dataset prior that included itself.
    #expect(run.resolutions.allSatisfy { !$0.structuralSeedSummary.datasetSummaryIncludedTargetTrain })
    #expect(run.datasetRerunProvenance.datasetSummaryIncludedTargetTrain == false)
    // Provenance clearly reports the per-train prior is applied leave-one-out.
    #expect(run.datasetRerunProvenance.datasetPriorAppliedLeaveOneOut == true)
    // The full dataset summary is still reported self-inclusive for audit/display.
    #expect(run.datasetStructuralSeedSummary.isSelfInclusive == true)
}

@Test
func pipelineLeaveOneOutIsDeterministicAcrossReruns() {
    let trainA = SpikeTrain(name: "loo_det_a", timestampsSec: burstThenGapTimestamps())
    let trainB = SpikeTrain(name: "loo_det_b", timestampsSec: burstThenGapTimestamps(offset: 0.0005))
    let dataset = SpikeDataset(
        name: "leave one out determinism",
        sourceDescription: "unit-test",
        trains: [trainA, trainB]
    )
    let settings = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run1 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: settings)
    let run2 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: settings)

    #expect(run1.candidateCount == run2.candidateCount)
    #expect(run1.selectedAutoCount == run2.selectedAutoCount)
    for train in dataset.trains {
        #expect(run1.resolution(for: train.id)?.burstBand?.seedUpperSec
                == run2.resolution(for: train.id)?.burstBand?.seedUpperSec)
    }
}

@Test
func leaveOneOutAggregateExcludesTargetTrain() {
    let resA = makeTrainLocalResolution(trainID: "A", burstSeedUpperSec: 0.008, burstAnchorCount: 3)
    let resB = makeTrainLocalResolution(trainID: "B", burstSeedUpperSec: 0.020, burstAnchorCount: 3)

    // LOO excluding A must equal aggregating B only, and report non-self-inclusive.
    let looA = StructuralDatasetSeedAggregator.aggregate(resolutions: [resA, resB], excluding: "A")
    let onlyB = StructuralDatasetSeedAggregator.aggregate(resolutions: [resB])
    #expect(looA.burstSeedUpperSec == onlyB.burstSeedUpperSec)
    #expect(looA.isSelfInclusive == false)
    #expect(looA.trainCount == 1)

    // The full (self-inclusive) aggregate still includes both trains.
    let full = StructuralDatasetSeedAggregator.aggregate(resolutions: [resA, resB])
    #expect(full.isSelfInclusive == true)
    #expect(full.trainCount == 2)
}

@Test
func leaveOneOutAggregateWithNoOtherEvidenceHasNoAnchors() {
    // Target A has evidence; the only other train B has none.
    let resA = makeTrainLocalResolution(trainID: "A", burstSeedUpperSec: 0.008, burstAnchorCount: 3)
    let resB = makeTrainLocalResolution(trainID: "B", burstSeedUpperSec: nil, burstAnchorCount: 0)

    let looA = StructuralDatasetSeedAggregator.aggregate(resolutions: [resA, resB], excluding: "A")
    #expect(looA.hasAnyAnchor == false)
    #expect(looA.isSelfInclusive == false)

    // Single-train dataset: excluding the only train leaves nothing to aggregate.
    let looSolo = StructuralDatasetSeedAggregator.aggregate(resolutions: [resA], excluding: "A")
    #expect(looSolo.hasAnyAnchor == false)
    #expect(looSolo.trainCount == 0)
}

@Test
func pipelineBridgeExpansionUsesLeaveOneOutDatasetPrior() {
    let trainA = SpikeTrain(name: "bridge_loo_a", timestampsSec: burstThenGapTimestamps())
    let trainB = SpikeTrain(name: "bridge_loo_b", timestampsSec: burstThenGapTimestamps(offset: 0.0005))
    let dataset = SpikeDataset(
        name: "bridge leave one out pipeline",
        sourceDescription: "unit-test",
        trains: [trainA, trainB]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    // Bridge expansion applies the dataset structural prior leave-one-out per train:
    // every per-train bridge summary is non-self-inclusive.
    #expect(run.datasetRerunProvenance.bridgeExpansionPriorAppliedLeaveOneOut == true)
    // The bridge expansion summary never includes the target train.
    #expect(run.datasetRerunProvenance.bridgeExpansionSummaryIncludedTargetTrain == false)
    // With two bursty trains, each train's leave-one-out bridge prior draws on the
    // other train's evidence, so a usable (anchored) LOO bridge prior is consumed.
    #expect(run.datasetRerunProvenance.bridgeExpansionUsedDatasetSummary == true)
    // The full self-inclusive summary remains available for dataset-level audit/display.
    #expect(run.datasetRerunProvenance.initialDatasetSummary.isSelfInclusive == true)
    #expect(run.datasetStructuralSeedSummary.isSelfInclusive == true)
    // No per-train resolution carries a dataset prior that included itself.
    #expect(run.resolutions.allSatisfy { !$0.structuralSeedSummary.datasetSummaryIncludedTargetTrain })
}

@Test
func pipelineBridgeExpansionSingleTrainReceivesNoSelfDerivedPrior() {
    // Small-N regression: a single-train dataset has no "other train" evidence, so the
    // leave-one-out bridge prior is empty and bridge expansion must not receive a
    // self-derived dataset prior — even though the full self-inclusive summary, kept for
    // audit/display, still contains that train's own anchors.
    let solo = SpikeTrain(name: "bridge_solo", timestampsSec: burstThenGapTimestamps())
    let dataset = SpikeDataset(
        name: "bridge leave one out single",
        sourceDescription: "unit-test",
        trains: [solo]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    // The full self-inclusive summary still has the train's own anchors (the would-be leak)...
    #expect(run.datasetRerunProvenance.initialDatasetSummary.hasAnyAnchor == true)
    // ...yet bridge expansion consumed no self-derived prior, because excluding the only
    // train leaves no other structural evidence.
    #expect(run.datasetRerunProvenance.bridgeExpansionPriorAppliedLeaveOneOut == true)
    #expect(run.datasetRerunProvenance.bridgeExpansionSummaryIncludedTargetTrain == false)
    #expect(run.datasetRerunProvenance.bridgeExpansionUsedDatasetSummary == false)
}

@Test
func pipelineBridgeExpansionLeaveOneOutIsDeterministicAndBounded() {
    let trainA = SpikeTrain(name: "bridge_det_a", timestampsSec: burstThenGapTimestamps())
    let trainB = SpikeTrain(name: "bridge_det_b", timestampsSec: burstThenGapTimestamps(offset: 0.0005))
    let dataset = SpikeDataset(
        name: "bridge leave one out determinism",
        sourceDescription: "unit-test",
        trains: [trainA, trainB]
    )
    let settings = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run1 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: settings)
    let run2 = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: settings)

    #expect(run1.candidateCount == run2.candidateCount)
    #expect(run1.selectedAutoCount == run2.selectedAutoCount)
    #expect(run1.burstCount == run2.burstCount)
    #expect(
        run1.datasetRerunProvenance.bridgeExpansionUsedDatasetSummary
            == run2.datasetRerunProvenance.bridgeExpansionUsedDatasetSummary
    )
    #expect(
        run1.datasetRerunProvenance.bridgeExpansionPriorAppliedLeaveOneOut
            == run2.datasetRerunProvenance.bridgeExpansionPriorAppliedLeaveOneOut
    )
}

private func makeTrainLocalResolution(
    trainID: String,
    burstSeedUpperSec: Double?,
    burstAnchorCount: Int
) -> TrainAdaptiveBandResolution {
    let train = SpikeTrain(name: trainID, timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 8))
    let base = TrainAdaptiveBandResolver.resolve(train: train)
    let summary = StructuralSeedBandSummary(
        burstAnchorCount: burstAnchorCount,
        burstSeedUpperSec: burstSeedUpperSec,
        burstBridgeUpperSec: burstSeedUpperSec.map { $0 * 1.5 }
    )
    return StructuralSeedBandResolver.attachingSummary(to: base, summary: summary)
}

private func burstThenGapTimestamps(offset: Double = 0) -> [Double] {
    let isi: [Double] = [
        0.004, 0.004, 0.004, 0.004, 0.400,
        0.004, 0.004, 0.004, 0.004, 0.400,
        0.004, 0.004, 0.004, 0.004, 0.400
    ]
    var values = [offset]
    values.reserveCapacity(isi.count + 1)
    for interval in isi {
        values.append((values.last ?? 0) + interval)
    }
    return values
}

private func cumulativeTimestamps(repeating isi: Double, count: Int) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(count + 1)
    for _ in 0..<count {
        values.append((values.last ?? 0) + isi)
    }
    return values
}
