@testable import STPDCore
import Testing

// MARK: - Phase D2-wire — dataset ISI distribution attached to detection runs
//
// Wiring-only: ClassicAnchorDetectionPipeline.run computes a DatasetISIDistribution once (at the
// detector/band floor, bandSettings.minValidISISec) and attaches it to ClassicAnchorDetectionRun. No
// detector reads it. These tests prove the field is populated on BOTH the direct-pipeline and the
// framework paths, equals a direct DatasetISIDistributionService.compute, uses the band floor (not the
// artifact floor), and does not change detection output. D3 (deriving ModeISIIntervals) is separate.

private func d2wTrain(_ name: String, _ timestamps: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: timestamps)
}

private func d2wDataset() -> SpikeDataset {
    SpikeDataset(
        name: "d2wire",
        sourceDescription: "unit-test",
        trains: [
            d2wTrain("fast_train", [0, 0.100, 0.106, 0.112, 0.200]),
            d2wTrain("slower_train", [0, 0.300, 0.320, 0.340, 0.360, 0.860, 1.360, 1.860]),
        ]
    )
}

private func d2wBandSettings() -> TrainAdaptiveBandSettings {
    TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
}

// 1 — Direct pipeline path populates the field, and it equals a direct band-floor compute.
@Test
func datasetISIDistributionWiringPipelinePopulatesField() {
    let dataset = d2wDataset()
    let bandSettings = d2wBandSettings()
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bandSettings)

    #expect(run.datasetISIDistribution != nil)
    let expected = DatasetISIDistributionService.compute(
        dataset: dataset, minimumValidISISec: bandSettings.minValidISISec
    )
    #expect(run.datasetISIDistribution == expected)
}

// 2 — Framework path (tagRun rebuilds the run) preserves the field.
@Test
func datasetISIDistributionWiringFrameworkPreservesField() {
    let dataset = d2wDataset()
    let bandSettings = d2wBandSettings()
    let run = HybridPatternDetectionFramework.run(dataset: dataset, bandSettings: bandSettings)

    #expect(run.datasetISIDistribution != nil)
    let expected = DatasetISIDistributionService.compute(
        dataset: dataset, minimumValidISISec: bandSettings.minValidISISec
    )
    #expect(run.datasetISIDistribution == expected)
}

// 3 — Floor decision pinned: the attached distribution uses the detector/band floor
// (minValidISISec = 0.001), NOT the QC/display artifact floor (0.0009). An ISI of 0.00095 is dropped
// at the band floor but would be kept at the artifact floor, so the two computes differ.
@Test
func datasetISIDistributionWiringUsesBandFloorNotArtifactFloor() {
    let dataset = SpikeDataset(
        name: "divergent", sourceDescription: "unit-test",
        trains: [
            d2wTrain("divergent", [0.0, 0.00095, 0.02095, 0.05095, 0.09095, 0.14095]),
            d2wTrain("normal", [0, 0.100, 0.106, 0.112, 0.200]),
        ]
    )
    let bandSettings = d2wBandSettings()
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bandSettings)

    let atBandFloor = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.001)
    let atArtifactFloor = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.0009)

    #expect(run.datasetISIDistribution == atBandFloor)        // wired to the band floor
    #expect(run.datasetISIDistribution != atArtifactFloor)    // NOT the artifact floor
    // Sanity on the divergence itself: the band floor drops exactly the one 0.00095 ISI.
    #expect(atBandFloor.pooledValidISICount + 1 == atArtifactFloor.pooledValidISICount)
}

// 4 — Behavior-neutral: attaching the diagnostic field does not change detection output.
@Test
func datasetISIDistributionWiringIsBehaviorNeutral() {
    let dataset = d2wDataset()
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: d2wBandSettings())
    #expect(run.results.count == dataset.trains.count)
}
