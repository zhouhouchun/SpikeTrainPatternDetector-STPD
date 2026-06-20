import STPDCore
import Testing

@Test
func datasetISIHistogramComputesRawAndTrainBalancedDistributions() {
    let first = SpikeTrain(name: "first", timestampsSec: [0, 0.002, 0.008, 0.020])
    let second = SpikeTrain(name: "second", timestampsSec: [0, 0.003, 0.017])
    let dataset = SpikeDataset(name: "synthetic", sourceDescription: "unit-test", trains: [first, second])

    let summary = DatasetISIHistogram.summarize(
        dataset: dataset,
        qualitySettings: SpikeQualitySettings(artifactThresholdSec: 0.001, refractorySuspectThresholdSec: 0.002),
        binWidthSec: 0.005,
        xMaxSec: 0.020
    )

    #expect(summary.totalValidISICount == 5)
    #expect(summary.visibleValidISICount == 5)
    #expect(summary.contributingTrainCount == 2)
    #expect(summary.bins.count == 4)
    #expect(summary.bins.map(\.rawCount) == [2, 1, 2, 0])
    #expect(isClose(summary.bins[0].rawFraction, 0.4))
    #expect(isClose(summary.bins[1].rawFraction, 0.2))
    #expect(isClose(summary.bins[2].rawFraction, 0.4))
    #expect(isClose(summary.bins[0].trainBalancedFraction, (1.0 / 3.0 + 1.0 / 2.0) / 2.0))
    #expect(isClose(summary.bins[1].trainBalancedFraction, (1.0 / 3.0 + 0.0) / 2.0))
    #expect(isClose(summary.bins[2].trainBalancedFraction, (1.0 / 3.0 + 1.0 / 2.0) / 2.0))
}

@Test
func datasetISIHistogramExcludesHardArtifactsFromEvidence() {
    let train = SpikeTrain(name: "artifact", timestampsSec: [0, 0.0005, 0.0030, 0.0100])
    let dataset = SpikeDataset(name: "synthetic", sourceDescription: "unit-test", trains: [train])

    let summary = DatasetISIHistogram.summarize(
        dataset: dataset,
        qualitySettings: SpikeQualitySettings(artifactThresholdSec: 0.001, refractorySuspectThresholdSec: 0.002),
        binWidthSec: 0.005,
        xMaxSec: 0.015
    )

    #expect(summary.totalValidISICount == 2)
    #expect(summary.artifactExcludedCount == 1)
    #expect(summary.trainRows.first?.artifactExcludedCount == 1)
    #expect(summary.bins.map(\.rawCount) == [1, 1, 0])
}

private func isClose(_ value: Double, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    abs(value - expected) <= tolerance
}
