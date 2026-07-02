@testable import STPDCore
import Testing

// MARK: - Phase D2 — DatasetISIDistributionService equivalence with the real QC paths
//
// Service-only D2: proves DatasetISIDistributionService.compute reproduces the EXISTING QC-filtered
// ISI paths (TrainAdaptiveBandResolver.validISIs, DatasetISIHistogram, SpikeQualityAnalyzer) exactly,
// without touching any detector. No pipeline/detector wiring is exercised here (that is D2-wire).

private func d2Train(_ name: String, isis: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for isi in isis { ts.append((ts.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private func d2Close(_ a: Double?, _ b: Double?, tol: Double = 1e-9) -> Bool {
    switch (a, b) {
    case (nil, nil): return true
    case let (x?, y?): return abs(x - y) <= tol
    default: return false
    }
}

/// Three representative trains; `t-mixed` includes a sub-floor artifact ISI (0.0005) so the floor
/// filter actually drops something and equivalence is non-vacuous. Names are unique (used to match
/// histogram rows to per-train distributions).
private func d2Dataset() -> SpikeDataset {
    SpikeDataset(
        name: "d2",
        sourceDescription: "d2-fixture",
        trains: [
            d2Train("t-fast", isis: [0.004, 0.006, 0.005, 0.007, 0.004, 0.009, 0.006, 0.005]),
            d2Train("t-mixed", isis: [0.0005, 0.002, 0.02, 0.05, 0.011, 0.03, 0.008]),
            d2Train("t-slow", isis: [0.05, 0.12, 0.09, 0.2, 0.15]),
        ]
    )
}

// 1 — Equivalence with TrainAdaptiveBandResolver.validISIs (detector/band path, minValidISISec).
@Test
func datasetISIDistributionServiceMatchesAdaptiveBandResolverValidISIs() {
    let floor = 0.001
    let dataset = d2Dataset()
    let dist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: floor)

    #expect(dist.trainDistributions.count == dataset.trains.count)
    // Service maps dataset.trains directly, so zip is order-aligned by construction.
    for (train, td) in zip(dataset.trains, dist.trainDistributions) {
        let resolverValid = TrainAdaptiveBandResolver.validISIs(train: train, minValidISISec: floor).sorted()
        #expect(td.validISIValuesSec == resolverValid)            // identical valid set (same source ISIs)
        #expect(td.validISICount == resolverValid.count)
        let resolution = TrainAdaptiveBandResolver.resolve(
            train: train, settings: TrainAdaptiveBandSettings(minValidISISec: floor)
        )
        #expect(td.validISICount == resolution.validISICount)     // matches the resolver's own count
    }
}

// 2 — Equivalence with DatasetISIHistogram per-train quantile rows + pooled valid count (QC/display
// path, artifactThresholdSec). Rows are matched by train name to be robust to any ordering.
@Test
func datasetISIDistributionServiceMatchesDatasetISIHistogramRows() {
    let floor = 0.001
    let dataset = d2Dataset()
    let quality = SpikeQualitySettings(artifactThresholdSec: floor)
    let dist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: floor)
    let histogram = DatasetISIHistogram.summarize(
        dataset: dataset, qualitySettings: quality, binWidthSec: 0.005
    )

    #expect(histogram.trainRows.count == dist.trainDistributions.count)
    for td in dist.trainDistributions {
        let row = histogram.trainRows.first { $0.trainName == td.trainName }
        #expect(row != nil)
        guard let row else { continue }
        #expect(td.validISICount == row.validISICount)
        #expect(d2Close(td.quantiles.minSec, row.minISISec))
        #expect(d2Close(td.quantiles.q10, row.q10ISISec))
        #expect(d2Close(td.quantiles.q25, row.q25ISISec))
        #expect(d2Close(td.quantiles.q50, row.medianISISec))
        #expect(d2Close(td.quantiles.q75, row.q75ISISec))
        #expect(d2Close(td.quantiles.q90, row.q90ISISec))
        #expect(d2Close(td.quantiles.maxSec, row.maxISISec))
    }
    #expect(dist.pooledValidISICount == histogram.totalValidISICount)
}

// 3 — Equivalence with SpikeQualityAnalyzer valid-ISI count, min, and median. The analyzer's median
// helper matches type-7 q50 exactly (even: mean of the two central values; odd: the middle value).
@Test
func datasetISIDistributionServiceMatchesSpikeQualityAnalyzer() {
    let floor = 0.001
    let dataset = d2Dataset()
    let quality = SpikeQualitySettings(artifactThresholdSec: floor)
    let dist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: floor)

    for (train, td) in zip(dataset.trains, dist.trainDistributions) {
        let q = SpikeQualityAnalyzer.quality(for: train, settings: quality)
        #expect(td.validISICount == q.validISICount)
        #expect(d2Close(td.quantiles.minSec, q.minValidISISec))
        #expect(d2Close(td.quantiles.q50, q.medianISISec))
    }
}

// 4 — Threshold-divergence characterization: an ISI in [0.0009, 0.001) is KEPT at the QC/display
// floor (0.0009) but DROPPED at the detector/band floor (0.001). Pins that the two paths may differ.
@Test
func datasetISIDistributionServiceHonorsInclusionFloorDivergence() {
    // First ISI is exactly 0.00095 (a timestamp diff from 0), then normal ISIs.
    let train = SpikeTrain(name: "divergent", timestampsSec: [0.0, 0.00095, 0.02095, 0.05095])
    let dataset = SpikeDataset(name: "d", sourceDescription: "d", trains: [train])

    let atQC = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.0009)
    let atBand = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.001)
    let qc = atQC.trainDistributions[0]
    let band = atBand.trainDistributions[0]

    #expect(qc.validISIValuesSec.contains(0.00095))       // kept at the 0.0009 floor
    #expect(!band.validISIValuesSec.contains(0.00095))    // dropped at the 0.001 floor
    #expect(qc.validISICount == band.validISICount + 1)
    // The dropped ISI is bucketed as a below-floor artifact, not silently lost.
    #expect(band.exclusionSummary.belowArtifactFloorCount == 1)
    #expect(qc.exclusionSummary.belowArtifactFloorCount == 0)
}

// 5 — Degenerate trains: 0-spike (isiSec == []) and 1-spike (isiSec == [nil]) produce empty,
// crash-free distributions; the structural leading placeholder is never counted; dataset-level
// pooled / train-balanced summaries stay sane (only real trains contribute).
@Test
func datasetISIDistributionServiceHandlesDegenerateTrains() {
    let zeroSpike = SpikeTrain(name: "zero", timestampsSec: [])   // isiSec == []
    let oneSpike = SpikeTrain(name: "one", timestampsSec: [0.5])  // isiSec == [nil]
    let real = d2Train("real", isis: [0.01, 0.02, 0.03])
    let dataset = SpikeDataset(name: "d", sourceDescription: "d", trains: [zeroSpike, oneSpike, real])

    let dist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.001)
    #expect(dist.trainDistributions.count == 3)

    for td in dist.trainDistributions.prefix(2) {   // zero-spike + one-spike
        #expect(td.validISICount == 0)
        #expect(td.rawISICount == 0)                        // no real ISI slots
        #expect(td.exclusionSummary.totalExcluded == 0)     // placeholder is NOT an exclusion
    }
    // Only the real train contributes to dataset-level summaries.
    #expect(dist.contributingTrainCount == 1)
    #expect(dist.pooledValidISICount == 3)
    #expect(dist.trainBalancedQuantiles?.count == 1)        // one contributing train
}
