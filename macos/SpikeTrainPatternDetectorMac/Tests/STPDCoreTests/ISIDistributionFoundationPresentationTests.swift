@testable import STPDCore
import Testing

// MARK: - Distribution-first foundation presentation model tests (pure; no UI).

private func presTrain(_ name: String, isis: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for isi in isis { ts.append((ts.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private let presBurst = [0.003, 0.0032, 0.0035, 0.003, 0.0033]
private let presTonic = [0.040, 0.052, 0.045, 0.058, 0.048, 0.055, 0.043, 0.050, 0.047, 0.053]

private func presDataset() -> SpikeDataset {
    SpikeDataset(name: "pres", sourceDescription: "unit-test", trains: [
        presTrain("bimodal", isis: presBurst + presTonic),
        presTrain("tonic-only", isis: presTonic),
    ])
}

// 1 — end-to-end `from(...)` with no run distribution computes the fallback and shapes rows.
@Test
func presentationFromDatasetFallbackComputesAndShapes() {
    let dataset = presDataset()
    let p = ISIDistributionFoundationPresentation.from(
        dataset: dataset, runDistribution: nil, minimumValidISISec: 0.001)

    #expect(p.source == .computedFromDataset)
    #expect(p.datasetName == "pres")
    #expect(p.floorSec == 0.001)
    #expect(p.perTrain.count == 2)                                  // one row per train
    #expect(p.contributingTrainCount == 2)
    #expect(p.pooled.validISICount == p.pooledValidISICount)
    #expect(p.pooled.q50Sec != nil)
    #expect(p.trainBalanced != nil)
    #expect(p.trainBalanced?.validISICount == 2)                   // contributing-train count
    // Per-train row carries identity + count.
    let bimodal = p.perTrain.first { $0.label == "bimodal" }
    #expect(bimodal != nil)
    #expect(bimodal?.trainID != nil)
    #expect((bimodal?.validISICount ?? 0) == presBurst.count + presTonic.count)
}

// 2 — the bimodal dataset yields D3 burst + tonic dataset-scope interval rows with dataset provenance.
@Test
func presentationSurfacesDerivedDatasetIntervals() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)

    let families = Set(p.datasetIntervals.map(\.family))
    #expect(families.contains(.burst))
    #expect(families.contains(.tonic))
    for row in p.datasetIntervals {
        #expect(row.scope == .dataset)
        #expect(row.provenanceOrigin == .datasetSupported)         // dataset-scope priors propagate-eligible
        #expect(row.mayPropagateToDataset == true)
        #expect(row.isValid == true)
        #expect(row.lowerSec <= row.upperSec)
    }
    // Burst row carries a bridge; tonic does not.
    let burst = p.datasetIntervals.first { $0.family == .burst }
    #expect(burst?.bridgeUpperSec != nil)
    let tonic = p.datasetIntervals.first { $0.family == .tonic }
    #expect(tonic?.bridgeUpperSec == nil)
}

// 3 — a supplied run distribution is preferred over recompute, and marks the source.
@Test
func presentationPrefersRunDistributionWhenProvided() {
    let dataset = presDataset()
    let runDist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.001)
    let p = ISIDistributionFoundationPresentation.from(
        dataset: dataset, runDistribution: runDist, minimumValidISISec: 0.001)

    #expect(p.source == .detectionRun)
    #expect(p.pooledValidISICount == runDist.pooledValidISICount)
    #expect(p.perTrain.count == runDist.trainDistributions.count)
}

// 4 — `make(...)` maps quantiles and interval provenance faithfully.
@Test
func presentationMakeMapsQuantilesAndProvenance() {
    let dataset = presDataset()
    let dist = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: 0.001)
    let derived = ModeISIIntervalDeriver.derive(datasetDistribution: dist, minimumValidISISec: 0.001)
    let p = ISIDistributionFoundationPresentation.make(
        distribution: dist, derived: derived, source: .detectionRun, floorSec: 0.001)

    #expect(p.pooled.q50Sec == dist.pooledQuantiles.q50)
    #expect(p.pooled.q90Sec == dist.pooledQuantiles.q90)
    #expect(p.pooled.minSec == dist.pooledQuantiles.minSec)
    // Interval rows preserve provenance permission flags from the derived intervals.
    if let derivedBurst = derived.dataset.burst,
       let rowBurst = p.datasetIntervals.first(where: { $0.family == .burst }) {
        #expect(rowBurst.mayPropagateToDataset == derivedBurst.provenance.mayPropagateToDataset)
        #expect(rowBurst.maySelectFinalLabel == derivedBurst.provenance.maySelectFinalLabel)
        #expect(rowBurst.isAuditOnly == derivedBurst.provenance.isAuditOnly)
        #expect(rowBurst.sourceStatistic == derivedBurst.provenance.sourceStatistic)
    }
}

// 5 — empty dataset is safe: zero counts, no intervals, no crash.
@Test
func presentationHandlesEmptyDatasetSafely() {
    let empty = SpikeDataset(name: "empty", sourceDescription: "unit-test", trains: [])
    let p = ISIDistributionFoundationPresentation.from(
        dataset: empty, runDistribution: nil, minimumValidISISec: 0.001)

    #expect(p.perTrain.isEmpty)
    #expect(p.pooledValidISICount == 0)
    #expect(p.contributingTrainCount == 0)
    #expect(p.datasetIntervals.isEmpty)
    #expect(p.pooled.q50Sec == nil)
}

// 6 — interval rows expose the tonic acceptance band (wider than the core), for UI display.
@Test
func presentationExposesTonicAcceptanceBand() {
    let p = ISIDistributionFoundationPresentation.from(
        dataset: presDataset(), runDistribution: nil, minimumValidISISec: 0.001)
    guard let tonic = p.datasetIntervals.first(where: { $0.family == .tonic }) else {
        #expect(Bool(false), "expected a tonic interval row"); return
    }
    #expect(tonic.acceptanceLowerSec != nil)
    #expect(tonic.acceptanceUpperSec != nil)
    #expect((tonic.acceptanceLowerSec ?? .infinity) <= tonic.lowerSec)   // core ⊆ acceptance
    #expect((tonic.acceptanceUpperSec ?? -.infinity) >= tonic.upperSec)
}
