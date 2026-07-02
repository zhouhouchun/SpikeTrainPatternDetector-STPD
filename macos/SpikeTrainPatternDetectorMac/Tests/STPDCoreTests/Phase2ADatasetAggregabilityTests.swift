@testable import STPDCore
import Testing

// MARK: - Phase 2A — dataset-aggregation safety firewall (aggregator-level unit tests)
//
// Weak-derived burst seeds, structural-window tonic fills, and audit-only pause priors are
// kept train-local and MUST NOT enter the DATASET seed aggregate. These tests pin the firewall
// at `StructuralDatasetSeedAggregator.aggregate()` independently of detector math — they build
// train-local summaries with explicit per-family aggregability flags and assert that a
// non-aggregatable family's band never appears in the dataset aggregate (so it can never be
// applied leave-one-out to reshape another train). Train-local behavior is unchanged.
//
// The core prior-propagation test (Phase 0 §8.1) is RED before the aggregator firewall and
// GREEN after it: before the filter, the aggregator is strength-blind and a weak burst band
// leaks into the dataset aggregate.

private func p2aTimestamps(repeating isi: Double, count: Int) -> [Double] {
    var v = [0.0]
    for _ in 0..<count { v.append((v.last ?? 0) + isi) }
    return v
}

private func p2aResolution(trainID: String, summary: StructuralSeedBandSummary) -> TrainAdaptiveBandResolution {
    let train = SpikeTrain(name: trainID, timestampsSec: p2aTimestamps(repeating: 0.010, count: 8))
    let base = TrainAdaptiveBandResolver.resolve(train: train)
    // attachingSummary preserves the summary (origin .trainLocal + aggregability flags).
    return StructuralSeedBandResolver.attachingSummary(to: base, summary: summary)
}

// PRIOR-PROPAGATION TEST (Phase 0 §8.1) — red before firewall, green after.
@Test
func weakDerivedBurstSeed_isExcludedFromDatasetAggregate() {
    // Train A: a train-local burst band, flagged weak-derived (not dataset-aggregatable).
    let weakA = p2aResolution(trainID: "A_weak", summary: StructuralSeedBandSummary(
        burstAnchorCount: 3,
        burstSeedUpperSec: 0.030,
        burstBridgeUpperSec: 0.045,
        isBurstSeedDatasetAggregatable: false
    ))
    // Train B: tonic only, no burst evidence.
    let tonicB = p2aResolution(trainID: "B_tonic", summary: StructuralSeedBandSummary(
        tonicAnchorCount: 5,
        tonicSeedLowerSec: 0.020,
        tonicSeedUpperSec: 0.060
    ))

    let agg = StructuralDatasetSeedAggregator.aggregate(resolutions: [weakA, tonicB])
    // A's weak-derived burst band must NOT enter the dataset aggregate.
    #expect(agg.burstSeedUpperSec == nil)
    #expect(agg.burstBridgeUpperSec == nil)

    // Leave-one-out excluding B leaves only A (non-aggregatable) → still no burst band to
    // apply back to B. This is the "does not reshape train B's resolved burst band" guarantee.
    let looExcludingB = StructuralDatasetSeedAggregator.aggregate(resolutions: [weakA, tonicB], excluding: "B_tonic")
    #expect(looExcludingB.burstSeedUpperSec == nil)
}

// POSITIVE CONTROL: an aggregatable (strong) burst seed still enters the aggregate.
@Test
func aggregatableBurstSeed_stillEntersDatasetAggregate() {
    let strongA = p2aResolution(trainID: "A_strong", summary: StructuralSeedBandSummary(
        burstAnchorCount: 3,
        burstSeedUpperSec: 0.010,
        burstBridgeUpperSec: 0.015
        // isBurstSeedDatasetAggregatable defaults true
    ))
    let agg = StructuralDatasetSeedAggregator.aggregate(resolutions: [strongA])
    #expect(agg.burstSeedUpperSec == 0.010)
}

@Test
func structuralWindowTonicSeed_isExcludedFromDatasetAggregate() {
    let windowA = p2aResolution(trainID: "A_window", summary: StructuralSeedBandSummary(
        tonicAnchorCount: 4,
        tonicSeedLowerSec: 0.020,
        tonicSeedUpperSec: 0.060,
        isTonicSeedDatasetAggregatable: false
    ))
    let agg = StructuralDatasetSeedAggregator.aggregate(resolutions: [windowA])
    #expect(agg.tonicSeedLowerSec == nil)
    #expect(agg.tonicSeedUpperSec == nil)
}

@Test
func auditOnlyPauseSeed_isExcludedFromDatasetAggregate() {
    let auditA = p2aResolution(trainID: "A_pause_audit", summary: StructuralSeedBandSummary(
        pauseAnchorCount: 3,
        pauseSeedLowerSec: 0.150,
        pauseSeedUpperSec: 0.400,
        isPauseSeedDatasetAggregatable: false
    ))
    let agg = StructuralDatasetSeedAggregator.aggregate(resolutions: [auditA])
    #expect(agg.pauseSeedLowerSec == nil)
    #expect(agg.pauseSeedUpperSec == nil)
}

// PER-FAMILY INDEPENDENCE: a summary non-aggregatable for burst but aggregatable for tonic
// must exclude the burst band yet keep the tonic band.
@Test
func datasetAggregabilityIsPerFamilyIndependent() {
    let mixedA = p2aResolution(trainID: "A_mixed", summary: StructuralSeedBandSummary(
        burstAnchorCount: 3,
        burstSeedUpperSec: 0.030,
        tonicAnchorCount: 5,
        tonicSeedLowerSec: 0.020,
        tonicSeedUpperSec: 0.060,
        isBurstSeedDatasetAggregatable: false,
        isTonicSeedDatasetAggregatable: true
    ))
    let agg = StructuralDatasetSeedAggregator.aggregate(resolutions: [mixedA])
    #expect(agg.burstSeedUpperSec == nil)      // burst family excluded
    #expect(agg.tonicSeedLowerSec == 0.020)    // tonic family kept
    #expect(agg.tonicSeedUpperSec == 0.060)
}
