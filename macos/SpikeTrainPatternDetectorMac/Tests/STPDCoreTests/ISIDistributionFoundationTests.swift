@testable import STPDCore
import Testing

// MARK: - Phase D1 — distribution-first ISI interval foundation (core value-type tests)
// Pure value types; no detector is wired to these yet.

// 1 — quantile calculation is stable on a simple known ISI array (type-7 interpolation).
@Test
func isiDistributionQuantilesAreStableOnKnownArray() {
    let q = ISIQuantiles.compute(from: [0.05, 0.01, 0.03, 0.02, 0.04])  // unsorted on purpose
    #expect(q.count == 5)
    #expect(q.minSec == 0.01)
    #expect(q.maxSec == 0.05)
    #expect(q.q25 == 0.02)   // pos = 0.25*4 = 1.0 -> values[1]
    #expect(q.q50 == 0.03)   // median
    #expect(q.q75 == 0.04)   // pos = 3.0 -> values[3]
    #expect(abs((q.meanSec ?? 0) - 0.03) < 1e-12)
    #expect(abs((q.q10 ?? 0) - 0.014) < 1e-9)   // 0.01 + 0.4*(0.02-0.01)
    #expect(abs((q.q90 ?? 0) - 0.046) < 1e-9)   // 0.04 + 0.6*(0.05-0.04)
}

// 2 — empty / single / non-finite values handled safely (no crash; quantiles nil when empty).
@Test
func isiDistributionHandlesEmptySingleAndNonFiniteSafely() {
    let empty = ISIQuantiles.compute(from: [])
    #expect(empty.count == 0)
    #expect(empty.q50 == nil)
    #expect(empty.minSec == nil)
    #expect(empty.meanSec == nil)

    let single = ISIQuantiles.compute(from: [0.02])
    #expect(single.count == 1)
    #expect(single.q50 == 0.02)
    #expect(single.minSec == 0.02 && single.maxSec == 0.02)
    #expect(single.meanSec == 0.02)

    // Non-finite values are dropped, not propagated as NaN.
    let mixed = ISIQuantiles.compute(from: [0.01, .nan, 0.03, .infinity, -.infinity])
    #expect(mixed.count == 2)
    #expect(abs((mixed.q50 ?? 0) - 0.02) < 1e-12)
    #expect(!(mixed.q50 ?? .nan).isNaN)

    // QC factory: index-0 nil is a STRUCTURAL placeholder (never counted); ISIs are kept inclusively
    // at value >= artifactThresholdSec, and only value < threshold is excluded.
    let dist = TrainISIDistribution.from(
        trainID: "t1", trainName: "t1",
        rawISISec: [nil, 0.01, .nan, 0.005, 0.03],   // leading nil == SpikeTrain.isiSec[0] placeholder
        artifactThresholdSec: 0.008, spikeCount: 6
    )
    #expect(dist.rawISICount == 4)                   // 4 real ISI slots (leading placeholder excluded)
    #expect(dist.validISICount == 2)                 // 0.01 and 0.03
    #expect(dist.excludedNonFiniteCount == 1)        // just .nan — the leading placeholder is NOT an exclusion
    #expect(dist.excludedArtifactCount == 1)         // 0.005 < 0.008
    #expect(dist.validISIValuesSec == [0.01, 0.03])  // sorted
    #expect(dist.spikeCount == 6)
}

// 2b — exclusion summary partitions QC drops into mutually-exclusive reason buckets, and the
// legacy aggregate counts remain consistent roll-ups over them. Index-0 nil is the structural
// placeholder and is NOT counted; a nil at index > 0 is a genuine missing-ISI exclusion.
@Test
func isiDistributionExclusionSummaryBreaksDownReasons() {
    let dist = TrainISIDistribution.from(
        trainID: "t", trainName: "t",
        //         ^placeholder ^nil@idx1  ^nan ^inf   ^neg  ^zero ^below ^ok  ^ok
        rawISISec: [nil,         nil,      .nan, .infinity, -0.5, 0.0, 0.004, 0.02, 0.05],
        artifactThresholdSec: 0.008
    )
    let s = dist.exclusionSummary
    #expect(s.nilCount == 1)                 // ONE non-leading nil (index 0 is the structural placeholder)
    #expect(s.nonFiniteCount == 2)           // .nan + .infinity
    #expect(s.nonPositiveCount == 2)         // -0.5 and 0.0 (zero == duplicate ts, not artifact)
    #expect(s.belowArtifactFloorCount == 1)  // 0.004 < 0.008 (strict; 0.008 itself would be kept)
    #expect(s.totalExcluded == 6)
    #expect(dist.validISICount == 2)         // 0.02, 0.05 survive
    #expect(dist.validISIValuesSec == [0.02, 0.05])
    #expect(dist.rawISICount == 8)           // 8 real slots (leading placeholder excluded)

    // Buckets are exhaustive over real slots: excluded + valid == rawISICount.
    #expect(s.totalExcluded + dist.validISICount == dist.rawISICount)

    // Legacy roll-ups stay consistent (nil+non-finite ; non-positive+below-floor).
    #expect(dist.excludedNonFiniteCount == 3)
    #expect(dist.excludedArtifactCount == 3)
    #expect(s.nonFiniteOrNilCount == 3)
    #expect(s.artifactOrNonPositiveCount == 3)

    // Empty summary is the neutral element.
    #expect(ISIExclusionSummary.none.totalExcluded == 0)
}

// 2c — the two QC corrections asserted crisply: (a) index-0 nil is a structural placeholder that is
// never an exclusion (1-spike train == [nil]; 0-spike train == []); (b) the artifact threshold is
// INCLUSIVE — an ISI exactly equal to the threshold is KEPT, matching SpikeQualityAnalyzer.
@Test
func isiDistributionAppliesStructuralPlaceholderAndInclusiveThreshold() {
    // (a) A lone leading placeholder (1-spike train) yields no ISI slots and NO exclusions.
    let onlyPlaceholder = TrainISIDistribution.from(
        trainID: "s", trainName: "s", rawISISec: [nil], artifactThresholdSec: 0.001
    )
    #expect(onlyPlaceholder.rawISICount == 0)
    #expect(onlyPlaceholder.validISICount == 0)
    #expect(onlyPlaceholder.exclusionSummary.totalExcluded == 0)
    #expect(onlyPlaceholder.exclusionSummary.nilCount == 0)   // leading nil is NOT a nil-exclusion

    // A 0-spike train (isiSec == []) is likewise empty and exclusion-free.
    let empty = TrainISIDistribution.from(
        trainID: "z", trainName: "z", rawISISec: [], artifactThresholdSec: 0.001
    )
    #expect(empty.rawISICount == 0 && empty.validISICount == 0)
    #expect(empty.exclusionSummary.totalExcluded == 0)

    // (b) Inclusive lower bound: value EXACTLY equal to the threshold is KEPT; strictly below dropped.
    let boundary = TrainISIDistribution.from(
        trainID: "b", trainName: "b",
        rawISISec: [nil, 0.001, 0.0009999, 0.002],   // 0.001 == threshold -> kept
        artifactThresholdSec: 0.001
    )
    #expect(boundary.validISIValuesSec == [0.001, 0.002])            // 0.001 KEPT (>= threshold)
    #expect(boundary.exclusionSummary.belowArtifactFloorCount == 1)  // only 0.0009999 (< threshold)
    #expect(boundary.exclusionSummary.totalExcluded == 1)
    #expect(boundary.rawISICount == 3)
}

// 3 — dataset pooled vs train-balanced statistics differ when train sizes differ.
@Test
func datasetISIDistributionPooledDiffersFromTrainBalancedWhenSizesDiffer() {
    let big = TrainISIDistribution(trainID: "big", trainName: "big",
                                   validISIValuesSec: Array(repeating: 0.01, count: 100))
    let small = TrainISIDistribution(trainID: "small", trainName: "small",
                                     validISIValuesSec: Array(repeating: 1.0, count: 4))
    let dataset = DatasetISIDistribution(datasetName: "d", trainDistributions: [big, small])

    #expect(dataset.contributingTrainCount == 2)
    #expect(dataset.pooledValidISICount == 104)
    // Pooled is dominated by the large train.
    #expect(abs((dataset.pooledQuantiles.q50 ?? 0) - 0.01) < 1e-12)
    // Train-balanced weights each train equally: mean of (0.01, 1.0) = 0.505.
    #expect(abs((dataset.trainBalancedQuantiles?.q50 ?? 0) - 0.505) < 1e-9)
    // They genuinely differ.
    #expect(dataset.pooledQuantiles.q50 != dataset.trainBalancedQuantiles?.q50)
    // Balanced count is the number of contributing trains, not the pooled ISI count.
    #expect(dataset.trainBalancedQuantiles?.count == 2)
}

// 4 — provenance flags correctly distinguish origins and permissions.
@Test
func intervalProvenanceFlagsDistinguishOriginsAndPermissions() {
    let local = IntervalProvenance.trainLocalDerived(sourceStatistic: "q90")
    #expect(local.origin == .trainLocalDerived)
    #expect(local.mayPropagateToDataset == false)     // train-local stays local (Phase 2A spirit)
    #expect(local.maySelectFinalLabel == true)
    #expect(local.isAuditOnly == false)

    let dataset = IntervalProvenance.datasetSupported(sourceStatistic: "dataset_q80")
    #expect(dataset.origin == .datasetSupported)
    #expect(dataset.mayPropagateToDataset == true)

    let propagated = IntervalProvenance.datasetPropagated(sourceStatistic: "loo_prior")
    #expect(propagated.mayPropagateToDataset == false)  // applied prior must not re-enter aggregate
    #expect(propagated.maySelectFinalLabel == true)

    let manual = IntervalProvenance.manualAdjusted(sourceStatistic: "manual_hard_gate")
    #expect(manual.origin == .manualAdjusted)
    #expect(manual.maySelectFinalLabel == true)
    #expect(manual.mayPropagateToDataset == false)      // default conservative

    let audit = IntervalProvenance.auditOnly(origin: .trainLocalDerived, sourceStatistic: "diagnostic")
    #expect(audit.isAuditOnly == true)
    #expect(audit.maySelectFinalLabel == false)         // audit-only never selects
    #expect(audit.mayPropagateToDataset == false)       // ...and never propagates
}

// 5 — ModeISIInterval validates lower <= upper: marks invalid explicitly and can reject.
@Test
func modeISIIntervalValidatesLowerUpperAndMarksInvalid() {
    let valid = ModeISIInterval(
        family: .tonic, lowerSec: 0.02, upperSec: 0.06,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "q20_q80")
    )
    #expect(valid.isValid == true)
    #expect(valid.contains(0.04) == true)
    #expect(valid.contains(0.10) == false)

    // lower > upper -> marked invalid (not crashing).
    let inverted = ModeISIInterval(
        family: .burst, lowerSec: 0.06, upperSec: 0.02,
        scope: .trainLocal, provenance: .defaultFallback()
    )
    #expect(inverted.isValid == false)
    #expect(inverted.contains(0.04) == false)   // invalid never contains

    // negative lower -> invalid.
    let negative = ModeISIInterval(
        family: .pause, lowerSec: -0.01, upperSec: 0.5,
        scope: .trainLocal, provenance: .defaultFallback()
    )
    #expect(negative.isValid == false)

    // validated(...) rejects invalid (returns nil) and accepts valid.
    #expect(ModeISIInterval.validated(
        family: .burst, lowerSec: 0.06, upperSec: 0.02,
        scope: .trainLocal, provenance: .defaultFallback()) == nil)
    #expect(ModeISIInterval.validated(
        family: .burst, lowerSec: 0.001, upperSec: 0.010,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "peak")) != nil)
}

// 6 — overlap descriptor distinguishes within-train mode overlap from cross-train band overlap.
@Test
func modeISIIntervalOverlapDescriptorDistinguishesWithinAndCrossTrain() {
    let burst = ModeISIInterval(family: .burst, lowerSec: 0.001, upperSec: 0.010,
                                scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "peak"))
    let tonic = ModeISIInterval(family: .tonic, lowerSec: 0.008, upperSec: 0.060,
                                scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "q20_q80"))
    let within = ISIOverlapDescriptor.between(burst, tonic, kind: .withinTrainModeOverlap)
    #expect(within.kind == .withinTrainModeOverlap)
    #expect(within.hasOverlap == true)
    #expect(abs(within.overlapWidthSec - 0.002) < 1e-9)   // [0.008, 0.010]

    let datasetBurst = ModeISIInterval(family: .burst, lowerSec: 0.001, upperSec: 0.005,
                                       scope: .dataset, provenance: .datasetSupported(sourceStatistic: "dataset_q50"))
    let datasetTonic = ModeISIInterval(family: .tonic, lowerSec: 0.020, upperSec: 0.060,
                                       scope: .dataset, provenance: .datasetSupported(sourceStatistic: "dataset_q20"))
    let cross = ISIOverlapDescriptor.between(datasetBurst, datasetTonic, kind: .crossTrainBandOverlap)
    #expect(cross.kind == .crossTrainBandOverlap)
    #expect(cross.hasOverlap == false)   // disjoint bands
}

// 9 — acceptance band: core ⊆ acceptance validity, containsAcceptance vs contains, effective fallback.
@Test
func modeISIIntervalAcceptanceBandValidityAndContainment() {
    // Valid: acceptance wider than core on both sides.
    let iv = ModeISIInterval(
        family: .tonic, lowerSec: 0.02, upperSec: 0.06,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"),
        acceptanceLowerSec: 0.008, acceptanceUpperSec: 0.17)
    #expect(iv.isValid)
    #expect(iv.effectiveAcceptanceLowerSec == 0.008)
    #expect(iv.effectiveAcceptanceUpperSec == 0.17)
    #expect(iv.contains(0.04) && !iv.contains(0.10))            // core
    #expect(iv.containsAcceptance(0.10))                        // inside acceptance, outside core
    #expect(iv.containsAcceptance(0.009) && !iv.contains(0.009))
    #expect(!iv.containsAcceptance(0.20))                       // beyond acceptance

    // nil acceptance ⇒ effective == core; containsAcceptance == contains (backward-compatible).
    let noAccept = ModeISIInterval(
        family: .tonic, lowerSec: 0.02, upperSec: 0.06,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"))
    #expect(noAccept.effectiveAcceptanceLowerSec == 0.02 && noAccept.effectiveAcceptanceUpperSec == 0.06)
    #expect(noAccept.containsAcceptance(0.04) == noAccept.contains(0.04))
    #expect(!noAccept.containsAcceptance(0.10))

    // Invalid: acceptance NARROWER than core (violates core ⊆ acceptance).
    #expect(ModeISIInterval.validated(
        family: .tonic, lowerSec: 0.02, upperSec: 0.06,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"),
        acceptanceLowerSec: 0.03) == nil)                      // acceptanceLower > lowerSec
    #expect(ModeISIInterval.validated(
        family: .tonic, lowerSec: 0.02, upperSec: 0.06,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"),
        acceptanceUpperSec: 0.05) == nil)                      // acceptanceUpper < upperSec
}
