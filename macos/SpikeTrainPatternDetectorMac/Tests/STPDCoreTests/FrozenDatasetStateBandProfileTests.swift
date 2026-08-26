@testable import STPDCore
import Foundation
import Testing

private let frozenProfileFloor = 0.001

private func frozenProfileTrain(name: String, isis: [Double]) -> SpikeTrain {
    var timestamps = [0.0]
    timestamps.reserveCapacity(isis.count + 1)
    var timestamp = 0.0
    for isi in isis {
        timestamp += isi
        timestamps.append(timestamp)
    }
    return SpikeTrain(name: name, timestampsSec: timestamps)
}

private func frozenProfile(
    _ trains: [SpikeTrain],
    floor: Double = frozenProfileFloor
) -> FrozenDatasetStateBandProfile {
    FrozenDatasetStateBandProfile.compute(
        dataset: SpikeDataset(name: "fixture", sourceDescription: "test", trains: trains),
        minimumValidISISec: floor
    )
}

private func frozenClose(
    _ lhs: Double?,
    _ rhs: Double?,
    tolerance: Double = 1e-10
) -> Bool {
    switch (lhs, rhs) {
    case let (lhs?, rhs?):
        return abs(lhs - rhs) <= tolerance
    case (nil, nil):
        return true
    default:
        return false
    }
}

@Test
func frozenStateBandMergesStableWindowsAndReportsRequiredMetrics() {
    let train = frozenProfileTrain(name: "stable", isis: Array(repeating: 0.010, count: 32))
    let profile = frozenProfile([train])

    #expect(profile.trains.count == 1)
    let runs = profile.trains[0].sustainedStableRuns
    #expect(runs.count == 1)
    let run = runs[0]
    #expect(run.startISIIndex == 1)
    #expect(run.endISIIndex == 32)
    #expect(run.isiCount == 32)
    #expect(run.spikeCount == 33)
    #expect(run.sourceWindowCount == 4)
    #expect(frozenClose(run.medianISISec, 0.010))
    #expect(frozenClose(run.q10ISISec, 0.010))
    #expect(frozenClose(run.q90ISISec, 0.010))
    #expect(frozenClose(run.cv, 0))
    #expect(frozenClose(run.lv, 0))
    #expect(frozenClose(run.medianBandFraction, 1))
    #expect(frozenClose(run.adjacentRatioPassFraction, 1))
}

@Test
func frozenStateBandNeverCrossesASubFloorSlot() {
    let isis = Array(repeating: 0.010, count: 29)
        + [0.0005]
        + Array(repeating: 0.010, count: 29)
    let train = frozenProfileTrain(name: "qc-break", isis: isis)
    let runs = frozenProfile([train]).trains[0].sustainedStableRuns

    #expect(runs.count == 2)
    #expect(runs.map(\.startISIIndex) == [1, 31])
    #expect(runs.map(\.endISIIndex) == [29, 59])
    #expect(runs.allSatisfy { $0.isiCount == 29 })
}

@Test
func frozenStateBandIsScaleEquivariantAtFixedQCFloorWhenValidityMaskIsUnchanged() {
    let baseISI = Array(repeating: 0.010, count: 35)
        + Array(repeating: 0.030, count: 5)
    let base = frozenProfile([frozenProfileTrain(name: "scale", isis: baseISI)])

    for scale in [10.0, 30.0] {
        let scaled = frozenProfile([
            frozenProfileTrain(name: "scale", isis: baseISI.map { $0 * scale })
        ])

        #expect(scaled.relativeHFSAdjudicationEligibility
            == base.relativeHFSAdjudicationEligibility)
        #expect(scaled.trains[0].sustainedStableRuns.map(\.startISIIndex)
            == base.trains[0].sustainedStableRuns.map(\.startISIIndex))
        #expect(scaled.trains[0].sustainedStableRuns.map(\.endISIIndex)
            == base.trains[0].sustainedStableRuns.map(\.endISIIndex))
        #expect(scaled.trains[0].slowerStableSupport.map(\.startISIIndex)
            == base.trains[0].slowerStableSupport.map(\.startISIIndex))
        #expect(scaled.trains[0].slowerStableSupport.map(\.endISIIndex)
            == base.trains[0].slowerStableSupport.map(\.endISIIndex))
        #expect(frozenClose(
            scaled.sustainedBandCenterSec,
            base.sustainedBandCenterSec.map { $0 * scale },
            tolerance: 1e-9
        ))
        #expect(frozenClose(
            scaled.slowerStableBandCenterSec,
            base.slowerStableBandCenterSec.map { $0 * scale },
            tolerance: 1e-9
        ))
        #expect(frozenClose(
            scaled.sustainedBandLogDispersion,
            base.sustainedBandLogDispersion
        ))
        #expect(frozenClose(
            scaled.slowerStableBandLogDispersion,
            base.slowerStableBandLogDispersion
        ))
    }
}

@Test
func frozenStateBandIsInvariantToTrainOrder() {
    let alpha = frozenProfileTrain(
        name: "alpha",
        isis: Array(repeating: 0.009, count: 34) + Array(repeating: 0.024, count: 4)
    )
    let beta = frozenProfileTrain(
        name: "beta",
        isis: Array(repeating: 0.015, count: 36) + Array(repeating: 0.036, count: 5)
    )
    let gamma = frozenProfileTrain(name: "gamma", isis: Array(repeating: 0.021, count: 31))

    #expect(frozenProfile([alpha, beta, gamma]) == frozenProfile([gamma, alpha, beta]))
}

@Test
func frozenStateBandBalancesTrainsRatherThanRawISICounts() {
    let shortFast = frozenProfileTrain(name: "fast", isis: Array(repeating: 0.010, count: 35))
    let longFast = frozenProfileTrain(name: "fast", isis: Array(repeating: 0.010, count: 3_500))
    let comparison = frozenProfileTrain(name: "comparison", isis: Array(repeating: 0.040, count: 35))

    let shortProfile = frozenProfile([shortFast, comparison])
    let longProfile = frozenProfile([longFast, comparison])

    #expect(shortProfile.baseCenterContributingTrainCount == 2)
    #expect(longProfile.baseCenterContributingTrainCount == 2)
    #expect(frozenClose(shortProfile.sustainedBandLogCenter, longProfile.sustainedBandLogCenter))
    #expect(frozenClose(shortProfile.sustainedBandLogDispersion, longProfile.sustainedBandLogDispersion))
    #expect(frozenClose(shortProfile.sustainedBandCenterSec, 0.020, tolerance: 1e-10))
}

@Test
func frozenStateBandAbstainsWithoutDatasetSlowerStableSupport() {
    let train = frozenProfileTrain(name: "one-band", isis: Array(repeating: 0.012, count: 60))
    let profile = frozenProfile([train])

    #expect(profile.baseCenterContributingTrainCount == 1)
    #expect(profile.trains[0].slowerStableSupport.isEmpty)
    #expect(profile.slowerStableContributingTrainCount == 0)
    #expect(profile.slowerStableBandLogCenter == nil)
    #expect(profile.relativeHFSAdjudicationEligibility == .abstainNoDatasetSlowerStableSupport)
}

@Test
func frozenStateBandRequiresExplicitRegularSlowerSupportBeforeHFSEligibility() {
    let train = frozenProfileTrain(
        name: "two-band",
        isis: Array(repeating: 0.010, count: 35)
            + Array(repeating: 0.025, count: 5)
    )
    let profile = frozenProfile([train])
    let slower = profile.trains[0].slowerStableSupport

    #expect(!slower.isEmpty)
    #expect(slower.allSatisfy { $0.isiCount >= 3 })
    #expect(zip(slower, slower.dropFirst()).allSatisfy { $0.endISIIndex < $1.startISIIndex })
    #expect(slower.allSatisfy { run in
        guard let sustainedCenter = profile.trains[0].sustainedBandCenterSec else {
            return false
        }
        return run.medianISISec >= 2 * sustainedCenter
    })
    for baseRun in profile.trains[0].sustainedStableRuns {
        for slowerRun in slower {
            #expect(baseRun.endISIIndex < slowerRun.startISIIndex
                || slowerRun.endISIIndex < baseRun.startISIIndex)
        }
    }
    #expect(profile.relativeHFSAdjudicationEligibility == .eligibleWithDatasetScaleSeparation)
}

@Test
func frozenStateBandKeepsLongFastAndLongSlowBandsOutOfTheFrozenCenter() {
    let train = frozenProfileTrain(
        name: "two-long-bands",
        isis: Array(repeating: 0.010, count: 35)
            + Array(repeating: 0.030, count: 35)
    )
    let profile = frozenProfile([train])
    let trainProfile = profile.trains[0]

    #expect(trainProfile.rawStableRunCount >= 2)
    #expect(frozenClose(profile.sustainedBandCenterSec, 0.010, tolerance: 1e-9))
    #expect(trainProfile.sustainedStableRuns.allSatisfy { $0.medianISISec <= 0.015 })
    #expect(!trainProfile.slowerStableSupport.isEmpty)
    #expect(trainProfile.slowerStableSupport.allSatisfy { $0.medianISISec >= 0.020 })
    for baseRun in trainProfile.sustainedStableRuns {
        for slowerRun in trainProfile.slowerStableSupport {
            #expect(baseRun.endISIIndex < slowerRun.startISIIndex
                || slowerRun.endISIIndex < baseRun.startISIIndex)
        }
    }
}

@Test
func frozenStateBandUsesFrozenDatasetCenterForCrossTrainSlowerEvidence() {
    let baseA = frozenProfileTrain(name: "base-a", isis: Array(repeating: 0.010, count: 35))
    let baseB = frozenProfileTrain(name: "base-b", isis: Array(repeating: 0.011, count: 35))
    let slowOnly = frozenProfileTrain(name: "slow-only", isis: Array(repeating: 0.030, count: 35))
    let profile = frozenProfile([slowOnly, baseB, baseA])
    let slowProfile = profile.trains.first { $0.trainName == "slow-only" }

    #expect(frozenClose(profile.sustainedBandCenterSec, 0.011, tolerance: 1e-9))
    #expect(slowProfile?.sustainedStableRuns.isEmpty == true)
    #expect(slowProfile?.slowerStableSupport.isEmpty == false)
    #expect(profile.slowerStableContributingTrainCount == 1)
    #expect(profile.relativeHFSAdjudicationEligibility == .eligibleWithDatasetScaleSeparation)
}

@Test
func frozenStateBandCannotBecomeEligibleWithoutRetainedBaseSupport() {
    let fast = frozenProfileTrain(name: "fast", isis: Array(repeating: 0.010, count: 35))
    let slow = frozenProfileTrain(name: "slow", isis: Array(repeating: 0.040, count: 35))
    let profile = frozenProfile([fast, slow])

    // With two widely separated contributions the type-7 log median lies at
    // 20 ms, outside +/-1.5x of both 10 and 40 ms runs. A slower run alone must
    // not turn that unresolved base estimate into HFS adjudication eligibility.
    #expect(frozenClose(profile.sustainedBandCenterSec, 0.020, tolerance: 1e-9))
    #expect(profile.sustainedBaseSupportTrainCount == 0)
    #expect(profile.relativeHFSAdjudicationEligibility == .abstainNoDatasetBaseSupport)
}

@Test
func frozenStateBandDuplicateNamesAreUniqueWithinDatasetButNotCrossRebuildIdentity() {
    let fast = frozenProfileTrain(name: "duplicate", isis: Array(repeating: 0.010, count: 35))
    let slow = frozenProfileTrain(name: "duplicate", isis: Array(repeating: 0.020, count: 35))
    let forward = frozenProfile([fast, slow])
    let reversed = frozenProfile([slow, fast])

    #expect(forward.trains.map(\.trainID) == ["duplicate", "duplicate#2"])
    #expect(Set(forward.trains.map(\.trainID)).count == forward.trains.count)
    #expect(reversed.trains.map(\.trainID) == ["duplicate", "duplicate#2"])
    #expect(frozenClose(forward.sustainedBandLogCenter, reversed.sustainedBandLogCenter))
    #expect(frozenClose(forward.sustainedBandLogDispersion, reversed.sustainedBandLogDispersion))

    // `SpikeDataset` assigns duplicate-name suffixes in input order. The aggregate
    // science is invariant, but cross-rebuild per-train identity needs an upstream
    // authoritative ID and is deliberately not claimed by this profile.
    #expect(forward.trains.first?.baseCenterLogContribution
        != reversed.trains.first?.baseCenterLogContribution)
}

@Test
func frozenStateBandFixedFloorChangesResultsWhenScalingChangesValidityMask() {
    let baseISI = Array(repeating: 0.010, count: 29)
        + [0.0005]
        + Array(repeating: 0.010, count: 29)
    let base = frozenProfile([frozenProfileTrain(name: "floor-boundary", isis: baseISI)])
    let scaled = frozenProfile([
        frozenProfileTrain(name: "floor-boundary", isis: baseISI.map { $0 * 10 })
    ])

    #expect(base.trains[0].sustainedStableRuns.count == 2)
    #expect(scaled.trains[0].sustainedStableRuns.count == 1)
    #expect(base.trains[0].sustainedStableRuns.map(\.startISIIndex)
        != scaled.trains[0].sustainedStableRuns.map(\.startISIIndex))
}

@Test
func frozenStateBandRejectsVariableBurstLikePacketsAsSustainedSupport() {
    let packet = Array(repeating: [0.004, 0.018, 0.006, 0.022], count: 10).flatMap { $0 }
    let profile = frozenProfile([frozenProfileTrain(name: "variable-packet", isis: packet)])

    #expect(profile.trains[0].sustainedStableRuns.isEmpty)
    #expect(profile.sustainedBandLogCenter == nil)
    #expect(profile.relativeHFSAdjudicationEligibility == .abstainNoFrozenBaseBand)
}
