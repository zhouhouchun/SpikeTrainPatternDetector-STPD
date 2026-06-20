import Foundation
import STPDCore
import Testing

@Test
func trainAdaptiveBandResolverDoesNotInventDefaultBurstBandForSparseTrain() throws {
    let train = SpikeTrain(
        name: "sparse",
        timestampsSec: [0, 0.200, 0.420]
    )

    let resolution = TrainAdaptiveBandResolver.resolve(
        train: train,
        settings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let burst = try #require(resolution.burstBand)
    let hfs = try #require(resolution.band(for: .highFrequencySpiking))
    let pause = try #require(resolution.band(for: .pause))

    #expect(resolution.validISICount == 2)
    #expect(burst.primarySource == .none)
    #expect(isClose(burst.seedLowerSec, 0.001))
    #expect(isClose(burst.seedUpperSec, 0.002))
    #expect(isClose(burst.bridgeUpperSec, 0.002))
    #expect(burst.contrastS == nil)
    #expect(isClose(hfs.seedUpperSec, 0.020))
    #expect(isClose(pause.seedLowerSec, 0.100))
    #expect(resolution.thresholdRows.count == AdaptiveBandPattern.allCases.count * AdaptiveBandField.allCases.count)

    let profile = resolution.seedBandProfile
    #expect(profile.nValidISI == 2)
    #expect(isClose(profile.datasetISISeedLowSec, 0.001))
    #expect(isClose(profile.datasetISISeedHighSec, 0.001))
    #expect(isClose(profile.datasetISIBridgeHighSec, 0.001))
    #expect(isClose(profile.datasetISIBoundaryFloorSec, 0.025))
    #expect(profile.seedLowPercentileInTrain == nil)
    #expect(profile.seedHighPercentileInTrain == nil)
    #expect(profile.seedBandFraction == nil)
    #expect(profile.seedRunCount == 0)
    #expect(profile.maxSeedRunLength == 0)
    #expect(isClose(profile.medianISISec, 0.210))
    #expect(isClose(profile.pauseFraction, 1))
    #expect(profile.phenotypePrior == "no_structural_burst_seed")
}

@Test
func detectionPipelineAddsDatasetSeedBandProfileAuditRowsWithoutAutoSelection() throws {
    let first = SpikeTrain(
        name: "first",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let second = SpikeTrain(
        name: "second",
        timestampsSec: [0, 0.300, 0.320, 0.340, 0.360, 0.860]
    )
    let dataset = SpikeDataset(
        name: "synthetic",
        sourceDescription: "unit-test",
        trains: [first, second]
    )

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let profiles = run.candidates.filter { $0.candidateLayer == "dataset_isi_train_seed_band_profile" }
    let eventCoreProfiles = run.candidates.filter { $0.candidateLayer == "event_core_train_isi_band_profile" }
    let structuralSeedProfiles = run.candidates.filter { $0.candidateLayer == "structural_seed_train_band_profile" }
    let structuralDatasetProfiles = run.candidates.filter { $0.candidateLayer == "structural_dataset_seed_profile" }
    let datasetSummary = run.datasetStructuralSeedSummary

    #expect(profiles.count == 2)
    #expect(profiles.allSatisfy { $0.finalLabel == .profile })
    #expect(profiles.allSatisfy { $0.action == "audit_only" })
    #expect(profiles.allSatisfy { $0.anchorLockLevel == .auditOnly })
    #expect(profiles.allSatisfy { !$0.selectedForAuto })
    #expect(profiles.allSatisfy { !$0.isEligibleForAutoSelection })
    #expect(profiles.allSatisfy { $0.profilePhenotypePrior != nil })
    #expect(eventCoreProfiles.count == 2)
    #expect(eventCoreProfiles.allSatisfy { $0.finalLabel == .profile })
    #expect(eventCoreProfiles.allSatisfy { $0.action == "audit_only" })
    #expect(eventCoreProfiles.allSatisfy { !$0.selectedForAuto })
    #expect(eventCoreProfiles.allSatisfy { !$0.isEligibleForAutoSelection })
    #expect(eventCoreProfiles.allSatisfy { $0.decisionPath == "dataset_manual_isi_band_profile_percentiles_are_outputs" })
    #expect(eventCoreProfiles.allSatisfy { $0.profileQ10ISISec != nil })
    #expect(eventCoreProfiles.allSatisfy { $0.profileQ25ISISec != nil })
    #expect(eventCoreProfiles.allSatisfy { $0.profileQ90ISISec != nil })
    #expect(eventCoreProfiles.allSatisfy { $0.profileBoundaryFloorHard == false })
    #expect(eventCoreProfiles.allSatisfy { $0.profileBurstContrastS != nil })
    #expect(eventCoreProfiles.allSatisfy { $0.profilePossibleContrastS != nil })
    #expect(structuralSeedProfiles.count == 2)
    #expect(structuralSeedProfiles.allSatisfy { $0.finalLabel == .profile })
    #expect(structuralSeedProfiles.allSatisfy { $0.action == "audit_only" })
    #expect(structuralSeedProfiles.allSatisfy { !$0.selectedForAuto })
    #expect(structuralSeedProfiles.allSatisfy { $0.decisionPath.hasPrefix("structural_seed_summary") })
    #expect(structuralDatasetProfiles.count == 1)
    #expect(structuralDatasetProfiles.allSatisfy { $0.finalLabel == .profile })
    #expect(structuralDatasetProfiles.allSatisfy { $0.candidateClass == "dataset_profile" })
    #expect(structuralDatasetProfiles.allSatisfy { !$0.selectedForAuto })
    #expect(structuralDatasetProfiles.allSatisfy { $0.decisionPath.hasPrefix("dataset_structural_seed_aggregate") })
    #expect(datasetSummary.trainCount == 2)
    #expect(datasetSummary.seededTrainCount >= 1)
    #expect(datasetSummary.burstAnchorCount >= 1)
    #expect(run.eventAnnotations(in: dataset).allSatisfy { annotation in
        run.candidates.first { $0.id == annotation.candidateID }?.finalLabel != .profile
    })
}

@Test
func optionalPDSTNAdaptiveBandResolverMatchesRReferenceAndFeedsClassicAnchor() throws {
    guard let path = ProcessInfo.processInfo.environment["STPD_COMPLEX_CSV"], !path.isEmpty else {
        return
    }

    let csvURL = URL(fileURLWithPath: path)
    let csv = try String(contentsOf: csvURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "PD_STN",
        sourceDescription: csvURL.path,
        unit: .seconds,
        duplicatePolicy: .collapseExact
    )

    try assertAdaptiveBandsAndAnchorCounts(
        dataset: dataset,
        trainName: "LT1D00.732F001-nw-11 (flag 1)",
        expectedBurstLower: 0.001218035,
        expectedBurstUpper: 0.035,
        expectedBurstBridge: 0.050,
        expectedAnchorCount: 8,
        expectedLockedCount: 4,
        expectedFirstStartISI: 16,
        expectedFirstEndISI: 18,
        expectedFirstLabel: .burst,
        expectedFirstMinContrast: 3.348,
        contrastTolerance: 0.001
    )

    try assertAdaptiveBandsAndAnchorCounts(
        dataset: dataset,
        trainName: "RT2D03.535_nw-5 (flag 1)",
        expectedBurstLower: 0.001315,
        expectedBurstUpper: 0.015,
        expectedBurstBridge: 0.0225,
        expectedAnchorCount: 185,
        expectedLockedCount: 102,
        expectedFirstStartISI: 12,
        expectedFirstEndISI: 17,
        expectedFirstLabel: .burst,
        expectedFirstMinContrast: 2.631,
        contrastTolerance: 0.001
    )
}

private func assertAdaptiveBandsAndAnchorCounts(
    dataset: SpikeDataset,
    trainName: String,
    expectedBurstLower: Double,
    expectedBurstUpper: Double,
    expectedBurstBridge: Double,
    expectedAnchorCount: Int,
    expectedLockedCount: Int,
    expectedFirstStartISI: Int,
    expectedFirstEndISI: Int,
    expectedFirstLabel: ClassicAnchorLabel,
    expectedFirstMinContrast: Double,
    contrastTolerance: Double
) throws {
    let train = try #require(dataset.trains.first { $0.name == trainName })
    let resolution = TrainAdaptiveBandResolver.resolve(
        train: train,
        settings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let burst = try #require(resolution.burstBand)

    #expect(burst.primarySource == .histogram)
    #expect(isClose(burst.seedLowerSec, expectedBurstLower, tolerance: 1e-9))
    #expect(isClose(burst.seedUpperSec, expectedBurstUpper, tolerance: 1e-12))
    #expect(isClose(burst.bridgeUpperSec, expectedBurstBridge, tolerance: 1e-12))
    #expect(isClose(burst.contrastS, 3.0))

    let settings = ClassicAnchorSettings(adaptiveResolution: resolution)
    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let referenceCandidates = result.candidates.filter { candidate in
        candidate.isEligibleForAutoSelection &&
            candidate.candidateClass == "classic_anchor_short_isi_packet"
    }
    let first = try #require(referenceCandidates.first)

    #expect(referenceCandidates.count == expectedAnchorCount)
    #expect(referenceCandidates.filter { $0.anchorLockLevel == .lockedClassic }.count == expectedLockedCount)
    #expect(first.startISIIndex == expectedFirstStartISI)
    #expect(first.endISIIndex == expectedFirstEndISI)
    #expect(first.finalLabel == expectedFirstLabel)
    #expect(isClose(first.edgeContrastMinQ90, expectedFirstMinContrast, tolerance: contrastTolerance))
    #expect(result.candidates.contains { $0.finalLabel == .reject && $0.action == "reject" })
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}
