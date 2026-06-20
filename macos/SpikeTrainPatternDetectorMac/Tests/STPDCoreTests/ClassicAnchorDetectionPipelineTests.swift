import Foundation
import STPDCore
import Testing

@Test
func classicAnchorDetectionPipelineUsesAdaptiveBandsPerTrain() throws {
    let first = SpikeTrain(
        name: "fast_train",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let second = SpikeTrain(
        name: "slower_train",
        timestampsSec: [0, 0.300, 0.320, 0.340, 0.360, 0.860, 1.360, 1.860]
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
    let firstResolution = try #require(run.resolution(for: "fast_train"))
    let secondResolution = try #require(run.resolution(for: "slower_train"))

    #expect(run.resolutions.count == 2)
    #expect(run.results.count == 2)
    #expect(firstResolution.burstBand?.primarySource == .structure)
    #expect(secondResolution.burstBand?.primarySource == .structure)
    #expect(firstResolution.burstBand?.seedUpperSec != secondResolution.burstBand?.seedUpperSec)
    #expect(selectedBurstFamilyEvents(in: run.result(for: "fast_train")?.candidates ?? []).count == 1)
    #expect(selectedBurstFamilyEvents(in: run.result(for: "slower_train")?.candidates ?? []).count == 1)
    #expect(selectedBurstFamilyEvents(in: run.candidates).count == 2)

    let annotations = run.eventAnnotations(in: dataset)
    #expect(!annotations.isEmpty)
    #expect(annotations.allSatisfy { annotation in
        run.candidates.first { $0.id == annotation.candidateID }?.selectedForAuto == true
    })
    #expect(annotations.allSatisfy { $0.semanticTrack == .event })
    #expect(annotations.allSatisfy { $0.rawEndSec >= $0.rawStartSec })
    #expect(annotations.allSatisfy { $0.alignedEndSec >= $0.alignedStartSec })
    #expect(Set(annotations.map(\.trainID)) == Set(dataset.trains.map(\.id)))

    let fastAnnotation = try #require(annotations.first { $0.trainID == first.id && $0.visualLabel == .burst })
    let coveredISIIndices = try #require(fastAnnotation.coveredISIIndices(in: first))
    #expect(Array(coveredISIIndices) == [2, 3])
    #expect(fastAnnotation.startISISecIndex == coveredISIIndices.lowerBound)
    #expect(fastAnnotation.endISISecIndex == coveredISIIndices.upperBound)
    #expect(first.timestampsSec[coveredISIIndices.lowerBound - 1] == fastAnnotation.rawStartSec)
    #expect(first.timestampsSec[coveredISIIndices.upperBound] == fastAnnotation.rawEndSec)
}

@Test
func optionalPDSTNClassicAnchorPipelineMatchesRReferenceSubset() throws {
    guard let path = ProcessInfo.processInfo.environment["STPD_COMPLEX_CSV"], !path.isEmpty else {
        return
    }

    let csvURL = URL(fileURLWithPath: path)
    let csv = try String(contentsOf: csvURL, encoding: .utf8)
    let fullDataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "PD_STN",
        sourceDescription: csvURL.path,
        unit: .seconds,
        duplicatePolicy: .collapseExact
    )
    let trainNames = [
        "LT1D00.732F001-nw-11 (flag 1)",
        "RT2D03.535_nw-5 (flag 1)"
    ]
    let trains = fullDataset.trains.filter { trainNames.contains($0.name) }
    let subset = SpikeDataset(
        name: "PD_STN_subset",
        sourceDescription: csvURL.path,
        trains: trains
    )

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: subset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    try assertPipelineTrain(
        run,
        trainName: "LT1D00.732F001-nw-11 (flag 1)",
        expectedBurstLower: 0.001218035,
        expectedBurstUpper: 0.035,
        expectedCandidateCount: 8,
        expectedLockedCount: 4,
        minimumSelectedPauseCount: 22
    )
    try assertPipelineTrain(
        run,
        trainName: "RT2D03.535_nw-5 (flag 1)",
        expectedBurstLower: 0.001315,
        expectedBurstUpper: 0.015,
        expectedCandidateCount: 185,
        expectedLockedCount: 102,
        minimumSelectedPauseCount: 36
    )

    let annotations = run.eventAnnotations(in: subset)
    #expect(!annotations.isEmpty)
    #expect(annotations.allSatisfy { annotation in
        run.candidates.first { $0.id == annotation.candidateID }?.selectedForAuto == true
    })
    #expect(annotations.allSatisfy { $0.semanticTrack == .event })
    #expect(annotations.contains { annotation in
        annotation.trainName == "LT1D00.732F001-nw-11 (flag 1)" &&
            annotation.lockLevel == .lockedClassic &&
            annotation.rawEndSec >= annotation.rawStartSec
    })
    #expect(annotations.contains { annotation in
        annotation.trainName == "RT2D03.535_nw-5 (flag 1)" &&
            annotation.visualLabel == .burst &&
            annotation.alignedEndSec >= annotation.alignedStartSec
    })
}

@Test
func sampleClassicAnchorKeepsShortValidISIInsideBurstCandidate() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    let csv = try String(contentsOf: sampleURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "sample",
        sourceDescription: sampleURL.path,
        unit: .seconds
    )
    let train = try #require(dataset.trains.first { $0.name == "LT1D00.83_fon1_1_nw_minus_7_08_minus_1_1" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "single sample train", sourceDescription: sampleURL.path, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    let matchingBursts = selectedBurstFamilyEvents(in: run.candidates).filter { candidate in
        candidate.trainID == train.id &&
            candidate.startISIIndex >= 84 &&
            candidate.endISIIndex <= 91
    }
    let covered = Set(matchingBursts.flatMap { candidate in
        Array(candidate.startISIIndex...candidate.endISIIndex)
    })
    let burst = try #require(matchingBursts.first)
    let allSelected = matchingBursts.allSatisfy { $0.selectedForAuto }
    let annotations = run.eventAnnotations(in: SpikeDataset(name: "single sample train", sourceDescription: sampleURL.path, trains: [train]))
    let annotationCoverage = Set(annotations
        .filter { annotation in
            annotation.trainID == train.id &&
                annotation.visualLabel == .burst &&
                (annotation.startISISecIndex...annotation.endISISecIndex).overlaps(84...91)
        }
        .flatMap { annotation in
            Array(annotation.startISISecIndex...annotation.endISISecIndex)
        })

    #expect(covered == Set(84...91))
    #expect(allSelected)
    #expect(annotationCoverage.isSuperset(of: Set(84...91)))
    #expect([ClassicAnchorLabel.highFrequencyBurst.rawValue, "burst_ii", ClassicAnchorLabel.burst.rawValue].contains(burst.auditRecommendedFinalClass))
}

@Test
func sampleClassicBurstFlanksBecomeStructuralPauseEvidenceBelowClassicPauseFloor() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    let csv = try String(contentsOf: sampleURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "sample",
        sourceDescription: sampleURL.path,
        unit: .seconds
    )
    let train = try #require(dataset.trains.first { $0.name == "LT1D00.83_fon1_1_nw_minus_7_08_minus_1_1" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "single sample train", sourceDescription: sampleURL.path, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    let burstEvents = selectedBurstFamilyEvents(in: run.candidates).filter { candidate in
        candidate.trainID == train.id &&
            candidate.startISIIndex >= 84 &&
            candidate.endISIIndex <= 91
    }
    let coveredBurstISI = Set(burstEvents.flatMap { candidate in
        Array(candidate.startISIIndex...candidate.endISIIndex)
    })
    _ = try #require(burstEvents.first)
    let flankPauses = run.candidates.filter { candidate in
        candidate.trainID == train.id &&
            candidate.selectedForAuto &&
            candidate.auditRecommendedTrack == .gap &&
            candidate.finalLabel == .pause &&
            (candidate.startISIIndex == 83 || candidate.startISIIndex == 92)
    }
    let allFlankPausesSelected = flankPauses.allSatisfy { $0.selectedForAuto }
    let allFlankPausesGap = flankPauses.allSatisfy { $0.auditRecommendedTrack == .gap }
    let allFlankPausesBelowClassicPauseFloor = flankPauses.allSatisfy { ($0.maxIntraISISec ?? 0) < 0.100 }

    #expect(coveredBurstISI == Set(84...91))
    #expect(Set(flankPauses.map(\.startISIIndex)) == [83, 92])
    #expect(allFlankPausesSelected)
    #expect(allFlankPausesGap)
    #expect(allFlankPausesBelowClassicPauseFloor)
}

@Test
func samplePipelineSummarizesStructuralSeedAnchorsPerTrain() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    let csv = try String(contentsOf: sampleURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "sample",
        sourceDescription: sampleURL.path,
        unit: .seconds
    )
    let train = try #require(dataset.trains.first { $0.name == "LT1D00.83_fon1_1_nw_minus_7_08_minus_1_1" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "single sample train", sourceDescription: sampleURL.path, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    let resolution = try #require(run.resolution(for: train.id))
    let summary = resolution.structuralSeedSummary
    let datasetSummary = run.datasetStructuralSeedSummary

    #expect(summary.source.hasPrefix("structural_candidate_audit"))
    #expect(summary.burstAnchorCount > 0)
    #expect(summary.burstSeedUpperSec != nil)
    #expect(summary.burstBridgeUpperSec != nil)
    #expect(summary.pauseAnchorCount >= 2)
    #expect(summary.pauseSeedLowerSec != nil)
    #expect(summary.pauseSeedUpperSec != nil)
    #expect(datasetSummary.source.hasPrefix("structural_dataset_seed_aggregate"))
    #expect(datasetSummary.trainCount == 1)
    #expect(datasetSummary.seededTrainCount == 1)
    #expect(datasetSummary.burstAnchorCount == summary.burstAnchorCount)
    #expect(datasetSummary.pauseAnchorCount == summary.pauseAnchorCount)
}

@Test
func structuralBridgeExpansionConnectsAdjacentBurstFragmentsAcrossSlightlyLargeISI() throws {
    let train = SpikeTrain(
        name: "bridgeable_burst_train",
        timestampsSec: timestamps(fromISI: [
            0.100,
            0.004,
            0.004,
            0.018,
            0.004,
            0.004,
            0.100
        ])
    )
    let dataset = SpikeDataset(
        name: "synthetic bridge expansion",
        sourceDescription: "unit-test",
        trains: [train]
    )

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let bridgeAudit = try #require(run.candidates.first { candidate in
        candidate.trainID == train.id &&
            candidate.startISIIndex == 2 &&
            candidate.endISIIndex == 6 &&
            candidate.finalLabel.isBurstEventFamily
    })
    let selectedCores = selectedBurstFamilyEvents(in: run.candidates).filter {
        $0.trainID == train.id &&
            (($0.startISIIndex == 2 && $0.endISIIndex == 3) ||
             ($0.startISIIndex == 5 && $0.endISIIndex == 6))
    }

    #expect(!bridgeAudit.selectedForAuto)
    #expect(bridgeAudit.anchorBandSource == .structure)
    #expect(bridgeAudit.gateStatus.contains("burst"))
    #expect(Set(selectedCores.map { "\($0.startISIIndex)-\($0.endISIIndex)" }) == Set(["2-3", "5-6"]))
}

@Test
func oneSidedWeakBoundaryShortISIClusterIsRetainedAsPossibleBurst() throws {
    let train = SpikeTrain(
        name: "one_sided_possible_burst",
        timestampsSec: timestamps(fromISI: [
            0.010,
            0.004,
            0.004,
            0.007,
            0.060
        ])
    )

    let result = ClassicAnchorDetector.detect(
        train: train,
        settings: ClassicAnchorSettings(
            minValidISISec: 0.001,
            burstBandLowerSec: 0.001,
            burstBandUpperSec: 0.006,
            burstBridgeUpperSec: 0.006,
            burstBandSource: .userPatternISILimit,
            burstContrastMin: 3.0,
            possibleBurstContrastMin: 2.0
        )
    )
    let candidate = try #require(result.candidates.first { candidate in
        candidate.finalLabel == .possibleBurst &&
            candidate.gateStatus == "event_grammar_one_sided_possible_burst" &&
            candidate.startISIIndex == 2 &&
            candidate.endISIIndex == 3
    })

    #expect(candidate.action == "demote_to_possible")
    #expect(candidate.anchorLockLevel == .strongCandidate)
    #expect(candidate.burstPossibleBoundaryPass == true)
    #expect(candidate.decisionPath.contains("other_side_does_not_lock_classic_boundary"))
    #expect(isClose(candidate.preRatioQ90, 2.5))
    #expect(isClose(candidate.postRatioQ90, 1.75))
}

@Test
func sampleClassicAnchorBridgesScreenshotBurstCluster() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    let csv = try String(contentsOf: sampleURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "sample",
        sourceDescription: sampleURL.path,
        unit: .seconds
    )
    let train = try #require(dataset.trains.first { $0.name == "LT1D01.96_fon_nw_minus_7_08_minus_1_1" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "single sample train", sourceDescription: sampleURL.path, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    let matchingBursts = selectedBurstFamilyEvents(in: run.candidates).filter { candidate in
        candidate.trainID == train.id &&
            candidate.startISIIndex == 36 &&
            candidate.endISIIndex == 39
    }
    let burst = try #require(matchingBursts.first)

    #expect(burst.selectedForAuto)
    #expect(burst.finalLabel.isBurstEventFamily || burst.auditRecommendedEventTrackClass == "burst")
    #expect(burst.anchorBandSource == .structure)
    #expect(burst.gateStatus.contains("burst"))
    #expect(isClose(burst.preGapSec, 0.122305))
    #expect(isClose(burst.postGapSec, 0.085782))
    #expect(isClose(burst.maxIntraISISec, 0.016598))
    #expect((burst.edgeContrastMinQ90 ?? 0) > burst.anchorContrastMinRequired)
}

@Test
func sampleClassicAnchorAcceptsEndBoundarySpikeClusterAsBurst() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    let csv = try String(contentsOf: sampleURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "sample",
        sourceDescription: sampleURL.path,
        unit: .seconds
    )
    let train = try #require(dataset.trains.first { $0.name == "LT1D00.83_fon1_1_nw_minus_7_08_minus_1_1" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "single sample train", sourceDescription: sampleURL.path, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    let matchingBursts = run.candidates.filter { candidate in
        candidate.trainID == train.id &&
            candidate.finalLabel == .burst &&
            candidate.startISIIndex == 116 &&
            candidate.endISIIndex == 119
    }
    let burst = try #require(matchingBursts.first)

    #expect(burst.selectedForAuto)
    #expect(burst.candidateLayer == "structure_first_classic_burst_anchor")
    #expect(burst.candidateClass == "structure_first_endpoint_classic_burst_i")
    #expect(burst.gateStatus == "structure_first_endpoint_classic_burst_i_pass")
    #expect(isClose(burst.preGapSec, 0.054764))
    #expect(burst.postGapSec == nil)
    #expect((burst.edgeContrastMinQ90 ?? 0) > burst.anchorContrastMinRequired)
}

private func assertPipelineTrain(
    _ run: ClassicAnchorDetectionRun,
    trainName: String,
    expectedBurstLower: Double,
    expectedBurstUpper: Double,
    expectedCandidateCount: Int,
    expectedLockedCount: Int,
    minimumSelectedPauseCount: Int
) throws {
    let resolution = try #require(run.resolution(for: trainName))
    let result = try #require(run.result(for: trainName))
    let burst = try #require(resolution.burstBand)
    let referenceBurstCandidates = result.candidates.filter { candidate in
        let auditLabel = candidate.suppressedOriginalLabel ?? candidate.finalLabel.rawValue
        return candidate.candidateClass == "classic_anchor_short_isi_packet" &&
            ["burst", "long_burst", "possible_burst"].contains(auditLabel)
    }
    let selectedPauseCandidates = result.candidates.filter { $0.finalLabel == .pause && $0.selectedForAuto }

    #expect(isClose(burst.seedLowerSec, expectedBurstLower, tolerance: 1e-9))
    #expect(isClose(burst.seedUpperSec, expectedBurstUpper, tolerance: 1e-12))
    #expect(referenceBurstCandidates.count == expectedCandidateCount)
    #expect(referenceBurstCandidates.filter { $0.anchorLockLevel == .lockedClassic }.count == expectedLockedCount)
    // Swift intentionally adds structural pre-screen and classic-burst-flank pause evidence before final
    // arbitration, so pause totals are allowed to exceed the R event-grammar reference count.
    #expect(selectedPauseCandidates.count >= minimumSelectedPauseCount)
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}

private func repositoryRoot() -> URL {
    var url = URL(fileURLWithPath: #filePath)
    for _ in 0..<5 {
        url.deleteLastPathComponent()
    }
    return url
}

private func timestamps(fromISI isi: [Double]) -> [Double] {
    var timestamps = [0.0]
    timestamps.reserveCapacity(isi.count + 1)
    for value in isi {
        timestamps.append((timestamps.last ?? 0) + value)
    }
    return timestamps
}

private func selectedBurstFamilyEvents(in candidates: [ClassicAnchorCandidate]) -> [ClassicAnchorCandidate] {
    candidates.filter { candidate in
        candidate.selectedForAuto &&
            candidate.auditRecommendedTrack == .event &&
            (
                candidate.finalLabel.isBurstEventFamily ||
                    candidate.auditRecommendedEventTrackClass == "burst"
            )
    }
}
