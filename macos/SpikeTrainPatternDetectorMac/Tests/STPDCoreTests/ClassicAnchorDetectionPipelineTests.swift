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
func sampleClearTwoSidedBurstClusterWithToleratedInternalTailIsDetected() throws {
    // Regression for the bundled sample miss: train LT1D00.83_fon1_2_nw_minus_7_08_minus_1_2
    // has a compact two-sided burst-like cluster (raw 30.108540 ... 30.154347 s, internal ISIs
    // 14.954/9.531/12.038/9.284 ms, pre-gap 63.844 ms, post-gap 66.638 ms). Its intraQ90 passes
    // the adaptive compactness upper but its max internal ISI (14.954 ms) sits ~0.046 ms above
    // it, so the strict max-ISI gate erased a clearly-bounded packet. It must now produce a
    // selected burst-family event.
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    let csv = try String(contentsOf: sampleURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "sample",
        sourceDescription: sampleURL.path,
        unit: .seconds
    )
    let train = try #require(dataset.trains.first { $0.name == "LT1D00.83_fon1_2_nw_minus_7_08_minus_1_2" })

    // Locate the cluster by spike time so the assertion does not depend on hard-coded indices.
    let clusterStartSpike = try #require(train.timestampsSec.firstIndex { abs($0 - 30.108540) <= 1e-4 })
    let clusterEndSpike = try #require(train.timestampsSec.firstIndex { abs($0 - 30.154347) <= 1e-4 })
    let clusterISIRange = (clusterStartSpike + 1)...clusterEndSpike

    let singleTrainDataset = SpikeDataset(
        name: "single sample train",
        sourceDescription: sampleURL.path,
        trains: [train]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: singleTrainDataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    // A selected burst-family EVENT must overlap the cluster.
    let clusterBursts = selectedBurstFamilyEvents(in: run.candidates).filter { candidate in
        candidate.trainID == train.id &&
            (candidate.startISIIndex...candidate.endISIIndex).overlaps(clusterISIRange)
    }
    let burst = try #require(clusterBursts.first)
    #expect(burst.finalLabel.isBurstEventFamily)
    #expect(burst.auditRecommendedTrack == .event)
    #expect(burst.selectedForAuto)

    // The detection comes from the structure-first tolerated-internal-tail rescue (not an
    // incidental other path), and the audit text exposes the rescue evidence.
    let rescueCandidate = try #require(run.candidates.first { candidate in
        candidate.trainID == train.id &&
            candidate.gateStatus == "structure_first_two_sided_classic_burst_i_pass_with_tolerated_internal_tail" &&
            (candidate.startISIIndex...candidate.endISIIndex).overlaps(clusterISIRange)
    })
    #expect(rescueCandidate.decisionPath.contains("max_intra_isi_sec="))
    #expect(rescueCandidate.decisionPath.contains("max_intra_excess_ratio="))
    #expect(rescueCandidate.decisionPath.contains("max_intra_tolerated_tail=true"))

    // It must NOT be classified as a state (tonic / HF tonic / HFS).
    let clusterStates = run.candidates.filter { candidate in
        candidate.trainID == train.id &&
            candidate.selectedForAuto &&
            (candidate.startISIIndex...candidate.endISIIndex).overlaps(clusterISIRange) &&
            [ClassicAnchorLabel.tonic, .highFrequencyTonic, .highFrequencySpiking].contains(candidate.finalLabel)
    }
    #expect(clusterStates.isEmpty)

    // The flanking large gaps (pre-gap and post-gap ISI indices) may still carry pause candidates.
    let flankPauseISIs = [clusterStartSpike, clusterEndSpike + 1]
    let flankPauses = run.candidates.filter { candidate in
        candidate.trainID == train.id &&
            candidate.finalLabel == .pause &&
            flankPauseISIs.contains { (candidate.startISIIndex...candidate.endISIIndex).contains($0) }
    }
    #expect(!flankPauses.isEmpty)
}

@Test
func toleratedInternalTailRescueRequiresStrongTwoSidedBoundaries() throws {
    // Overcorrection guard. Two trains are identical except the post-flank gap of a compact
    // cluster whose max internal ISI (12 ms) sits just above the adaptive compactness upper
    // (~10.5 ms) while its bulk (intraQ90 ~9.3 ms) is compact. The cluster's ISI indices are
    // 12...15 in both trains.
    //   - Strong TWO-SIDED gaps (60 ms each): the tolerated-tail rescue fires → burst event.
    //   - Strong PRE gap but WEAK post boundary (4 ms): the rescue must NOT fire → no burst,
    //     so a max-ISI tail is never rescued without strong two-sided boundary evidence.
    func guardTrain(strongPostGap: Bool) -> SpikeTrain {
        var isi: [Double] = Array(repeating: 0.030, count: 10)
        isi.append(0.060)                                   // strong pre gap
        isi.append(contentsOf: [0.003, 0.003, 0.003, 0.012]) // compact cluster, max tail 12 ms
        isi.append(strongPostGap ? 0.060 : 0.004)           // post boundary: strong or weak
        isi.append(contentsOf: Array(repeating: 0.030, count: 10))
        var timestamps = [0.0]
        for value in isi {
            timestamps.append((timestamps.last ?? 0) + value)
        }
        return SpikeTrain(name: "guard_\(strongPostGap ? "two_sided" : "one_sided")", timestampsSec: timestamps)
    }

    func run(strongPostGap: Bool) -> ClassicAnchorDetectionRun {
        let train = guardTrain(strongPostGap: strongPostGap)
        return ClassicAnchorDetectionPipeline.run(
            dataset: SpikeDataset(name: "guard", sourceDescription: "unit-test", trains: [train]),
            bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
        )
    }
    let toleratedTailStatus = "structure_first_two_sided_classic_burst_i_pass_with_tolerated_internal_tail"
    func rescueUsed(_ r: ClassicAnchorDetectionRun) -> Bool {
        r.candidates.contains { $0.gateStatus == toleratedTailStatus }
    }
    // The max-tail ISI (index 15) is only pulled into a burst when the rescue fires.
    func tailIncludedInSelectedBurst(_ r: ClassicAnchorDetectionRun) -> Bool {
        selectedBurstFamilyEvents(in: r.candidates).contains {
            $0.startISIIndex <= 15 && $0.endISIIndex >= 15
        }
    }

    let twoSided = run(strongPostGap: true)
    let oneSided = run(strongPostGap: false)

    // Strong two-sided boundaries: the tolerated-tail rescue fires and the tail is included.
    #expect(rescueUsed(twoSided))
    #expect(tailIncludedInSelectedBurst(twoSided))
    // Weak opposite boundary: the rescue must NOT fire and the max-tail ISI must NOT be pulled
    // into a burst (the compact core can still be its own burst, but the tail is not rescued).
    #expect(!rescueUsed(oneSided))
    #expect(!tailIncludedInSelectedBurst(oneSided))
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
func datasetSeedAwareRerunRecordsProvenance() throws {
    let dataset = seedAwareSyntheticDataset()

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let provenance = run.datasetRerunProvenance

    #expect(provenance.source == "dataset_seed_aware_rerun")
    #expect(provenance.stagePath == [
        "dataset_seed_aggregation",
        "dataset_bridge_expansion",
        "train_seed_summary_refresh",
        "final_dataset_seed_aggregation",
        "apply_dataset_seed_summary",
        "dataset_seed_aware_rerun"
    ])
    #expect(provenance.trainCount == dataset.trains.count)
    #expect(provenance.rerunExecuted)
    #expect(provenance.rerunTrainCount == dataset.trains.count)
    #expect(provenance.initialDatasetSummary.trainCount == dataset.trains.count)
    #expect(provenance.finalDatasetSummary == run.datasetStructuralSeedSummary)
    #expect(provenance.finalDatasetSummary.source.hasPrefix("structural_dataset_seed_aggregate"))
    #expect(provenance.datasetSummaryAppliedToResolutions == run.datasetStructuralSeedSummary.hasAnyAnchor)
}

@Test
func datasetSeedAwareRerunIsDeterministicForSameDataset() throws {
    let dataset = seedAwareSyntheticDataset()

    let first = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let second = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    #expect(first.datasetRerunProvenance == second.datasetRerunProvenance)
    #expect(stableCandidateSignature(first) == stableCandidateSignature(second))
}

@Test
func datasetSeedAwareRerunKeepsCleanTonicTargetBoundedWithContextTrain() throws {
    let target = cleanTonicTrain()
    let context = SpikeTrain(
        name: "context_burst_seed",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let isolatedDataset = SpikeDataset(
        name: "isolated clean tonic",
        sourceDescription: "unit-test",
        trains: [target]
    )
    let contextDataset = SpikeDataset(
        name: "contextual clean tonic",
        sourceDescription: "unit-test",
        trains: [context, target]
    )

    let isolated = ClassicAnchorDetectionPipeline.run(
        dataset: isolatedDataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let withContext = ClassicAnchorDetectionPipeline.run(
        dataset: contextDataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let isolatedTarget = try #require(isolated.result(for: target.id))
    let contextTarget = try #require(withContext.result(for: target.id))
    let isolatedSelected = selectedNonProfileBehaviorSignature(in: isolatedTarget.candidates)
    let contextSelected = selectedNonProfileBehaviorSignature(in: contextTarget.candidates)

    #expect(withContext.datasetRerunProvenance.trainCount == 2)
    #expect(withContext.datasetRerunProvenance.rerunTrainCount == 2)
    #expect(isolatedTarget.candidates.filter { $0.selectedForAuto && $0.finalLabel == .pause }.isEmpty)
    #expect(contextTarget.candidates.filter { $0.selectedForAuto && $0.finalLabel == .pause }.isEmpty)
    #expect(contextSelected == isolatedSelected)
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

private func seedAwareSyntheticDataset() -> SpikeDataset {
    let first = SpikeTrain(
        name: "seeded_fast_burst",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let second = SpikeTrain(
        name: "seeded_slower_burst",
        timestampsSec: [0, 0.300, 0.320, 0.340, 0.360, 0.860, 1.360, 1.860]
    )
    return SpikeDataset(
        name: "synthetic seed-aware rerun",
        sourceDescription: "unit-test",
        trains: [first, second]
    )
}

private func cleanTonicTrain() -> SpikeTrain {
    SpikeTrain(
        name: "bounded_clean_tonic",
        timestampsSec: stride(from: 0.0, through: 0.700, by: 0.050).map { $0 }
    )
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

private func stableCandidateSignature(_ run: ClassicAnchorDetectionRun) -> [String] {
    run.candidates.map(candidateSignature).sorted()
}

private func selectedNonProfileBehaviorSignature(in candidates: [ClassicAnchorCandidate]) -> [String] {
    candidates
        .filter { $0.selectedForAuto && $0.finalLabel != .profile }
        .map { candidate in
            [
                candidate.trainID,
                "\(candidate.finalLabel)",
                "\(candidate.auditRecommendedTrack)",
                "\(candidate.startISIIndex)-\(candidate.endISIIndex)"
            ].joined(separator: "|")
        }
        .sorted()
}

private func candidateSignature(_ candidate: ClassicAnchorCandidate) -> String {
    [
        candidate.trainID,
        candidate.candidateLayer,
        candidate.candidateClass,
        "\(candidate.finalLabel)",
        "\(candidate.auditRecommendedTrack)",
        "\(candidate.startISIIndex)-\(candidate.endISIIndex)",
        candidate.selectedForAuto ? "selected" : "unselected",
        "\(candidate.selectionStatus)"
    ].joined(separator: "|")
}
