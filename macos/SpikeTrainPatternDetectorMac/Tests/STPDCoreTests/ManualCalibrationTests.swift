import Foundation
import STPDCore
import Testing

// Phase 1C: manual-derived calibration summary (preview/audit only — never applied to the detector).

private func train(_ name: String, _ timestamps: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: timestamps)
}

private func annotation(
    _ trainID: String,
    _ label: ManualAnnotationLabel,
    _ start: Double,
    _ end: Double,
    id: UUID = UUID(),
    createdAt: Date = Date(timeIntervalSince1970: 100),
    updatedAt: Date = Date(timeIntervalSince1970: 100)
) -> ManualAnnotation {
    ManualAnnotation(
        id: id,
        trainID: trainID,
        label: label,
        startSec: start,
        endSec: end,
        createdAt: createdAt,
        updatedAt: updatedAt
    )
}

// Train "g": spikes 0, 0.020, 0.024, 0.030, 0.038 -> ISI2..4 = 0.004, 0.006, 0.008.
private let gTrain = train("g", [0, 0.020, 0.024, 0.030, 0.038])

// MARK: - 1. Exact covered-ISI count + q90/q95.

@Test
func burstAnnotationComputesExactCoveredCountAndPercentiles() throws {
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [gTrain],
        annotations: [annotation("g", .burst, 0.020, 0.038)],
        minValidISISeconds: 0.001
    )
    let row = try #require(summary.row(label: "burst"))
    #expect(row.coveredISICount == 3)            // ISI 2..4: 0.004, 0.006, 0.008
    #expect(row.annotationCount == 1)
    #expect(row.trainCount == 1)
    #expect(abs((row.minISISeconds ?? 0) - 0.004) < 1e-9)
    #expect(abs((row.medianISISeconds ?? 0) - 0.006) < 1e-9)
    #expect(abs((row.maxISISeconds ?? 0) - 0.008) < 1e-9)
    #expect(abs((row.meanISISeconds ?? 0) - 0.006) < 1e-9)
    // q90 over sorted [0.004,0.006,0.008]: 0.006 + 0.8*(0.008-0.006) = 0.0076; q95 = 0.0078.
    #expect(abs((row.q90ISISeconds ?? 0) - 0.0076) < 1e-9)
    #expect(abs((row.q95ISISeconds ?? 0) - 0.0078) < 1e-9)
    #expect(row.isUsableForCalibration)          // enough ISIs for a provisional preview
    #expect(!row.hasIndependentAnnotationReplication)
    #expect(!row.hasCrossTrainReplication)
    #expect(row.source == "manual_annotations")
    #expect(row.appliedToDetector == false)
    // The burst_family row mirrors the single burst label here.
    let family = summary.row(label: "burst_family")
    #expect(family?.isFamily == true)
    #expect(family?.coveredISICount == 3)
    #expect(family?.isUsableForCalibration == true)
    #expect(family?.hasIndependentAnnotationReplication == false)
    #expect(family?.hasCrossTrainReplication == false)
    #expect(abs((family?.q90ISISeconds ?? 0) - 0.0076) < 1e-9)

    // A long marked run cannot inflate confidence: it is based on resolved evidence runs, not ISI count.
    let proposal = LearnedManualThresholdBuilder.build(from: summary)
    let contribution = try #require(
        proposal.contributions.first { $0.sourceLabel == "burst_family" && $0.field == "seed_upper_sec" }
    )
    #expect(contribution.annotationCount == 1)
    #expect(contribution.coveredISICount == 3)
    #expect(abs(contribution.confidence - (1.0 / 7.0)) < 1e-12)
}

@Test
func adjacentSingletonBurstMarksComposeBeforeStructuralValidation() throws {
    let composedTrain = train("composed", [0, 0.010, 0.020, 0.030])
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [composedTrain],
        annotations: [
            annotation("composed", .burst, 0, 0.010),
            annotation("composed", .burst, 0.010, 0.020),
            annotation("composed", .burst, 0.020, 0.030)
        ],
        minValidISISeconds: 0.001
    )

    let burst = try #require(summary.row(label: "burst"))
    let family = try #require(summary.row(label: "burst_family"))
    #expect(burst.coveredISICount == 3)
    #expect(burst.evidenceRunCount == 1)
    #expect(burst.isUsableForCalibration)
    #expect(family.coveredISICount == 3)
    #expect(family.evidenceRunCount == 1)
    #expect(family.isUsableForCalibration)
    #expect(summary.skippedAnnotationCount == 0)
}

@Test
func twoAdjacentSingletonBurstMarksAreStructuralEvidenceButBelowCalibrationGate() throws {
    let composedTrain = train("two-singletons", [0, 0.010, 0.020])
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [composedTrain],
        annotations: [
            annotation("two-singletons", .burst, 0, 0.010),
            annotation("two-singletons", .burst, 0.010, 0.020)
        ],
        minValidISISeconds: 0.001
    )

    let burst = try #require(summary.row(label: "burst"))
    let family = try #require(summary.row(label: "burst_family"))
    #expect(burst.coveredISICount == 2)
    #expect(burst.evidenceRunCount == 1)
    #expect(!burst.isUsableForCalibration)
    #expect(family.coveredISICount == 2)
    #expect(family.evidenceRunCount == 1)
    #expect(!family.isUsableForCalibration)
    #expect(summary.skippedAnnotationCount == 0)
}

@Test
func manyEvidenceRunsFromOneTrainCannotInflateSupportScore() throws {
    let row = ManualAnnotationCalibrationLabelSummary(
        label: "tonic",
        displayName: "Tonic",
        isPositive: true,
        isFamily: false,
        annotationCount: 100,
        trainCount: 1,
        coveredISICount: 400,
        minISISeconds: 0.40,
        q10ISISeconds: 0.42,
        q40ISISeconds: 0.45,
        medianISISeconds: 0.46,
        q90ISISeconds: 0.50,
        q95ISISeconds: 0.52,
        maxISISeconds: 0.55,
        meanISISeconds: 0.46,
        sampleCV: 0.08,
        isUsableForCalibration: true,
        source: "manual_annotations",
        appliedToDetector: false,
        method: "test",
        recommendationText: ""
    )
    let proposal = LearnedManualThresholdBuilder.build(
        from: ManualAnnotationCalibrationSummary(rows: [row], skippedAnnotationCount: 0)
    )
    let contribution = try #require(
        proposal.contributions.first { $0.sourceLabel == "tonic" && $0.field == "isi_upper_sec" }
    )

    #expect(contribution.evidenceRunCount == 100)
    #expect(contribution.trainCount == 1)
    #expect(abs(contribution.supportScore - (1.0 / 7.0)) < 1e-12)
    #expect(contribution.confidence == contribution.supportScore)
}

// MARK: - 2. Wrong-train / out-of-range annotations are skipped, not included in stats.

@Test
func incompatibleAnnotationsAreSkippedAndExcludedFromStats() {
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [gTrain],
        annotations: [
            annotation("g", .burst, 0.020, 0.038),     // valid -> 3 ISIs
            annotation("other", .burst, 0.020, 0.038),  // wrong train
            annotation("g", .burst, 9.0, 9.5)           // out of range
        ],
        minValidISISeconds: 0.001
    )
    #expect(summary.skippedAnnotationCount == 2)
    let row = summary.row(label: "burst")
    #expect(row?.coveredISICount == 3)               // only the valid annotation contributed
    #expect(row?.annotationCount == 1)
}

// MARK: - 3. not_burst veto contributes only negative counts, not positive calibration values.

@Test
func notBurstVetoCountsSeparatelyAndDoesNotAffectBurstCalibration() {
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [gTrain],
        annotations: [
            annotation("g", .burst, 0.020, 0.038),     // ISI 2..4
            annotation("g", .notBurst, 0.020, 0.030)   // ISI 2..3 (overlaps, but separate channel)
        ],
        minValidISISeconds: 0.001
    )
    // Burst calibration is unchanged by the veto.
    let burst = summary.row(label: "burst")
    #expect(burst?.coveredISICount == 3)
    #expect(abs((burst?.q90ISISeconds ?? 0) - 0.0076) < 1e-9)
    // not_burst row: counts only, no percentiles, not usable.
    let veto = summary.row(label: "not_burst")
    #expect(veto?.isPositive == false)
    #expect(veto?.coveredISICount == 2)              // ISI 2..3
    #expect(veto?.q90ISISeconds == nil)
    #expect(veto?.medianISISeconds == nil)
    #expect(veto?.isUsableForCalibration == false)
    #expect(veto?.method == "negative_veto_coverage")
}

// MARK: - 4. Artifact-like ISIs below minValidISISeconds are excluded; the floor itself is valid.

@Test
func artifactLikeISIsAreExcludedFromCalibration() {
    // ISI1 = 0.0005 (artifact), ISI2 = 0.0025, ISI3 = 0.006, ISI4 = 0.008.
    let artifactTrain = train("a", [0, 0.0005, 0.003, 0.009, 0.017])
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [artifactTrain],
        annotations: [annotation("a", .burst, 0.0, 0.017)],   // covers ISI 1..4
        minValidISISeconds: 0.001
    )
    let burst = summary.row(label: "burst")
    #expect(burst?.coveredISICount == 3)             // ISI1 (0.0005 < 0.001) excluded
    #expect(abs((burst?.minISISeconds ?? 0) - 0.0025) < 1e-9)
}

@Test
func qcFilteringCannotLeaveSingletonBurstCalibrationEvidence() {
    // The raw annotation covers two ISIs and initially passes the structural minimum, but ISI 1 is
    // sub-floor. After QC only ISI 2 remains, so burst structure must be revalidated and rejected.
    let artifactTrain = train("qc-burst", [0, 0.0005, 0.0105])
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [artifactTrain],
        annotations: [annotation("qc-burst", .burst, 0, 0.0105)],
        minValidISISeconds: 0.001
    )

    #expect(summary.row(label: "burst") == nil)
    #expect(summary.row(label: "burst_family") == nil)
    #expect(summary.skippedAnnotationCount == 1)
}

@Test
func exactMinimumValidISIFloorIsIncluded() {
    let floorTrain = train("floor", [0, 0.001, 0.003])
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [floorTrain],
        annotations: [annotation("floor", .tonic, 0, 0.003)],
        minValidISISeconds: 0.001
    )
    let tonic = summary.row(label: "tonic")
    #expect(tonic?.coveredISICount == 2)
    #expect(abs((tonic?.minISISeconds ?? 0) - 0.001) < 1e-12)
}

@Test
func annotationWithNoQCValidISIsDoesNotInflateReplicationOrConfidence() throws {
    let validTrain = train("valid", [0, 0.04, 0.08, 0.12, 0.16])
    let subfloorTrain = train("subfloor", [0, 0.0002, 0.0004, 0.0006, 0.0008])
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [validTrain, subfloorTrain],
        annotations: [
            annotation("valid", .tonic, 0, 0.16),
            annotation("subfloor", .tonic, 0, 0.0008)
        ],
        minValidISISeconds: 0.001
    )

    let tonic = try #require(summary.row(label: "tonic"))
    #expect(tonic.coveredISICount == 4)
    #expect(tonic.annotationCount == 1)
    #expect(tonic.trainCount == 1)
    #expect(!tonic.hasIndependentAnnotationReplication)
    #expect(!tonic.hasCrossTrainReplication)
    #expect(summary.skippedAnnotationCount == 1)

    let proposal = LearnedManualThresholdBuilder.build(from: summary)
    let contribution = try #require(
        proposal.contributions.first { $0.sourceLabel == "tonic" && $0.field == "isi_lower_sec" }
    )
    #expect(contribution.annotationCount == 1)
    #expect(abs(contribution.confidence - (1.0 / 7.0)) < 1e-12)
}

// MARK: - 5. Version folding and overlap ownership prevent pseudoreplication.

@Test
func calibrationUsesOnlyLatestVersionOfSameAnnotationID() throws {
    let t = train("versioned", [0, 0.04, 0.08, 0.12, 0.16])
    let sharedID = UUID(uuidString: "11111111-aaaa-aaaa-aaaa-aaaaaaaaaaaa")!
    let oldTonic = annotation(
        "versioned", .tonic, 0, 0.16, id: sharedID,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let newPause = annotation(
        "versioned", .pause, 0.04, 0.12, id: sharedID,
        updatedAt: Date(timeIntervalSince1970: 300)
    )

    for annotations in [[oldTonic, newPause], [newPause, oldTonic]] {
        let summary = ManualAnnotationCalibrationSummarizer.summarize(
            trains: [t], annotations: annotations, minValidISISeconds: 0.001
        )
        #expect(summary.row(label: "tonic") == nil)
        let pause = try #require(summary.row(label: "pause"))
        #expect(pause.annotationCount == 1)
        #expect(pause.coveredISICount == 2)
        #expect(summary.skippedAnnotationCount == 1)
    }
}

@Test
func calibrationUsesLatestCrossTrainVersionOfSameAnnotationID() throws {
    let tA = train("move-a", [0, 0.04, 0.08])
    let tB = train("move-b", [0, 0.20, 0.40])
    let sharedID = UUID(uuidString: "22222222-aaaa-aaaa-aaaa-aaaaaaaaaaaa")!
    let oldA = annotation(
        "move-a", .tonic, 0, 0.08, id: sharedID,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let movedB = annotation(
        "move-b", .pause, 0, 0.40, id: sharedID,
        updatedAt: Date(timeIntervalSince1970: 300)
    )
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [tA, tB], annotations: [oldA, movedB], minValidISISeconds: 0.001
    )

    #expect(summary.row(label: "tonic") == nil)
    let pause = try #require(summary.row(label: "pause"))
    #expect(pause.annotationCount == 1)
    #expect(pause.trainCount == 1)
    #expect(pause.coveredISICount == 2)
    #expect(abs((pause.medianISISeconds ?? 0) - 0.20) < 1e-12)
    #expect(summary.skippedAnnotationCount == 1)
}

@Test
func fullyOverlappingSameLabelAnnotationsCountEvidenceOnce() throws {
    let t = train("same-label", [0, 0.04, 0.08, 0.12, 0.16])
    let older = annotation(
        "same-label", .tonic, 0, 0.16,
        id: UUID(uuidString: "33333333-aaaa-aaaa-aaaa-aaaaaaaaaaaa")!,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let newer = annotation(
        "same-label", .tonic, 0, 0.16,
        id: UUID(uuidString: "44444444-aaaa-aaaa-aaaa-aaaaaaaaaaaa")!,
        updatedAt: Date(timeIntervalSince1970: 300)
    )
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [t], annotations: [newer, older], minValidISISeconds: 0.001
    )

    let tonic = try #require(summary.row(label: "tonic"))
    #expect(tonic.coveredISICount == 4)
    #expect(tonic.annotationCount == 1)
    #expect(!tonic.hasIndependentAnnotationReplication)
    #expect(summary.skippedAnnotationCount == 1)
    let proposal = LearnedManualThresholdBuilder.build(from: summary)
    let contribution = try #require(
        proposal.contributions.first { $0.sourceLabel == "tonic" && $0.field == "isi_lower_sec" }
    )
    #expect(abs(contribution.confidence - (1.0 / 7.0)) < 1e-12)
}

@Test
func partiallyOverlappingSameLabelMarksCollapseToOneEvidenceRun() throws {
    let t = train("partial-same-label", [0, 0.04, 0.08, 0.12, 0.16, 0.20])
    let older = annotation(
        "partial-same-label", .tonic, 0, 0.12,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let newer = annotation(
        "partial-same-label", .tonic, 0.08, 0.20,
        updatedAt: Date(timeIntervalSince1970: 300)
    )
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [t], annotations: [older, newer], minValidISISeconds: 0.001
    )

    let tonic = try #require(summary.row(label: "tonic"))
    #expect(tonic.coveredISICount == 5)
    #expect(tonic.annotationCount == 1)
    #expect(!tonic.hasMultipleEvidenceRuns)
    #expect(summary.skippedAnnotationCount == 0)

    let contribution = try #require(
        LearnedManualThresholdBuilder.build(from: summary).contributions.first {
            $0.sourceLabel == "tonic" && $0.field == "isi_lower_sec"
        }
    )
    #expect(abs(contribution.confidence - (1.0 / 7.0)) < 1e-12)
}

@Test
func conflictingPositiveLabelsPartitionISIValuesByFinalOwnership() throws {
    let t = train("conflict", [0, 0.04, 0.08, 0.12, 0.16])
    let olderTonic = annotation(
        "conflict", .tonic, 0, 0.16,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let newerPause = annotation(
        "conflict", .pause, 0.04, 0.12,
        updatedAt: Date(timeIntervalSince1970: 300)
    )
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [t], annotations: [newerPause, olderTonic], minValidISISeconds: 0.001
    )

    let tonic = try #require(summary.row(label: "tonic"))
    let pause = try #require(summary.row(label: "pause"))
    #expect(tonic.coveredISICount == 2) // ISI 1 and 4 remain tonic-owned
    #expect(pause.coveredISICount == 2) // ISI 2 and 3 are pause-owned
    #expect(tonic.annotationCount == 2) // ISI 1 and 4 are two disjoint resolved tonic runs.
    #expect(pause.annotationCount == 1)
    #expect(tonic.coveredISICount + pause.coveredISICount == 4)
    #expect(summary.skippedAnnotationCount == 0)
}

@Test
func overlapThatLeavesSingletonBurstContributesNoBurstCalibration() throws {
    let t = train("burst-fragment", [0, 0.1, 0.2, 0.3])
    let olderBurst = annotation(
        "burst-fragment", .burst, 0.1, 0.3,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let newerTonic = annotation(
        "burst-fragment", .tonic, 0.2, 0.3,
        updatedAt: Date(timeIntervalSince1970: 300)
    )
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [t], annotations: [olderBurst, newerTonic], minValidISISeconds: 0.001
    )

    #expect(summary.row(label: "burst") == nil)
    #expect(summary.row(label: "burst_family") == nil)
    let tonic = try #require(summary.row(label: "tonic"))
    #expect(tonic.coveredISICount == 1)
    #expect(tonic.annotationCount == 1)
    #expect(summary.skippedAnnotationCount == 1)
}

// MARK: - 6. Sample CV uses the canonical STPDStatistics sample (n-1) semantics.

@Test
func sampleCVUsesSampleVarianceSemantics() {
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [gTrain],
        annotations: [annotation("g", .burst, 0.020, 0.038)],
        minValidISISeconds: 0.001
    )
    // values [0.004,0.006,0.008]: mean 0.006, sample sd 0.002 -> CV 0.3333 (n-1).
    // (population CV would be ~0.2722.)
    #expect(abs((summary.row(label: "burst")?.sampleCV ?? 0) - (1.0 / 3.0)) < 1e-4)
}

// MARK: - 7. Preview usability and replication are distinct evidence dimensions.

@Test
func usabilityUsesISICountWhileReplicationIsReportedSeparately() throws {
    // Tonic gate is >= 4. A tonic annotation covering 3 ISIs is present but not usable.
    let threeISI = train("t3", [0, 0.04, 0.08, 0.12])      // ISI 1..3
    let belowGate = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [threeISI],
        annotations: [annotation("t3", .tonic, 0.0, 0.12)],
        minValidISISeconds: 0.001
    )
    let tonicBelow = belowGate.row(label: "tonic")
    #expect(tonicBelow?.coveredISICount == 3)
    #expect(tonicBelow?.isUsableForCalibration == false)

    // Four ISIs from one annotation/train are enough for a provisional preview, while the row still reports
    // that multiple-run/cross-train evidence is absent.
    let fourISI = train("t4", [0, 0.04, 0.08, 0.12, 0.16])  // ISI 1..4
    let oneAnnotation = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [fourISI],
        annotations: [annotation("t4", .tonic, 0.0, 0.16)],
        minValidISISeconds: 0.001
    )
    #expect(oneAnnotation.row(label: "tonic")?.coveredISICount == 4)
    #expect(oneAnnotation.row(label: "tonic")?.isUsableForCalibration == true)
    #expect(oneAnnotation.row(label: "tonic")?.hasMultipleEvidenceBearingAnnotations == false)
    #expect(oneAnnotation.row(label: "tonic")?.hasIndependentAnnotationReplication == false)
    #expect(oneAnnotation.row(label: "tonic")?.hasCrossTrainReplication == false)

    // Adjacent marks on the same train collapse to one resolved evidence run.
    let sameTrain = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [fourISI],
        annotations: [
            annotation("t4", .tonic, 0.0, 0.08),
            annotation("t4", .tonic, 0.08, 0.16)
        ],
        minValidISISeconds: 0.001
    )
    #expect(sameTrain.row(label: "tonic")?.annotationCount == 1)
    #expect(sameTrain.row(label: "tonic")?.trainCount == 1)
    #expect(sameTrain.row(label: "tonic")?.isUsableForCalibration == true)
    #expect(sameTrain.row(label: "tonic")?.hasMultipleEvidenceRuns == false)
    #expect(sameTrain.row(label: "tonic")?.hasMultipleEvidenceBearingAnnotations == false)
    #expect(sameTrain.row(label: "tonic")?.hasIndependentAnnotationReplication == false)
    #expect(sameTrain.row(label: "tonic")?.hasCrossTrainReplication == false)

    // Two separated runs on one train remain two pieces of evidence, but still are not independent
    // biological replication and do not satisfy the cross-train flag.
    let separatedTrain = train("separated", [0, 0.04, 0.08, 0.12, 0.16, 0.20])
    let separated = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [separatedTrain],
        annotations: [
            annotation("separated", .tonic, 0, 0.08),
            annotation("separated", .tonic, 0.12, 0.20)
        ],
        minValidISISeconds: 0.001
    )
    #expect(separated.row(label: "tonic")?.annotationCount == 2)
    #expect(separated.row(label: "tonic")?.hasMultipleEvidenceRuns == true)
    #expect(separated.row(label: "tonic")?.hasIndependentAnnotationReplication == false)
    #expect(separated.row(label: "tonic")?.hasCrossTrainReplication == false)
    let separatedProposal = LearnedManualThresholdBuilder.build(from: separated)
    let separatedContribution = try #require(
        separatedProposal.contributions.first { $0.sourceLabel == "tonic" && $0.field == "isi_upper_sec" }
    )
    // Two resolved runs from one train improve the quantiles but remain one train-clustered support unit.
    #expect(separatedContribution.evidenceRunCount == 2)
    #expect(separatedContribution.trainCount == 1)
    #expect(abs(separatedContribution.supportScore - (1.0 / 7.0)) < 1e-12)
    #expect(separatedContribution.confidence == separatedContribution.supportScore)

    // Evidence from two trains supports multiple runs and cross-train replication. Biological
    // independence remains unknown because the model has no biological-unit/session identifier.
    let tA = train("tA", [0, 0.04, 0.08])
    let tB = train("tB", [0, 0.04, 0.08])
    let independent = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [tA, tB],
        annotations: [
            annotation("tA", .tonic, 0.0, 0.08),
            annotation("tB", .tonic, 0.0, 0.08)
        ],
        minValidISISeconds: 0.001
    )
    let tonic = independent.row(label: "tonic")
    #expect(tonic?.coveredISICount == 4)
    #expect(tonic?.annotationCount == 2)
    #expect(tonic?.trainCount == 2)
    #expect(tonic?.isUsableForCalibration == true)
    #expect(tonic?.hasMultipleEvidenceBearingAnnotations == true)
    #expect(tonic?.hasIndependentAnnotationReplication == false)
    #expect(tonic?.hasCrossTrainReplication == true)
}

// MARK: - 9. Partial fast-pattern marks do not contribute; partial slow marks do.

@Test
func partialFastPatternMarksDoNotContributeToCalibration() {
    // Spikes 0,0.010,0.014,0.020,0.030; window [0.011,0.013] is strictly inside ISI2.
    let packet = train("p", [0, 0.010, 0.014, 0.020, 0.030])

    // Partial .burst -> strict full-containment fails -> skipped, no burst calibration row.
    let burstSummary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [packet],
        annotations: [annotation("p", .burst, 0.011, 0.013)],
        minValidISISeconds: 0.001
    )
    #expect(burstSummary.row(label: "burst") == nil)
    #expect(burstSummary.row(label: "burst_family") == nil)
    #expect(burstSummary.skippedAnnotationCount == 1)

    // Partial .highFrequencySpiking -> also strict -> skipped.
    let hfSummary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [packet],
        annotations: [annotation("p", .highFrequencySpiking, 0.011, 0.013)],
        minValidISISeconds: 0.001
    )
    #expect(hfSummary.row(label: "high_frequency_spiking") == nil)
    #expect(hfSummary.skippedAnnotationCount == 1)

    // Partial .tonic -> overlap fallback -> still contributes (ISI2 = 0.004).
    let tonicSummary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [packet],
        annotations: [annotation("p", .tonic, 0.011, 0.013)],
        minValidISISeconds: 0.001
    )
    #expect(tonicSummary.row(label: "tonic")?.coveredISICount == 1)
    #expect(tonicSummary.skippedAnnotationCount == 0)
}

// MARK: - Empty + CSV

@Test
func emptyAnnotationsProduceEmptySummary() {
    let summary = ManualAnnotationCalibrationSummarizer.summarize(trains: [gTrain], annotations: [])
    #expect(summary.isEmpty)
    #expect(summary.appliedToDetector == false)
}

@Test
func calibrationCSVHasProvenanceColumnsAndAppliedFalse() throws {
    let summary = ManualAnnotationCalibrationSummarizer.summarize(
        trains: [gTrain],
        annotations: [annotation("g", .burst, 0.020, 0.038)],
        minValidISISeconds: 0.001
    )
    let csv = ManualAnnotationCalibrationCSVExporter.csv(summary: summary)
    let rows = csv.split(separator: "\n").map { $0.split(separator: ",", omittingEmptySubsequences: false).map(String.init) }
    let header = try #require(rows.first)
    #expect(header.contains("label"))
    #expect(header.contains("annotation_count"))
    #expect(header.contains("evidence_run_count"))
    #expect(header.contains("covered_isi_count"))
    #expect(header.contains("q90_ms"))
    #expect(header.contains("applied_to_detector"))
    #expect(header.contains("source"))
    #expect(header.contains("method"))
    #expect(!csv.contains("mm"))
    // Every data row reports applied_to_detector=false and source manual_annotations.
    let appliedIndex = try #require(header.firstIndex(of: "applied_to_detector"))
    let sourceIndex = try #require(header.firstIndex(of: "source"))
    let legacyCountIndex = try #require(header.firstIndex(of: "annotation_count"))
    let evidenceRunIndex = try #require(header.firstIndex(of: "evidence_run_count"))
    #expect(legacyCountIndex == 3)
    #expect(evidenceRunIndex == header.count - 1)
    for row in rows.dropFirst() {
        if appliedIndex < row.count { #expect(row[appliedIndex] == "false") }
        if sourceIndex < row.count { #expect(row[sourceIndex] == "manual_annotations") }
        if legacyCountIndex < row.count, evidenceRunIndex < row.count {
            #expect(row[legacyCountIndex] == row[evidenceRunIndex])
        }
    }
}
