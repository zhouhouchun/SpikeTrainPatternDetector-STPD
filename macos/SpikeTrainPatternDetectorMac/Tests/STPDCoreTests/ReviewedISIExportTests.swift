import Foundation
import STPDCore
import Testing

// Reviewed-final per-ISI / per-timestamp export: auto pattern vs reviewed/final pattern, with the
// >= 2 ISI / 3 spike burst-validity rule applied to final (not auto).

private let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)

private func burstRun() -> (dataset: SpikeDataset, run: ClassicAnchorDetectionRun, train: SpikeTrain) {
    let isi = Array(repeating: 0.1, count: 3) + Array(repeating: 0.005, count: 9) + Array(repeating: 0.1, count: 3)
    var ts = [0.0]
    for d in isi { ts.append(ts.last! + d) }
    let train = SpikeTrain(name: "b", timestampsSec: ts)
    let dataset = SpikeDataset(name: "burst", sourceDescription: "synthetic", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    return (dataset, run, train)
}

private func autoAnnotations(_ dataset: SpikeDataset, _ run: ClassicAnchorDetectionRun) -> [ClassicAnchorEventAnnotation] {
    run.eventAnnotations(in: dataset, tracks: [.event, .gap, .state])
}

private func longestBurst(_ dataset: SpikeDataset, _ run: ClassicAnchorDetectionRun, _ train: SpikeTrain) -> ClassicAnchorEventAnnotation? {
    autoAnnotations(dataset, run)
        .filter { ManualAnnotationProjector.burstFamilyLabels.contains($0.label.rawValue) }
        .max { ($0.coveredISIIndices(in: train)?.count ?? 0) < ($1.coveredISIIndices(in: train)?.count ?? 0) }
}

private func projection(veto: Set<Int> = [], positive: [Int: String] = [:]) -> ManualAnnotationProjection {
    ManualAnnotationProjection(
        manualPositiveLabelByISI: positive,
        manualNegativeVetoByISI: veto,
        finalLabelByISI: [:],
        effectiveAutoLabelByISI: [:],
        autoBlockedByManualLockISIs: Set(positive.keys),
        autoBurstBlockedByVetoISIs: veto,
        positiveCoverageISICount: positive.count,
        negativeCoverageISICount: veto.count,
        skippedAnnotationCount: 0
    )
}

private func row(_ rows: [ReviewedISIExportRow], _ train: SpikeTrain, isi: Int) -> ReviewedISIExportRow? {
    rows.first { $0.trainID == train.id && $0.isiIndex == isi }
}

private func isBurst(_ label: String) -> Bool { ManualAnnotationProjector.burstFamilyLabels.contains(label) }

// MARK: - 1. Per-timestamp indexing contract.

@Test
func perTimestampRowsFollowTheISIIndexingContract() {
    let (dataset, run, train) = burstRun()
    let rows = ReviewedISIExportBuilder.build(dataset: dataset, autoAnnotations: autoAnnotations(dataset, run), projectionsByTrain: [:])
    #expect(rows.count == train.spikeCount)                      // one row per spike

    let first = rows[0]
    #expect(first.spikeIndex == 0 && first.isiIndex == 0)
    #expect(first.isiSec == 0 && first.autoPattern.isEmpty && first.finalPattern.isEmpty)
    #expect(first.timestampSec == train.timestampsSec[0])

    for k in 1..<train.spikeCount {
        let r = rows[k]
        #expect(r.spikeIndex == k && r.isiIndex == k)
        #expect(abs(r.timestampSec - train.timestampsSec[k]) < 1e-12)
        #expect(abs(r.isiSec - (train.timestampsSec[k] - train.timestampsSec[k - 1])) < 1e-12)
    }
}

// MARK: - 2. Veto splits a burst into two valid (>= 2 ISI) parts.

@Test
func vetoSplitsBurstIntoTwoValidParts() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 5)
    let s = span.lowerBound, e = span.upperBound
    let veto = s + 2                                            // leaves [s..s+1] (2) and [s+3..e] (>=2)

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: autoAnnotations(dataset, run),
        projectionsByTrain: [train.id: projection(veto: [veto])]
    )
    // auto_pattern stays burst across the whole span.
    for isi in span { #expect(isBurst(row(rows, train, isi: isi)?.autoPattern ?? "")) }
    // final_pattern: burst on the two surviving runs, none at the vetoed ISI.
    for isi in [s, s + 1] { #expect(isBurst(row(rows, train, isi: isi)?.finalPattern ?? "")) }
    for isi in (s + 3)...e { #expect(isBurst(row(rows, train, isi: isi)?.finalPattern ?? "")) }
    let vetoRow = try #require(row(rows, train, isi: veto))
    #expect(!isBurst(vetoRow.finalPattern) && vetoRow.finalPattern.isEmpty)
    #expect(vetoRow.finalSource == "manual_veto_removed")
    #expect(vetoRow.manualVetoSuppressed == true)
}

// MARK: - 3. Veto leaving one-ISI fragments => no final burst (the >=2 rule).

@Test
func vetoLeavingOneISIFragmentsRemovesFinalBurst() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 3)
    let s = span.lowerBound, e = span.upperBound
    // Veto s+1 and everything from s+3..e -> surviving auto-burst is {s} and {s+2}, both one-ISI.
    var veto: Set<Int> = [s + 1]
    if s + 3 <= e { veto.formUnion((s + 3)...e) }

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: autoAnnotations(dataset, run),
        projectionsByTrain: [train.id: projection(veto: veto)]
    )
    // auto remains burst on the original span.
    for isi in span { #expect(isBurst(row(rows, train, isi: isi)?.autoPattern ?? "")) }
    // NO final burst anywhere in the span — the two surviving fragments are one-ISI bursts.
    for isi in span { #expect(!isBurst(row(rows, train, isi: isi)?.finalPattern ?? "")) }
    #expect(row(rows, train, isi: s)?.finalSource == "invalid_single_isi_burst_fragment")
    #expect(row(rows, train, isi: s + 2)?.finalSource == "invalid_single_isi_burst_fragment")
    #expect(row(rows, train, isi: s)?.manualVetoSuppressed == false)
    #expect(row(rows, train, isi: s + 2)?.manualVetoSuppressed == false)
}

// MARK: - 4. Positive manual label inside an auto burst.

@Test
func positiveManualLabelInsideAutoBurst() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 5)
    let s = span.lowerBound, e = span.upperBound
    let covered = s + 2

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: autoAnnotations(dataset, run),
        projectionsByTrain: [train.id: projection(positive: [covered: "tonic"])]
    )
    for isi in span { #expect(isBurst(row(rows, train, isi: isi)?.autoPattern ?? "")) }   // auto unchanged
    let coveredRow = try #require(row(rows, train, isi: covered))
    #expect(coveredRow.finalPattern == "tonic")
    #expect(coveredRow.finalSource == "manual_positive")
    // Surrounding final burst fragments obey the >= 2 ISI rule (each side has >= 2 ISIs here).
    for isi in [s, s + 1] { #expect(isBurst(row(rows, train, isi: isi)?.finalPattern ?? "")) }
    for isi in (s + 3)...e { #expect(isBurst(row(rows, train, isi: isi)?.finalPattern ?? "")) }
}

@Test
func singleISIManualBurstIsInvalidButSingleISITonicRemainsValid() throws {
    let (dataset, _, train) = burstRun()
    let covered = 1

    let burstRows = ReviewedISIExportBuilder.build(
        dataset: dataset,
        autoAnnotations: [],
        projectionsByTrain: [train.id: projection(positive: [covered: "burst"])]
    )
    let manualBurst = try #require(row(burstRows, train, isi: covered))
    #expect(manualBurst.finalPattern.isEmpty)
    #expect(manualBurst.finalSource == "invalid_single_isi_burst_fragment")
    #expect(manualBurst.manualVetoSuppressed == false)

    let tonicRows = ReviewedISIExportBuilder.build(
        dataset: dataset,
        autoAnnotations: [],
        projectionsByTrain: [train.id: projection(positive: [covered: "tonic"])]
    )
    let manualTonic = try #require(row(tonicRows, train, isi: covered))
    #expect(manualTonic.finalPattern == "tonic")
    #expect(manualTonic.finalSource == "manual_positive")
    #expect(manualTonic.manualVetoSuppressed == false)
}

@Test
func adjacentManualAndAutoBurstISIsFormOneValidFinalFamilyRun() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 2)

    let manualISI: Int
    let adjacentAutoISI: Int
    if span.lowerBound > 1 {
        manualISI = span.lowerBound - 1
        adjacentAutoISI = span.lowerBound
    } else {
        try #require(span.upperBound + 1 < train.spikeCount)
        manualISI = span.upperBound + 1
        adjacentAutoISI = span.upperBound
    }

    var autoLabelsByISI: [Int: String] = [:]
    let automaticAnnotations = autoAnnotations(dataset, run)
    for annotation in automaticAnnotations where annotation.trainID == train.id {
        if let covered = annotation.coveredISIIndices(in: train) {
            for isi in covered { autoLabelsByISI[isi] = annotation.label.rawValue }
        }
    }
    #expect(!isBurst(autoLabelsByISI[manualISI] ?? ""))
    #expect(isBurst(autoLabelsByISI[adjacentAutoISI] ?? ""))

    let manualBurst = ManualAnnotation(
        trainID: train.id,
        label: .burst,
        startSec: train.timestampsSec[manualISI - 1],
        endSec: train.timestampsSec[manualISI]
    )
    let projected = ManualAnnotationProjector.project(
        train: train,
        autoLabelsByISI: autoLabelsByISI,
        annotations: [manualBurst],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true,
        minValidISISeconds: bands.minValidISISec
    )
    #expect(projected.manualPositiveLabelByISI[manualISI] == "burst")
    #expect(projected.skippedAnnotationCount == 0)

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: automaticAnnotations,
        projectionsByTrain: [train.id: projected]
    )

    let manualRow = try #require(row(rows, train, isi: manualISI))
    let autoRow = try #require(row(rows, train, isi: adjacentAutoISI))
    #expect(isBurst(manualRow.finalPattern))
    #expect(manualRow.finalSource == "manual_positive")
    #expect(isBurst(autoRow.finalPattern))
    #expect(autoRow.finalSource == "auto_projected")
}

@Test
func projectionQCFloorKeepsReviewedExportConsistentWithCalibration() throws {
    let train = SpikeTrain(name: "qc-export", timestampsSec: [0, 0.0005, 0.0105])
    let dataset = SpikeDataset(name: "qc-export", sourceDescription: "synthetic", trains: [train])
    let manualBurst = ManualAnnotation(
        trainID: train.id,
        label: .burst,
        startSec: 0,
        endSec: 0.0105
    )
    let projected = ManualAnnotationProjector.project(
        train: train,
        autoLabelsByISI: [:],
        annotations: [manualBurst],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true,
        minValidISISeconds: bands.minValidISISec
    )
    #expect(projected.manualPositiveLabelByISI.isEmpty)
    #expect(projected.finalLabelByISI.isEmpty)
    #expect(projected.skippedAnnotationCount == 1)
    #expect(projected.finalBurstISIsRemovedByStructuralMinimum == [2])

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset,
        autoAnnotations: [],
        projectionsByTrain: [train.id: projected]
    )
    for isi in 1...2 {
        let reviewed = try #require(row(rows, train, isi: isi))
        #expect(reviewed.finalPattern.isEmpty)
        #expect(reviewed.finalSource == "none")
    }
}

// MARK: - 5. Non-burst labels are not removed by a not_burst veto.

@Test
func nonBurstLabelsAreNotRemovedByVeto() throws {
    var ts = [0.0]
    for _ in 0..<40 { ts.append(ts.last! + 0.05) }
    let train = SpikeTrain(name: "t", timestampsSec: ts)
    let dataset = SpikeDataset(name: "tonic", sourceDescription: "synthetic", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)

    let nonBurst = try #require(autoAnnotations(dataset, run)
        .first { !ManualAnnotationProjector.burstFamilyLabels.contains($0.label.rawValue) })
    let span = try #require(nonBurst.coveredISIIndices(in: train))

    // Even with every ISI in the (burst) veto set, a non-burst label is preserved in final.
    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: autoAnnotations(dataset, run),
        projectionsByTrain: [train.id: projection(veto: Set(span))]
    )
    for isi in span {
        let r = try #require(row(rows, train, isi: isi))
        #expect(r.finalPattern == nonBurst.label.rawValue)
        #expect(r.finalSource == "auto_projected")
        #expect(r.manualVetoSuppressed == false)
    }
}

// MARK: - 6. The existing raw candidate export is unchanged.

@Test
func existingRawCandidateExportIsUnchanged() {
    let (dataset, run, _) = burstRun()
    let candidateCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset, run: run, annotations: [], reviewStatuses: [:], includeUnselectedCandidates: true
    )
    let candidateHeader = candidateCSV.split(separator: "\n").first.map { $0.split(separator: ",").map(String.init) } ?? []
    // The candidate exporter still has its per-candidate schema (one row per candidate, not per ISI).
    #expect(candidateHeader.contains("candidate_id"))
    #expect(candidateHeader.contains("label"))

    // The reviewed export is a distinct per-ISI schema.
    let reviewed = ReviewedISIExportCSVExporter.headers
    #expect(reviewed.first == "train_id")
    #expect(reviewed.contains("auto_pattern") && reviewed.contains("final_pattern") && reviewed.contains("final_source"))
    #expect(!reviewed.contains("candidate_id"))
}

// MARK: - 6b. Manual review rejection: auto_pattern preserved, final_pattern dropped.

@Test
func reviewRejectedBurstKeepsAutoButDropsFinal() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: autoAnnotations(dataset, run),
        projectionsByTrain: [:],
        reviewRejectedCandidateIDs: [burst.candidateID]
    )
    for isi in span {
        let r = try #require(row(rows, train, isi: isi))
        #expect(isBurst(r.autoPattern))                          // raw auto preserved
        #expect(r.autoCandidateID == burst.candidateID)          // traceability kept
        #expect(r.finalPattern.isEmpty)                          // final dropped
        #expect(r.finalSource == "manual_review_rejected")
    }
}

@Test
func positiveManualLabelOverridesRejectedAutoCandidate() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 3)
    let covered = span.lowerBound + 1

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: autoAnnotations(dataset, run),
        projectionsByTrain: [train.id: projection(positive: [covered: "tonic"])],
        reviewRejectedCandidateIDs: [burst.candidateID]
    )
    // The positive manual label still appears even though the underlying auto candidate was rejected.
    let coveredRow = try #require(row(rows, train, isi: covered))
    #expect(isBurst(coveredRow.autoPattern))
    #expect(coveredRow.finalPattern == "tonic")
    #expect(coveredRow.finalSource == "manual_positive")
    // Every other in-span ISI is review-rejected in final (and never burst).
    for isi in span where isi != covered {
        let r = try #require(row(rows, train, isi: isi))
        #expect(r.finalPattern.isEmpty && r.finalSource == "manual_review_rejected")
    }
}

// MARK: - 7. End-to-end: a real not_burst annotation -> project -> reviewed export splits the burst.

@Test
func endToEndNotBurstAnnotationDrivesFinalSplit() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 5)
    let v = span.lowerBound + 2
    let notBurst = ManualAnnotation(
        trainID: train.id, label: .notBurst,
        startSec: train.timestampsSec[v - 1], endSec: train.timestampsSec[v]
    )
    var auto: [Int: String] = [:]
    for annotation in autoAnnotations(dataset, run) where annotation.trainID == train.id {
        if let s = annotation.coveredISIIndices(in: train) { for i in s { auto[i] = annotation.label.rawValue } }
    }
    let proj = ManualAnnotationProjector.project(
        train: train, autoLabelsByISI: auto, annotations: [notBurst],
        honorManualLock: true, manualNegativeLabelsEnabled: true
    )
    try #require(!proj.autoBurstBlockedByVetoISIs.isEmpty)

    let rows = ReviewedISIExportBuilder.build(
        dataset: dataset, autoAnnotations: autoAnnotations(dataset, run),
        projectionsByTrain: [train.id: proj]
    )
    for isi in proj.autoBurstBlockedByVetoISIs {
        let r = try #require(row(rows, train, isi: isi))
        #expect(r.autoPattern.isEmpty == false)            // auto traceability kept
        #expect(r.finalSource == "manual_veto_removed")
        #expect(r.finalPattern.isEmpty)
        #expect(r.manualVetoSuppressed == true)
    }
}
