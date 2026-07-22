import Foundation
import Testing
@testable import STPDCore

// Manual annotation projection onto PUBLIC event annotations: a not_burst veto splits/suppresses
// burst-family runs, and a positive manual label suppresses the overlapped auto label (override) —
// without touching detection or the raw audit path.

private let bands = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)

private func burstRun() -> (dataset: SpikeDataset, run: ClassicAnchorDetectionRun, train: SpikeTrain) {
    // 8 short ISIs give a burst with plenty of interior room to veto.
    let isi = Array(repeating: 0.1, count: 3) + Array(repeating: 0.005, count: 8) + Array(repeating: 0.1, count: 3)
    var ts = [0.0]
    for d in isi { ts.append(ts.last! + d) }
    let train = SpikeTrain(name: "b", timestampsSec: ts)
    let dataset = SpikeDataset(name: "burst", sourceDescription: "synthetic", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)
    return (dataset, run, train)
}

private func longestBurst(_ dataset: SpikeDataset, _ run: ClassicAnchorDetectionRun, _ train: SpikeTrain) -> ClassicAnchorEventAnnotation? {
    run.eventAnnotations(in: dataset)
        .filter { ManualAnnotationProjector.burstFamilyLabels.contains($0.label.rawValue) }
        .max { ($0.coveredISIIndices(in: train)?.count ?? 0) < ($1.coveredISIIndices(in: train)?.count ?? 0) }
}

private func autoLabelsByISI(_ dataset: SpikeDataset, _ run: ClassicAnchorDetectionRun, _ train: SpikeTrain) -> [Int: String] {
    var labels: [Int: String] = [:]
    for annotation in run.eventAnnotations(in: dataset) where annotation.trainID == train.id {
        guard let span = annotation.coveredISIIndices(in: train) else { continue }
        for isi in span { labels[isi] = annotation.label.rawValue }
    }
    return labels
}

private func burstAnnotation(
    id: String,
    train: SpikeTrain,
    span: ClosedRange<Int>,
    label: ClassicAnchorLabel = .burst,
    priority: Int = 100,
    decisionPath: String = "test"
) -> ClassicAnchorEventAnnotation {
    let nISI = span.upperBound - span.lowerBound + 1
    let candidate = ClassicAnchorCandidate(
        id: id,
        trainID: train.id,
        trainName: train.name,
        candidateLayer: "test",
        candidateClass: label.rawValue,
        finalLabel: label,
        gateStatus: "pass",
        decisionPath: decisionPath,
        action: "accept",
        score: 1,
        priority: priority,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: span.lowerBound,
        endISIIndex: span.upperBound,
        startSpikeIndex: span.lowerBound,
        endSpikeIndex: span.upperBound + 1,
        nISI: nISI,
        nValidISI: nISI,
        nSpikes: nISI + 1,
        durationSec: Double(nISI) * 0.01,
        intraQ10Sec: 0.01,
        intraQ40Sec: 0.01,
        intraQ50Sec: 0.01,
        intraQ90Sec: 0.01,
        intraQ95Sec: 0.01,
        maxIntraISISec: 0.01,
        meanIntraISISec: 0.01,
        cv: 0,
        lv: 0,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: "burst",
        anchorLockLevel: .lockedClassic,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.02,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
    return ClassicAnchorEventAnnotation(candidate: candidate, train: train)
}

private func projectVeto(
    _ annotations: [ClassicAnchorEventAnnotation], train: SpikeTrain, vetoed: Set<Int>
) -> (annotations: [ClassicAnchorEventAnnotation], vetoSuppressedISICount: Int) {
    ManualAnnotationProjector.projectPublicEventAnnotations(
        annotations,
        vetoedBurstISIsByTrain: [train.id: vetoed],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
}

// MARK: - 1. A vetoed interior ISI splits a burst into two contiguous runs (e.g. 1...5 with 3 vetoed -> 1...2, 4...5).

@Test
func vetoedInteriorISISplitsBurstIntoTwoRuns() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 5)
    let vetoed = span.lowerBound + 2                   // the 3rd ISI of the run (the "ISI 3" case)
    try #require(vetoed > span.lowerBound && vetoed < span.upperBound)

    let result = projectVeto(run.eventAnnotations(in: dataset), train: train, vetoed: [vetoed])
    let runs = result.annotations
        .filter { $0.candidateID == burst.candidateID }
        .compactMap { $0.coveredISIIndices(in: train) }
        .sorted { $0.lowerBound < $1.lowerBound }

    #expect(runs.count == 2)
    #expect(runs.first == span.lowerBound...(vetoed - 1))   // e.g. 1...2
    #expect(runs.last == (vetoed + 1)...span.upperBound)    // e.g. 4...5
    #expect(result.vetoSuppressedISICount == 1)
    #expect(!runs.contains { $0.contains(vetoed) })
}

// MARK: - 2. The veto removes ONLY the covered burst ISI.

@Test
func vetoRemovesOnlyTheCoveredBurstISI() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 5)
    let vetoed = span.lowerBound + 2

    let result = projectVeto(run.eventAnnotations(in: dataset), train: train, vetoed: [vetoed])
    let survivingISIs = Set(
        result.annotations
            .filter { $0.candidateID == burst.candidateID }
            .compactMap { $0.coveredISIIndices(in: train) }
            .flatMap { Array($0) }
    )
    #expect(survivingISIs == Set(span).subtracting([vetoed]))
    #expect(result.vetoSuppressedISICount == 1)
}

@Test
func vetoThatWouldLeaveOneISIBurstFragmentDropsThatFragment() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 4)
    let vetoed = span.lowerBound + 1

    let result = projectVeto(run.eventAnnotations(in: dataset), train: train, vetoed: [vetoed])
    let survivingISIs = Set(
        result.annotations
            .filter { $0.candidateID == burst.candidateID }
            .compactMap { $0.coveredISIIndices(in: train) }
            .flatMap { Array($0) }
    )
    // The left singleton is not a valid burst; the valid right-hand run remains public.
    #expect(!survivingISIs.contains(span.lowerBound))
    #expect(!survivingISIs.contains(vetoed))
    #expect(survivingISIs == Set((vetoed + 1)...span.upperBound))
    #expect(result.vetoSuppressedISICount == 1)
}

// MARK: - 3. End-to-end: a not_burst manual annotation -> projector veto set -> split public annotations.

@Test
func endToEndNotBurstAnnotationSplitsPublicAnnotations() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 4)
    let v = span.lowerBound + 2
    let notBurst = ManualAnnotation(
        trainID: train.id, label: .notBurst,
        startSec: train.timestampsSec[v - 1], endSec: train.timestampsSec[v]
    )
    let projection = ManualAnnotationProjector.project(
        train: train,
        autoLabelsByISI: autoLabelsByISI(dataset, run, train),
        annotations: [notBurst],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )
    let vetoed = projection.autoBurstBlockedByVetoISIs
    try #require(!vetoed.isEmpty)
    try #require(vetoed.allSatisfy { span.contains($0) })

    let result = projectVeto(run.eventAnnotations(in: dataset), train: train, vetoed: vetoed)
    let survivingISIs = Set(
        result.annotations
            .filter { $0.candidateID == burst.candidateID }
            .compactMap { $0.coveredISIIndices(in: train) }
            .flatMap { Array($0) }
    )
    #expect(survivingISIs == Set(span).subtracting(vetoed))
    #expect(result.vetoSuppressedISICount == vetoed.count)
}

// MARK: - 4. Non-burst labels are never removed by a not_burst veto.

@Test
func nonBurstAnnotationsAreNotRemovedByVeto() throws {
    // A regular tonic train yields a non-burst (state) annotation.
    var ts = [0.0]
    for _ in 0..<40 { ts.append(ts.last! + 0.05) }
    let train = SpikeTrain(name: "t", timestampsSec: ts)
    let dataset = SpikeDataset(name: "tonic", sourceDescription: "synthetic", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: bands)

    let nonBurst = run.eventAnnotations(in: dataset, tracks: [.event, .state, .gap])
        .filter { !ManualAnnotationProjector.burstFamilyLabels.contains($0.label.rawValue) }
    try #require(!nonBurst.isEmpty)
    let target = try #require(nonBurst.first)
    let span = try #require(target.coveredISIIndices(in: train))

    // A not_burst veto over every ISI the non-burst annotation covers must leave it unchanged.
    let result = projectVeto(nonBurst, train: train, vetoed: Set(span))
    #expect(result.annotations == nonBurst)
    #expect(result.vetoSuppressedISICount == 0)
}

// MARK: - 5. Positive manual labels override auto labels in the public projection.

@Test
func positiveManualLabelSplitsOverlappedAutoBurst() throws {
    // A positive manual label (lock-suppressed ISIs) splits the auto burst the same way a veto does,
    // but it is NOT counted as a veto suppression.
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 4)
    let covered = span.lowerBound + 2

    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        run.eventAnnotations(in: dataset),
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [train.id: [covered]],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let runs = result.annotations
        .filter { $0.candidateID == burst.candidateID }
        .compactMap { $0.coveredISIIndices(in: train) }
        .sorted { $0.lowerBound < $1.lowerBound }
    #expect(runs.count == 2)
    #expect(!runs.contains { $0.contains(covered) })     // auto no longer drawn under the positive label
    #expect(result.vetoSuppressedISICount == 0)          // positive lock is not a veto
}

@Test
func endToEndPositiveManualLabelOverridesAutoInProjection() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 4)
    let v = span.lowerBound + 2
    // Author a positive tonic over one interior burst ISI.
    let tonic = ManualAnnotation(
        trainID: train.id, label: .tonic,
        startSec: train.timestampsSec[v - 1], endSec: train.timestampsSec[v]
    )
    let projection = ManualAnnotationProjector.project(
        train: train,
        autoLabelsByISI: autoLabelsByISI(dataset, run, train),
        annotations: [tonic],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )
    // The projector's final label is the positive manual one (override mechanism), and the covered
    // auto ISI is lock-suppressed.
    try #require(!projection.autoBlockedByManualLockISIs.isEmpty)
    for isi in projection.autoBlockedByManualLockISIs {
        #expect(projection.finalLabelByISI[isi] == "tonic")
        #expect(projection.manualPositiveLabelByISI[isi] == "tonic")
    }

    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        run.eventAnnotations(in: dataset),
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [train.id: projection.autoBlockedByManualLockISIs],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let survivingISIs = Set(
        result.annotations
            .filter { $0.candidateID == burst.candidateID }
            .compactMap { $0.coveredISIIndices(in: train) }
            .flatMap { Array($0) }
    )
    #expect(survivingISIs == Set(span).subtracting(projection.autoBlockedByManualLockISIs))
    #expect(result.vetoSuppressedISICount == 0)
}

// MARK: - 6. The raw audit path / inputs are unchanged by the projection.

@Test
func rawAuditAndInputsUnchangedByVeto() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.count >= 3)
    let vetoed = span.lowerBound + 1

    let input = run.eventAnnotations(in: dataset)
    let rawAudit = run.eventAnnotations(in: dataset, selectedOnly: false, includeEvidenceOnly: true, exclusiveFinalProjection: false)

    let result = projectVeto(input, train: train, vetoed: [vetoed])

    // The veto changed the PUBLIC output (the vetoed ISI is gone), proving it ran...
    #expect(result.annotations != input)
    #expect(!result.annotations.contains { $0.coveredISIIndices(in: train)?.contains(vetoed) ?? false })
    // ...while the input array and the RAW audit annotations are untouched (full burst span intact).
    #expect(run.eventAnnotations(in: dataset) == input)
    #expect(rawAudit.contains { $0.candidateID == burst.candidateID && ($0.coveredISIIndices(in: train)?.contains(vetoed) ?? false) })
}

// MARK: - 7. No manual effect -> identity.

@Test
func noManualEffectIsIdentity() {
    let (dataset, run, train) = burstRun()
    let input = run.eventAnnotations(in: dataset)
    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        input,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(result.annotations == input)
    #expect(result.vetoSuppressedISICount == 0)
}

@Test
func vetoedAutoSingletonSurvivesWhenAdjacentManualBurstCompletesFinalRun() throws {
    let (dataset, run, train) = burstRun()
    let burst = try #require(longestBurst(dataset, run, train))
    let span = try #require(burst.coveredISIIndices(in: train))
    try #require(span.lowerBound > 1 && span.count >= 4)

    let survivingAutoISI = span.lowerBound
    let vetoedISI = survivingAutoISI + 1
    let manualISI = survivingAutoISI - 1
    let manualBurst = ManualAnnotation(
        trainID: train.id,
        label: .burst,
        startSec: train.timestampsSec[manualISI - 1],
        endSec: train.timestampsSec[manualISI]
    )
    let veto = ManualAnnotation(
        trainID: train.id,
        label: .notBurst,
        startSec: train.timestampsSec[vetoedISI - 1],
        endSec: train.timestampsSec[vetoedISI]
    )
    let projection = ManualAnnotationProjector.project(
        train: train,
        autoLabelsByISI: autoLabelsByISI(dataset, run, train),
        annotations: [manualBurst, veto],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )
    let manualSupport = Set(projection.manualPositiveLabelByISI.compactMap { isi, label in
        ManualAnnotationProjector.burstFamilyLabels.contains(label) ? isi : nil
    })
    #expect(projection.finalLabelByISI[survivingAutoISI] == burst.label.rawValue)
    #expect(manualSupport.contains(manualISI))

    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        run.eventAnnotations(in: dataset),
        vetoedBurstISIsByTrain: [train.id: projection.autoBurstBlockedByVetoISIs],
        lockSuppressedISIsByTrain: [train.id: projection.autoBlockedByManualLockISIs],
        validatedManualBurstSupportISIsByTrain: [train.id: manualSupport],
        trainsByID: [train.id: train]
    )
    let survivingAuto = Set(
        result.annotations
            .filter { $0.candidateID == burst.candidateID }
            .compactMap { $0.coveredISIIndices(in: train) }
            .flatMap(Array.init)
    )
    #expect(survivingAuto.contains(survivingAutoISI))
    #expect(survivingAuto.contains(manualISI))
    #expect(!survivingAuto.contains(vetoedISI))
    #expect(result.annotations.contains { annotation in
        annotation.candidateID == burst.candidateID
            && annotation.decisionPath.contains("manual_projection_includes_manual_burst_support=true")
    })
    #expect(result.annotations
        .filter { ManualAnnotationProjector.burstFamilyLabels.contains($0.label.rawValue) }
        .allSatisfy { ($0.coveredISIIndices(in: train)?.count ?? 0) >= 2 })
    #expect(result.vetoSuppressedISICount == 1)

    let repeated = ManualAnnotationProjector.projectPublicEventAnnotations(
        result.annotations,
        vetoedBurstISIsByTrain: [train.id: projection.autoBurstBlockedByVetoISIs],
        lockSuppressedISIsByTrain: [train.id: projection.autoBlockedByManualLockISIs],
        validatedManualBurstSupportISIsByTrain: [train.id: manualSupport],
        trainsByID: [train.id: train]
    )
    #expect(repeated.annotations == result.annotations)
}

@Test
func adjacentAutoSingletonsAcrossCandidateBoundarySurviveGlobalValidation() {
    let train = SpikeTrain(
        name: "adjacent-auto-candidates",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
    )
    let first = burstAnnotation(id: "burst-a", train: train, span: 1...2)
    let second = burstAnnotation(id: "burst-b", train: train, span: 3...4)

    // Suppression leaves one ISI from each distinct candidate. Individually each clipped event is a
    // singleton, but the final train-level automatic coverage is the valid contiguous run [2...3].
    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        [first, second],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [train.id: [1, 4]],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )

    let projected = result.annotations.filter {
        !Set($0.sourceCandidateIDs).isDisjoint(with: Set(["burst-a", "burst-b"]))
    }
    #expect(projected.count == 1)
    #expect(projected.first?.coveredISIIndices(in: train) == 2...3)
    #expect(projected.first?.sourceCandidateIDs == ["burst-a", "burst-b"])
    #expect(projected.first?.candidateID == "burst-a")
    #expect(projected.first?.decisionPath.contains("manual_projection_source_candidate_count=2") == true)
    #expect(projected.allSatisfy { ($0.coveredISIIndices(in: train)?.count ?? 0) >= 2 })
    #expect(result.vetoSuppressedISICount == 0)
}

@Test
func overlappingCandidatesCountEachVetoedISIOnce() {
    let train = SpikeTrain(
        name: "overlapping-auto-candidates",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06]
    )
    let first = burstAnnotation(id: "burst-a", train: train, span: 1...4)
    let second = burstAnnotation(id: "burst-b", train: train, span: 2...5)

    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        [first, second],
        vetoedBurstISIsByTrain: [train.id: [3]],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )

    #expect(result.vetoSuppressedISICount == 1)
    #expect(!result.annotations.contains {
        $0.coveredISIIndices(in: train)?.contains(3) == true
    })
}

@Test
func manualBurstSupportAloneTriggersPublicComponentRebuild() throws {
    let train = SpikeTrain(
        name: "manual-support-trigger",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 2...3)

    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [train.id: [4]],
        trainsByID: [train.id: train]
    )

    let projected = try #require(result.annotations.first)
    #expect(projected.coveredISIIndices(in: train) == 2...4)
    #expect(projected.automaticSupportISIIndices == [2, 3])
    #expect(projected.decisionPath.contains("manual_projection_includes_manual_burst_support=true"))
}

@Test
func manualBurstSupportCompletesAutomaticSingletonWithoutOtherManualEffect() throws {
    let train = SpikeTrain(
        name: "manual-support-completes-singleton",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 2...2)

    let result = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [train.id: [3]],
        trainsByID: [train.id: train]
    )

    let projected = try #require(result.annotations.first)
    #expect(projected.coveredISIIndices(in: train) == 2...3)
    #expect(projected.automaticSupportISIIndices == [2])

    let removed = ManualAnnotationProjector.projectPublicEventAnnotations(
        result.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(removed.annotations.isEmpty)
}

@Test
func removingManualBurstSupportRestoresAutomaticGeometryOnReprojection() throws {
    let train = SpikeTrain(
        name: "manual-support-removal",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 2...3)
    let withManual = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [train.id: [4]],
        trainsByID: [train.id: train]
    )
    let extended = try #require(withManual.annotations.first)
    #expect(extended.coveredISIIndices(in: train) == 2...4)

    let removed = ManualAnnotationProjector.projectPublicEventAnnotations(
        withManual.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )

    let restored = try #require(removed.annotations.first)
    #expect(restored.coveredISIIndices(in: train) == 2...3)
    #expect(restored.automaticSupportISIIndices == [2, 3])
    let tokens = restored.decisionPath.split(separator: ";").map(String.init)
    #expect(tokens.contains("manual_projection_includes_manual_burst_support=false"))
    #expect(!tokens.contains("manual_projection_includes_manual_burst_support=true"))
}

@Test
func mixedBurstFamilyComponentUsesConservativePossibleAuthority() throws {
    let train = SpikeTrain(
        name: "mixed-burst-authority",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
    )

    func projected(possiblePriority: Int, canonicalPriority: Int) throws -> ClassicAnchorEventAnnotation {
        let possible = burstAnnotation(
            id: "possible",
            train: train,
            span: 2...2,
            label: .possibleBurst,
            priority: possiblePriority
        )
        let canonical = burstAnnotation(
            id: "canonical",
            train: train,
            span: 3...3,
            label: .burst,
            priority: canonicalPriority
        )
        let result = ManualAnnotationProjector.projectPublicEventAnnotations(
            [canonical, possible],
            vetoedBurstISIsByTrain: [:],
            lockSuppressedISIsByTrain: [train.id: [1]],
            validatedManualBurstSupportISIsByTrain: [:],
            trainsByID: [train.id: train]
        )
        return try #require(result.annotations.first)
    }

    let possibleWinsPriority = try projected(possiblePriority: 1_000, canonicalPriority: 10)
    let canonicalWinsPriority = try projected(possiblePriority: 10, canonicalPriority: 1_000)
    #expect(possibleWinsPriority.label == .possibleBurst)
    #expect(canonicalWinsPriority.label == .possibleBurst)
    #expect(possibleWinsPriority.sourceCandidateIDs == ["canonical", "possible"])
    #expect(canonicalWinsPriority.sourceCandidateIDs == ["canonical", "possible"])
    #expect(possibleWinsPriority.decisionPath.contains(
        "manual_projection_burst_authority=possible_if_any_source_possible"
    ))
}

@Test
func projectedComponentIdentityIsIndependentOfSameCandidateFragmentOrder() throws {
    let train = SpikeTrain(
        name: "stable-component-identity",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06]
    )
    let original = burstAnnotation(id: "burst-a", train: train, span: 1...5)
    let left = try #require(original.clipped(to: 1...2, in: train))
    let right = try #require(original.clipped(to: 4...5, in: train))

    func project(_ annotations: [ClassicAnchorEventAnnotation]) throws -> ClassicAnchorEventAnnotation {
        let result = ManualAnnotationProjector.projectPublicEventAnnotations(
            annotations,
            vetoedBurstISIsByTrain: [:],
            lockSuppressedISIsByTrain: [:],
            validatedManualBurstSupportISIsByTrain: [train.id: [3]],
            trainsByID: [train.id: train]
        )
        return try #require(result.annotations.first)
    }

    let forward = try project([left, right])
    let reversed = try project([right, left])
    #expect(forward == reversed)
    #expect(forward.id == "burst-a-public-burst-isi-1-5")
    #expect(forward.automaticSupportISIIndices == [1, 2, 4, 5])
}

@Test
func removingManualBridgeRestoresEachSourceAuthorityAndIdentity() throws {
    let train = SpikeTrain(
        name: "mixed-authority-manual-bridge",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06]
    )
    let canonical = burstAnnotation(
        id: "canonical",
        train: train,
        span: 1...2,
        label: .burst,
        priority: 200,
        decisionPath: "canonical_origin=true"
    )
    let possible = burstAnnotation(
        id: "possible",
        train: train,
        span: 4...5,
        label: .possibleBurst,
        priority: 100,
        decisionPath: "possible_origin=true"
    )

    let joined = ManualAnnotationProjector.projectPublicEventAnnotations(
        [canonical, possible],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [train.id: [3]],
        trainsByID: [train.id: train]
    )
    let merged = try #require(joined.annotations.first)
    #expect(joined.annotations.count == 1)
    #expect(merged.coveredISIIndices(in: train) == 1...5)
    #expect(merged.label == .possibleBurst)
    #expect(merged.candidateID == "canonical")
    #expect(merged.sourceCandidateIDs == ["canonical", "possible"])
    #expect(merged.automaticEventSources.map(\.candidateID) == ["canonical", "possible"])
    #expect(merged.automaticEventSources.first { $0.candidateID == "canonical" }?.supportISIIndices == [1, 2])
    #expect(merged.automaticEventSources.first { $0.candidateID == "possible" }?.supportISIIndices == [4, 5])

    let separated = ManualAnnotationProjector.projectPublicEventAnnotations(
        joined.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(separated.annotations.count == 2)

    let restoredCanonical = try #require(separated.annotations.first {
        $0.sourceCandidateIDs == ["canonical"]
    })
    #expect(restoredCanonical.coveredISIIndices(in: train) == 1...2)
    #expect(restoredCanonical.label == .burst)
    #expect(restoredCanonical.candidateID == "canonical")
    #expect(restoredCanonical.automaticSupportISIIndices == [1, 2])
    #expect(restoredCanonical.automaticEventSources.map(\.label) == [.burst])
    #expect(restoredCanonical.decisionPath.contains("canonical_origin=true"))
    #expect(!restoredCanonical.decisionPath.contains("possible_origin=true"))

    let restoredPossible = try #require(separated.annotations.first {
        $0.sourceCandidateIDs == ["possible"]
    })
    #expect(restoredPossible.coveredISIIndices(in: train) == 4...5)
    #expect(restoredPossible.label == .possibleBurst)
    #expect(restoredPossible.candidateID == "possible")
    #expect(restoredPossible.automaticSupportISIIndices == [4, 5])
    #expect(restoredPossible.automaticEventSources.map(\.label) == [.possibleBurst])
    #expect(restoredPossible.decisionPath.contains("possible_origin=true"))
    #expect(!restoredPossible.decisionPath.contains("canonical_origin=true"))

    let repeated = ManualAnnotationProjector.projectPublicEventAnnotations(
        separated.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(repeated.annotations == separated.annotations)
}

@Test
func removingVetoRestoresSuppressedAutomaticBurstSupport() throws {
    let train = SpikeTrain(
        name: "reversible-veto",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 1...3)

    let vetoed = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automatic],
        vetoedBurstISIsByTrain: [train.id: [1]],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let clipped = try #require(vetoed.annotations.first)
    #expect(vetoed.annotations.count == 1)
    #expect(clipped.coveredISIIndices(in: train) == 2...3)
    #expect(clipped.automaticSupportISIIndices == [2, 3])
    #expect(clipped.automaticEventSources.first?.supportISIIndices == [1, 2, 3])

    let restored = ManualAnnotationProjector.projectPublicEventAnnotations(
        vetoed.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let event = try #require(restored.annotations.first)
    #expect(restored.annotations.count == 1)
    #expect(event.coveredISIIndices(in: train) == 1...3)
    #expect(event.automaticSupportISIIndices == [1, 2, 3])
    #expect(event.automaticEventSources.first?.supportISIIndices == [1, 2, 3])

    let repeated = ManualAnnotationProjector.projectPublicEventAnnotations(
        restored.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(repeated.annotations == restored.annotations)
}

@Test
func removingPositiveLockRestoresSuppressedNonBurstSupport() throws {
    let train = SpikeTrain(
        name: "reversible-non-burst-lock",
        timestampsSec: [0, 0.05, 0.10, 0.15, 0.20]
    )
    let automatic = burstAnnotation(
        id: "tonic-a",
        train: train,
        span: 1...3,
        label: .tonic,
        decisionPath: "tonic_origin=true"
    )

    let locked = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [train.id: [1]],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let clipped = try #require(locked.annotations.first)
    #expect(locked.annotations.count == 1)
    #expect(clipped.coveredISIIndices(in: train) == 2...3)
    #expect(clipped.label == .tonic)
    #expect(clipped.automaticSupportISIIndices == [2, 3])
    #expect(clipped.automaticEventSources.first?.supportISIIndices == [1, 2, 3])
    #expect(clipped.decisionPath.contains("manual_projection_non_burst_lock=true"))

    let restored = ManualAnnotationProjector.projectPublicEventAnnotations(
        locked.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let event = try #require(restored.annotations.first)
    #expect(restored.annotations.count == 1)
    #expect(event.coveredISIIndices(in: train) == 1...3)
    #expect(event.label == .tonic)
    #expect(event.decisionPath.contains("tonic_origin=true"))

    let repeated = ManualAnnotationProjector.projectPublicEventAnnotations(
        restored.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(repeated.annotations == restored.annotations)
}

@Test
func manualReprojectionDoesNotRestoreEvidenceRemovedByAutomaticClipping() throws {
    let train = SpikeTrain(
        name: "automatic-boundary",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06]
    )
    let raw = burstAnnotation(id: "burst-a", train: train, span: 1...5)
    let automaticPublic = try #require(raw.clipped(to: 2...4, in: train))
    #expect(automaticPublic.automaticEventSources.first?.supportISIIndices == [2, 3, 4])

    let vetoed = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automaticPublic],
        vetoedBurstISIsByTrain: [train.id: [2]],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(vetoed.annotations.first?.coveredISIIndices(in: train) == 3...4)

    let restored = ManualAnnotationProjector.projectPublicEventAnnotations(
        vetoed.annotations,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let event = try #require(restored.annotations.first)
    #expect(event.coveredISIIndices(in: train) == 2...4)
    #expect(event.automaticSupportISIIndices == [2, 3, 4])
    #expect(!event.automaticSupportISIIndices.contains(1))
    #expect(!event.automaticSupportISIIndices.contains(5))
}

@Test
func stateArchiveRestoresBurstAfterCompleteVeto() throws {
    let train = SpikeTrain(
        name: "complete-burst-veto",
        timestampsSec: [0, 0.01, 0.02]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 1...2)

    let vetoed = ManualAnnotationProjector.projectPublicEventState(
        [automatic],
        vetoedBurstISIsByTrain: [train.id: [1, 2]],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(vetoed.annotations.isEmpty)
    #expect(vetoed.vetoSuppressedISICount == 2)
    #expect(vetoed.automaticSourceArchive.count == 1)
    #expect(vetoed.automaticSourceArchive.first?.supportISIIndices == [1, 2])

    let restored = ManualAnnotationProjector.projectPublicEventState(
        vetoed,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let event = try #require(restored.annotations.first)
    #expect(restored.annotations.count == 1)
    #expect(event.coveredISIIndices(in: train) == 1...2)
    #expect(event.automaticSupportISIIndices == [1, 2])
    #expect(restored.automaticSourceArchive == vetoed.automaticSourceArchive)

    let repeated = ManualAnnotationProjector.projectPublicEventState(
        restored,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(repeated == restored)
}

@Test
func stateArchiveRestoresNonBurstAfterCompletePositiveLock() throws {
    let train = SpikeTrain(
        name: "complete-tonic-lock",
        timestampsSec: [0, 0.05, 0.10]
    )
    let automatic = burstAnnotation(
        id: "tonic-a",
        train: train,
        span: 1...2,
        label: .tonic,
        decisionPath: "tonic_origin=true"
    )

    let locked = ManualAnnotationProjector.projectPublicEventState(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [train.id: [1, 2]],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(locked.annotations.isEmpty)
    #expect(locked.automaticSourceArchive.count == 1)
    #expect(locked.automaticSourceArchive.first?.supportISIIndices == [1, 2])

    let restored = ManualAnnotationProjector.projectPublicEventState(
        locked,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let event = try #require(restored.annotations.first)
    #expect(restored.annotations.count == 1)
    #expect(event.coveredISIIndices(in: train) == 1...2)
    #expect(event.label == .tonic)
    #expect(event.decisionPath.contains("tonic_origin=true"))
    #expect(!event.decisionPath.contains("manual_projection_"))

    let repeated = ManualAnnotationProjector.projectPublicEventState(
        restored,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(repeated == restored)
}

@Test
func removingManualBurstSupportRefreshesUnchangedGeometryProvenance() throws {
    let train = SpikeTrain(
        name: "manual-support-provenance",
        timestampsSec: [0, 0.01, 0.02]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 1...2)

    let supported = ManualAnnotationProjector.projectPublicEventState(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [train.id: [2]],
        trainsByID: [train.id: train]
    )
    let withManual = try #require(supported.annotations.first)
    #expect(withManual.coveredISIIndices(in: train) == 1...2)
    #expect(withManual.decisionPath.split(separator: ";").contains {
        $0 == "manual_projection_includes_manual_burst_support=true"
    })

    let removed = ManualAnnotationProjector.projectPublicEventState(
        supported,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let restored = try #require(removed.annotations.first)
    let tokens = restored.decisionPath.split(separator: ";").map(String.init)
    #expect(restored.coveredISIIndices(in: train) == 1...2)
    #expect(tokens.contains("manual_projection_includes_manual_burst_support=false"))
    #expect(!tokens.contains("manual_projection_includes_manual_burst_support=true"))

    let repeated = ManualAnnotationProjector.projectPublicEventState(
        removed,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(repeated == removed)
}

@Test
func stateArchiveCanonicalizesSameCandidateFragmentsAcrossManualBridge() throws {
    let train = SpikeTrain(
        name: "archive-fragment-bridge",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 1...5)
    let left = try #require(automatic.clipped(to: 1...2, in: train))
    let right = try #require(automatic.clipped(to: 4...5, in: train))

    let joined = ManualAnnotationProjector.projectPublicEventState(
        [left, right],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [train.id: [3]],
        trainsByID: [train.id: train]
    )
    let joinedEvent = try #require(joined.annotations.first)
    #expect(joined.annotations.count == 1)
    #expect(joinedEvent.coveredISIIndices(in: train) == 1...5)
    #expect(joined.automaticSourceArchive.count == 1)
    #expect(joined.automaticSourceArchive.first?.supportISIIndices == [1, 2, 4, 5])

    let joinedAgain = ManualAnnotationProjector.projectPublicEventState(
        joined,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [train.id: [3]],
        trainsByID: [train.id: train]
    )
    #expect(joinedAgain == joined)

    let separated = ManualAnnotationProjector.projectPublicEventState(
        joinedAgain,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let separatedSpans = separated.annotations
        .compactMap { $0.coveredISIIndices(in: train) }
        .sorted { $0.lowerBound < $1.lowerBound }
    #expect(separatedSpans == [1...2, 4...5])
    #expect(separated.automaticSourceArchive.count == 1)
    #expect(separated.automaticSourceArchive.first?.supportISIIndices == [1, 2, 4, 5])

    let separatedAgain = ManualAnnotationProjector.projectPublicEventState(
        separated,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(separatedAgain == separated)
}

@Test
func stateArchiveDefersRestorationUntilMissingTrainGeometryReturns() throws {
    let train = SpikeTrain(
        name: "archive-missing-train",
        timestampsSec: [0, 0.01, 0.02]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 1...2)
    let vetoed = ManualAnnotationProjector.projectPublicEventState(
        [automatic],
        vetoedBurstISIsByTrain: [train.id: [1, 2]],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(vetoed.annotations.isEmpty)
    #expect(vetoed.automaticSourceArchive.count == 1)

    let deferred = ManualAnnotationProjector.projectPublicEventState(
        vetoed,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [:]
    )
    #expect(deferred.annotations.isEmpty)
    #expect(deferred.automaticSourceArchive == vetoed.automaticSourceArchive)

    let restored = ManualAnnotationProjector.projectPublicEventState(
        deferred,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let restoredEvent = try #require(restored.annotations.first)
    #expect(restored.annotations.count == 1)
    #expect(restoredEvent.coveredISIIndices(in: train) == 1...2)
    #expect(restored.automaticSourceArchive == vetoed.automaticSourceArchive)
}

@Test
func stateArchiveNoOpPreservesPreFragmentedAutomaticAnnotations() throws {
    let train = SpikeTrain(
        name: "archive-fragment-no-op",
        timestampsSec: [0, 0.01, 0.02, 0.03, 0.04, 0.05]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 1...5)
    let left = try #require(automatic.clipped(to: 1...2, in: train))
    let right = try #require(automatic.clipped(to: 4...5, in: train))

    let initial = ManualAnnotationProjector.projectPublicEventState(
        [left, right],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(initial.annotations == [left, right])
    #expect(initial.automaticSourceArchive.count == 1)
    #expect(initial.automaticSourceArchive.first?.supportISIIndices == [1, 2, 4, 5])

    let repeated = ManualAnnotationProjector.projectPublicEventState(
        initial,
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    #expect(repeated == initial)
    #expect(repeated.annotations.map(\.id) == [left.id, right.id])
    #expect(repeated.annotations.map(\.automaticEventSources) == [
        left.automaticEventSources,
        right.automaticEventSources
    ])
}

@Test
func legacyPublicEventProjectionCallDefaultsManualBurstSupportToEmpty() {
    let train = SpikeTrain(
        name: "legacy-public-projection",
        timestampsSec: [0, 0.01, 0.02]
    )
    let automatic = burstAnnotation(id: "burst-a", train: train, span: 1...2)

    let legacy = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        trainsByID: [train.id: train]
    )
    let explicit = ManualAnnotationProjector.projectPublicEventAnnotations(
        [automatic],
        vetoedBurstISIsByTrain: [:],
        lockSuppressedISIsByTrain: [:],
        validatedManualBurstSupportISIsByTrain: [:],
        trainsByID: [train.id: train]
    )

    #expect(legacy.annotations == explicit.annotations)
    #expect(legacy.vetoSuppressedISICount == explicit.vetoSuppressedISICount)
}
