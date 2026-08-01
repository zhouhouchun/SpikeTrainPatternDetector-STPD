import Foundation
import STPDCore
import Testing

// Phase 1C: explainability (current → learned diffs) + provenance string. Pure logic.

private let burstFamily = ManualAnnotationCalibrationSummarizer.familyBurst

private func calibRow(
    label: String,
    isFamily: Bool = false,
    annotationCount: Int = 5,
    trainCount: Int = 2,
    coveredISICount: Int = 30,
    q10: Double? = nil,
    q90: Double? = nil,
    q95: Double? = nil
) -> ManualAnnotationCalibrationLabelSummary {
    ManualAnnotationCalibrationLabelSummary(
        label: label, displayName: label, isPositive: true, isFamily: isFamily,
        annotationCount: annotationCount, trainCount: trainCount, coveredISICount: coveredISICount,
        minISISeconds: nil, q10ISISeconds: q10, q40ISISeconds: nil, medianISISeconds: nil,
        q90ISISeconds: q90, q95ISISeconds: q95, maxISISeconds: nil, meanISISeconds: nil, sampleCV: nil,
        isUsableForCalibration: true, source: "manual_annotations", appliedToDetector: false, method: "test", recommendationText: ""
    )
}

private func builtProposal(_ rows: [ManualAnnotationCalibrationLabelSummary]) -> LearnedThresholdProposal {
    LearnedManualThresholdBuilder.build(from: ManualAnnotationCalibrationSummary(rows: rows, skippedAnnotationCount: 0))
}

// MARK: - current Auto → learned Soft is reported as applied (a change).

@Test
func currentAutoToLearnedSoftIsApplied() {
    let proposal = builtProposal([calibRow(label: burstFamily, isFamily: true, annotationCount: 8, trainCount: 3, q90: 0.030, q95: 0.045)])
    let explanations = LearnedManualThresholdApplier.explain(proposal: proposal, current: ManualThresholdFieldState())
    let seed = explanations.first { $0.field == "seed_upper_sec" }
    #expect(seed?.change == .applied)
    #expect(seed?.isChange == true)
    #expect(seed?.currentMode == .automatic)
    #expect(seed?.currentValueMs == nil)
    #expect(seed?.learnedMode == .softAnchor)
    #expect(seed?.learnedValueMs == 30)
}

// MARK: - current Soft → different learned Soft is changed; matching value is noChange.

@Test
func currentSoftToDifferentLearnedSoftIsChangedAndMatchingIsNoChange() {
    let proposal = builtProposal([calibRow(label: ManualAnnotationLabel.tonic.rawValue, q10: 0.30, q90: 0.55)])
    // tonic already soft with matching lower (300) but a different upper (600).
    let current = ManualThresholdFieldState(tonicMode: .softAnchor, tonicMinISIMs: 300, tonicMaxISIMs: 600)
    let explanations = LearnedManualThresholdApplier.explain(proposal: proposal, current: current)

    let upper = explanations.first { $0.field == "isi_upper_sec" }
    #expect(upper?.change == .changed)
    #expect(upper?.currentValueMs == 600)
    #expect(upper?.learnedValueMs == 550)

    let lower = explanations.first { $0.field == "isi_lower_sec" }
    #expect(lower?.change == .noChange)
    #expect(lower?.isChange == false)
}

// MARK: - current Hard family is reported skipped (not overwritten).

@Test
func currentHardIsReportedSkipped() {
    let proposal = builtProposal([calibRow(label: burstFamily, isFamily: true, q90: 0.030, q95: 0.045)])
    let current = ManualThresholdFieldState(burstMode: .hardGate, burstSeedMaxISIMs: 100, burstBridgeMaxISIMs: 120)
    let explanations = LearnedManualThresholdApplier.explain(proposal: proposal, current: current)
    #expect(explanations.contains { $0.field == "seed_upper_sec" && $0.change == .skippedHard })
    #expect(explanations.filter { $0.family == "burst" }.allSatisfy { $0.change == .skippedHard })
}

// MARK: - a family with no usable learned value: no explanation row, reported as no-value by apply.

@Test
func noUsableLearnedValueIsReportedNoValueAndNotExplained() {
    let proposal = builtProposal([calibRow(label: ManualAnnotationLabel.tonic.rawValue, q10: 0.30, q90: 0.55)])
    let explanations = LearnedManualThresholdApplier.explain(proposal: proposal, current: ManualThresholdFieldState())
    #expect(explanations.allSatisfy { $0.family == "tonic" })   // no burst/hf_tonic/pause rows

    let result = LearnedManualThresholdApplier.apply(proposal: proposal, to: ManualThresholdFieldState())
    #expect(result.noValueFamilies.contains("burst"))
    #expect(result.noValueFamilies.contains("hf_tonic"))
    #expect(result.noValueFamilies.contains("pause"))
}

// MARK: - provenance note includes label, n, train count, statistic, confidence.

@Test
func provenanceNoteIncludesAllFields() {
    let proposal = builtProposal([calibRow(label: burstFamily, isFamily: true, annotationCount: 8, trainCount: 3, q90: 0.030, q95: 0.045)])
    let seed = try? #require(proposal.contributions.first { $0.field == "seed_upper_sec" })
    // Train-clustered support = min(8, 3) / (min(8, 3) + 6) = 3/9 → 0.33.
    #expect(seed?.provenanceNote == "learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.33)")
}

// MARK: - apply result carries provenance notes for applied fields, and none for hard-skipped families.

@Test
func applyResultCarriesProvenanceNotesForAppliedFields() {
    let proposal = builtProposal([calibRow(label: burstFamily, isFamily: true, annotationCount: 8, trainCount: 3, q90: 0.030, q95: 0.045)])
    let result = LearnedManualThresholdApplier.apply(proposal: proposal, to: ManualThresholdFieldState())
    #expect(result.appliedFamilies == ["burst"])
    #expect(result.appliedProvenanceNotes.count == 2)   // seed (q90) + bridge (q95)
    #expect(result.appliedProvenanceNotes.allSatisfy { $0.hasPrefix("learned_from_manual_annotations(label=burst_family,") })
    #expect(result.appliedProvenanceNotes.contains { $0.contains("stat=q90") })
    #expect(result.appliedProvenanceNotes.contains { $0.contains("stat=q95") })
}

@Test
func hardSkippedFamilyProducesNoProvenanceNote() {
    let proposal = builtProposal([calibRow(label: burstFamily, isFamily: true, q90: 0.030, q95: 0.045)])
    let result = LearnedManualThresholdApplier.apply(
        proposal: proposal,
        to: ManualThresholdFieldState(burstMode: .hardGate, burstSeedMaxISIMs: 100, burstBridgeMaxISIMs: 120)
    )
    #expect(result.skippedHardFamilies == ["burst"])
    #expect(result.appliedProvenanceNotes.isEmpty)
}
