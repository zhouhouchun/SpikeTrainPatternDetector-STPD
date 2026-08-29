import Foundation
import STPDCore
import Testing

// Pure display-formatting helpers for the learned-threshold UI (no behavior change).

private func contribution(
    family: String = "burst",
    field: String = "seed_upper_sec",
    sourceLabel: String = ManualAnnotationCalibrationSummarizer.familyBurst,
    statistic: String = "q90",
    annotationCount: Int = 8,
    trainCount: Int = 3,
    confidence: Double = 0.57
) -> LearnedThresholdContribution {
    LearnedThresholdContribution(
        family: family, field: field, sourceLabel: sourceLabel, statistic: statistic, valueSec: 0.030,
        mode: .softAnchor, annotationCount: annotationCount, trainCount: trainCount, coveredISICount: 40,
        confidence: confidence
    )
}

@Test
func evidenceSummaryUsesExplicitLabels() {
    #expect(contribution(annotationCount: 8, trainCount: 3, confidence: 0.57).evidenceSummary == "n=8 · trains=3 · support=0.57")
    #expect(contribution(annotationCount: 12, trainCount: 4, confidence: 0.667).evidenceSummary == "n=12 · trains=4 · support=0.67")
}

@Test
func displayProvenanceNoteNormalizesBurstFamilyLabelOnly() {
    let burstMachine = "learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.57)"
    #expect(LearnedManualThresholdApplier.displayProvenanceNote(burstMachine)
        == "learned_from_manual_annotations(label=burst,n=8,trains=3,stat=q90,support=0.57)")

    // Non-burst-family labels are untouched.
    let tonicMachine = "learned_from_manual_annotations(label=tonic,n=6,trains=2,stat=q90,confidence=0.50)"
    #expect(LearnedManualThresholdApplier.displayProvenanceNote(tonicMachine)
        == "learned_from_manual_annotations(label=tonic,n=6,trains=2,stat=q90,support=0.50)")
}

@Test
func machineProvenanceNoteUnchangedByDisplayNormalization() {
    // Req 4: machine tokens (decisionPath / CSV) must keep the source label `burst_family`.
    #expect(contribution().provenanceNote.contains("label=burst_family"))
    #expect(!contribution().provenanceNote.contains("label=burst,"))
}
