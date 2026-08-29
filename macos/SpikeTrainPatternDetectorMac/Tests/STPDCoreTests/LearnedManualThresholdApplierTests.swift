import Foundation
import STPDCore
import Testing

// Phase 1B: LearnedManualThresholdApplier writes a learned proposal into the flat manual-threshold field
// state as soft anchors, except for explicitly confirmed HFS minimum-support gates. Existing user Hard
// families remain protected. Pure transform — this is the document apply logic.

private func soft(_ valueSec: Double) -> ManualISIThreshold { ManualISIThreshold(mode: .softAnchor, valueSec: valueSec) }

private func proposal(_ profile: ManualThresholdProfile) -> LearnedThresholdProposal {
    LearnedThresholdProposal(profile: profile, contributions: [], skipped: [], mode: .softAnchor)
}

// MARK: - Apply writes soft anchors (ms) for every learned family.

@Test
func applyWritesSoftAnchorsForAllLearnedFamilies() {
    let p = proposal(ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: soft(0.030), bridgeUpperISI: soft(0.045)),
        hfTonic: HFTonicManualThresholds(isiFloor: soft(0.020), isiUpper: soft(0.050)),
        tonic: TonicManualThresholds(isiLower: soft(0.30), isiUpper: soft(0.55)),
        pause: PauseManualThresholds(isiLower: soft(0.80))
    ))
    let r = LearnedManualThresholdApplier.apply(proposal: p, to: ManualThresholdFieldState())

    #expect(r.state.burstMode == .softAnchor && r.state.burstSeedMaxISIMs == 30 && r.state.burstBridgeMaxISIMs == 45)
    #expect(r.state.tonicMode == .softAnchor && r.state.tonicMinISIMs == 300 && r.state.tonicMaxISIMs == 550)
    #expect(r.state.hfTonicMode == .softAnchor && r.state.hfTonicMinISIMs == 20 && r.state.hfTonicMaxISIMs == 50)
    #expect(r.state.pauseMode == .softAnchor && r.state.pauseMinISIMs == 800)
    #expect(r.appliedFamilies == ["burst", "tonic", "hf_tonic", "pause"])
    #expect(r.skippedHardFamilies.isEmpty)
    #expect(r.didApplyAnything)
}

// MARK: - A user Hard family is preserved (skipped), other families still apply.

@Test
func applyPreservesUserHardFamilies() {
    let p = proposal(ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: soft(0.030), bridgeUpperISI: soft(0.045)),
        tonic: TonicManualThresholds(isiLower: soft(0.30), isiUpper: soft(0.55))
    ))
    let current = ManualThresholdFieldState(burstMode: .hardGate, burstSeedMaxISIMs: 100, burstBridgeMaxISIMs: 120)
    let r = LearnedManualThresholdApplier.apply(proposal: p, to: current)

    // burst left exactly as the user set it
    #expect(r.state.burstMode == .hardGate && r.state.burstSeedMaxISIMs == 100 && r.state.burstBridgeMaxISIMs == 120)
    #expect(r.skippedHardFamilies.contains("burst"))
    #expect(!r.appliedFamilies.contains("burst"))
    // tonic applied as soft
    #expect(r.state.tonicMode == .softAnchor && r.state.tonicMinISIMs == 300 && r.state.tonicMaxISIMs == 550)
    #expect(r.appliedFamilies.contains("tonic"))
}

// MARK: - Families with no learned value are reported and untouched.

@Test
func applyReportsNoValueFamiliesAndLeavesThemAutomatic() {
    let p = proposal(ManualThresholdProfile(
        tonic: TonicManualThresholds(isiLower: soft(0.30), isiUpper: soft(0.55))
    ))
    let r = LearnedManualThresholdApplier.apply(proposal: p, to: ManualThresholdFieldState())

    #expect(r.appliedFamilies == ["tonic"])
    #expect(r.noValueFamilies == ["burst", "hfs", "hf_tonic", "pause"])
    #expect(r.state.burstMode == .automatic && r.state.hfTonicMode == .automatic && r.state.pauseMode == .automatic)
}

// MARK: - Non-HFS learned families never set Hard mode.

@Test
func applyNeverSetsHardModeForNonHFSFamilies() {
    let p = proposal(ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: soft(0.030), bridgeUpperISI: soft(0.045)),
        hfTonic: HFTonicManualThresholds(isiFloor: soft(0.02), isiUpper: soft(0.05)),
        tonic: TonicManualThresholds(isiLower: soft(0.30), isiUpper: soft(0.55)),
        pause: PauseManualThresholds(isiLower: soft(0.80))
    ))
    let r = LearnedManualThresholdApplier.apply(proposal: p, to: ManualThresholdFieldState())
    #expect(![r.state.burstMode, r.state.tonicMode, r.state.hfTonicMode, r.state.pauseMode].contains(.hardGate))
}

// MARK: - An all-automatic proposal is a no-op.

@Test
func applyAllAutomaticProposalIsNoOp() {
    let existing = ManualThresholdFieldState(tonicMode: .softAnchor, tonicMinISIMs: 300, tonicMaxISIMs: 550)
    let r = LearnedManualThresholdApplier.apply(proposal: proposal(.automatic), to: existing)
    #expect(r.state == existing)
    #expect(r.appliedFamilies.isEmpty)
    #expect(r.noValueFamilies == ["burst", "hfs", "tonic", "hf_tonic", "pause"])
    #expect(!r.didApplyAnything)
}

// MARK: - Unlearned (no-value) families keep their existing non-hard values untouched.

@Test
func applyLeavesUnlearnedNonHardFamiliesUntouched() {
    let p = proposal(ManualThresholdProfile(
        hfTonic: HFTonicManualThresholds(isiFloor: soft(0.02), isiUpper: soft(0.05))
    ))
    let current = ManualThresholdFieldState(tonicMode: .softAnchor, tonicMinISIMs: 200, tonicMaxISIMs: 600)
    let r = LearnedManualThresholdApplier.apply(proposal: p, to: current)

    #expect(r.state.tonicMode == .softAnchor && r.state.tonicMinISIMs == 200 && r.state.tonicMaxISIMs == 600)
    #expect(r.state.hfTonicMode == .softAnchor && r.state.hfTonicMinISIMs == 20 && r.state.hfTonicMaxISIMs == 50)
    #expect(r.appliedFamilies == ["hf_tonic"])
    #expect(r.noValueFamilies.contains("tonic"))
}

// MARK: - End-to-end: calibration summary -> builder -> applier.

@Test
func builderThenApplierEndToEnd() {
    func row(_ label: String, q10: Double? = nil, q90: Double? = nil, q95: Double? = nil) -> ManualAnnotationCalibrationLabelSummary {
        ManualAnnotationCalibrationLabelSummary(
            label: label, displayName: label, isPositive: true, isFamily: label == ManualAnnotationCalibrationSummarizer.familyBurst,
            annotationCount: 6, trainCount: 3, coveredISICount: 40, minISISeconds: nil, q10ISISeconds: q10, q40ISISeconds: nil,
            medianISISeconds: nil, q90ISISeconds: q90, q95ISISeconds: q95, maxISISeconds: nil, meanISISeconds: nil, sampleCV: nil,
            isUsableForCalibration: true, source: "manual_annotations", appliedToDetector: false, method: "test", recommendationText: ""
        )
    }
    let summary = ManualAnnotationCalibrationSummary(rows: [
        row(ManualAnnotationCalibrationSummarizer.familyBurst, q90: 0.030, q95: 0.045),
        row(ManualAnnotationLabel.tonic.rawValue, q10: 0.30, q90: 0.55),
    ], skippedAnnotationCount: 0)

    let built = LearnedManualThresholdBuilder.build(from: summary)
    let r = LearnedManualThresholdApplier.apply(proposal: built, to: ManualThresholdFieldState())

    #expect(r.state.burstMode == .softAnchor && r.state.burstSeedMaxISIMs == 30 && r.state.burstBridgeMaxISIMs == 45)
    #expect(r.state.tonicMode == .softAnchor && r.state.tonicMinISIMs == 300 && r.state.tonicMaxISIMs == 550)
    #expect(r.appliedFamilies == ["burst", "tonic"])
    #expect(r.noValueFamilies == ["hfs", "hf_tonic", "pause"])
}

@Test
func hfsLearningWritesConfirmedMinimumSupportGatesAndPreservesUserHardValues() {
    let learned = proposal(ManualThresholdProfile(
        hfs: HFSManualThresholds(
            minSpikes: .init(mode: .hardGate, value: 36),
            minDurationSec: .init(mode: .hardGate, valueSec: 0.240)
        )
    ))
    let applied = LearnedManualThresholdApplier.apply(
        proposal: learned,
        to: ManualThresholdFieldState()
    )
    #expect(applied.appliedFamilies == ["hfs"])
    #expect(applied.state.hfsMode == .hardGate)
    #expect(applied.state.hfsMinSpikes == 36)
    #expect(applied.state.hfsMinDurationMs == 240)

    let protected = LearnedManualThresholdApplier.apply(
        proposal: learned,
        to: ManualThresholdFieldState(
            hfsMode: .hardGate,
            hfsMinSpikes: 50,
            hfsMinDurationMs: 500
        )
    )
    #expect(protected.skippedHardFamilies == ["hfs"])
    #expect(protected.state.hfsMinSpikes == 50)
    #expect(protected.state.hfsMinDurationMs == 500)
}
