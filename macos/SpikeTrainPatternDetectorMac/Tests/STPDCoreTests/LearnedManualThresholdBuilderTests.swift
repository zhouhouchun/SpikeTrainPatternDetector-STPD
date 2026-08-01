import Foundation
import STPDCore
import Testing

// Phase 1A: LearnedManualThresholdBuilder turns a manual-annotation calibration summary into a learned,
// soft-anchor ManualThresholdProfile. Pure transform — these test the mapping, gating, and provenance.

private let burstFamily = ManualAnnotationCalibrationSummarizer.familyBurst

private func calibRow(
    label: String,
    isPositive: Bool = true,
    isFamily: Bool = false,
    annotationCount: Int = 5,
    trainCount: Int = 2,
    coveredISICount: Int = 30,
    q10: Double? = nil,
    q40: Double? = nil,
    median: Double? = nil,
    q90: Double? = nil,
    q95: Double? = nil,
    usable: Bool = true
) -> ManualAnnotationCalibrationLabelSummary {
    ManualAnnotationCalibrationLabelSummary(
        label: label, displayName: label, isPositive: isPositive, isFamily: isFamily,
        annotationCount: annotationCount, trainCount: trainCount, coveredISICount: coveredISICount,
        minISISeconds: nil, q10ISISeconds: q10, q40ISISeconds: q40, medianISISeconds: median,
        q90ISISeconds: q90, q95ISISeconds: q95, maxISISeconds: nil, meanISISeconds: nil, sampleCV: nil,
        isUsableForCalibration: usable, source: "manual_annotations", appliedToDetector: false,
        method: "test", recommendationText: ""
    )
}

private func summary(_ rows: [ManualAnnotationCalibrationLabelSummary]) -> ManualAnnotationCalibrationSummary {
    ManualAnnotationCalibrationSummary(rows: rows, skippedAnnotationCount: 0)
}

// MARK: - 1. Burst manual labels produce soft burst thresholds (seedUpper=q90, bridgeUpper=q95).

@Test
func burstLabelsProduceSoftBurstThresholds() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: burstFamily, isFamily: true, q90: 0.030, q95: 0.045)])
    )
    #expect(proposal.profile.burst.seedUpperISI.mode == .softAnchor)
    #expect(proposal.profile.burst.seedUpperISI.valueSec == 0.030)
    #expect(proposal.profile.burst.bridgeUpperISI.mode == .softAnchor)
    #expect(proposal.profile.burst.bridgeUpperISI.valueSec == 0.045)
    // minSpikes is not derivable from the summary → stays automatic.
    #expect(proposal.profile.burst.minSpikes.mode == .automatic)
    #expect(proposal.contributions.contains { $0.family == "burst" && $0.field == "seed_upper_sec" && $0.statistic == "q90" })
    #expect(proposal.contributions.contains { $0.field == "bridge_upper_sec" && $0.statistic == "q95" })
}

// MARK: - 2. Tonic labels produce a soft tonic range (isiLower=q10, isiUpper=q90).

@Test
func tonicLabelsProduceSoftTonicRange() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: ManualAnnotationLabel.tonic.rawValue, q10: 0.30, q90: 0.55)])
    )
    #expect(proposal.profile.tonic.isiLower.mode == .softAnchor && proposal.profile.tonic.isiLower.valueSec == 0.30)
    #expect(proposal.profile.tonic.isiUpper.mode == .softAnchor && proposal.profile.tonic.isiUpper.valueSec == 0.55)
    #expect(proposal.contributions.contains { $0.family == "tonic" && $0.field == "isi_lower_sec" && $0.statistic == "q10" })
    #expect(proposal.contributions.contains { $0.family == "tonic" && $0.field == "isi_upper_sec" && $0.statistic == "q90" })
}

// MARK: - 3. Pause labels produce a soft pause lower threshold (isiLower=q10, R's pause T_seed).

@Test
func pauseLabelsProduceSoftPauseLower() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: ManualAnnotationLabel.pause.rawValue, q10: 0.80, median: 1.20, q90: 1.60)])
    )
    #expect(proposal.profile.pause.isiLower.mode == .softAnchor && proposal.profile.pause.isiLower.valueSec == 0.80)
    #expect(proposal.contributions.contains { $0.family == "pause" && $0.field == "isi_lower_sec" && $0.statistic == "q10" })
}

// MARK: - 4. not_burst / veto labels never produce learned thresholds.

@Test
func notBurstLabelsDoNotLearn() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: ManualAnnotationLabel.notBurst.rawValue, isPositive: false, q10: 0.01, q90: 0.02, usable: false)])
    )
    #expect(proposal.isAllAutomatic)
    #expect(proposal.skipped.contains { $0.sourceLabel == ManualAnnotationLabel.notBurst.rawValue && $0.reason == .negativeVetoLabel })
}

// MARK: - 5. Insufficient count (not usable) produces no threshold.

@Test
func insufficientCountProducesNoThreshold() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: burstFamily, isFamily: true, annotationCount: 2, q90: 0.030, q95: 0.045, usable: false)])
    )
    #expect(proposal.isAllAutomatic)
    #expect(proposal.profile.burst.seedUpperISI.mode == .automatic)
    #expect(proposal.profile.burst.bridgeUpperISI.mode == .automatic)
    #expect(proposal.skipped.contains { $0.sourceLabel == burstFamily && $0.reason == .notUsableMinCount })
}

// MARK: - 6. The learned profile is softAnchor (never hardGate) by default.

@Test
func learnedProfileIsSoftAnchorByDefault() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([
            calibRow(label: burstFamily, isFamily: true, q90: 0.030, q95: 0.045),
            calibRow(label: ManualAnnotationLabel.tonic.rawValue, q10: 0.30, q90: 0.55),
            calibRow(label: ManualAnnotationLabel.pause.rawValue, q10: 0.80, q90: 1.6),
        ])
    )
    let learnedISI = [
        proposal.profile.burst.seedUpperISI, proposal.profile.burst.bridgeUpperISI,
        proposal.profile.tonic.isiLower, proposal.profile.tonic.isiUpper, proposal.profile.pause.isiLower,
    ].filter(\.isActive)
    #expect(!learnedISI.isEmpty)
    #expect(learnedISI.allSatisfy { $0.mode == .softAnchor })
    #expect(proposal.contributions.allSatisfy { $0.mode == .softAnchor })
    #expect(proposal.mode == .softAnchor)
    // No field is a hard gate.
    #expect(proposal.profile.burst.seedUpperISI.mode != .hardGate)
}

// MARK: - 7. All-automatic when there are no usable positive labels.

@Test
func allAutomaticWhenNoUsableLabels() {
    #expect(LearnedManualThresholdBuilder.build(from: summary([])).isAllAutomatic)
    #expect(LearnedManualThresholdBuilder.build(from: summary([])).profile == .automatic)

    let onlyUnusableAndVeto = summary([
        calibRow(label: ManualAnnotationLabel.tonic.rawValue, q10: 0.30, q90: 0.55, usable: false),
        calibRow(label: ManualAnnotationLabel.notBurst.rawValue, isPositive: false, usable: false),
    ])
    #expect(LearnedManualThresholdBuilder.build(from: onlyUnusableAndVeto).isAllAutomatic)
}

// MARK: - Extra coverage: HF-tonic mapping, HFS not learned, and provenance/end-to-end.

@Test
func hfTonicLabelsProduceSoftFloorAndUpper() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: ManualAnnotationLabel.highFrequencyTonic.rawValue, q10: 0.020, q90: 0.050)])
    )
    #expect(proposal.profile.hfTonic.isiFloor.mode == .softAnchor && proposal.profile.hfTonic.isiFloor.valueSec == 0.020)
    #expect(proposal.profile.hfTonic.isiUpper.mode == .softAnchor && proposal.profile.hfTonic.isiUpper.valueSec == 0.050)
}

@Test
func hfsLabelsAreNotLearnedInPhase1A() {
    // Even a well-populated HFS label maps to no Phase-1A field (HFSManualThresholds has no ISI bound).
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: ManualAnnotationLabel.highFrequencySpiking.rawValue, annotationCount: 12, coveredISICount: 200, q10: 0.005, q90: 0.012)])
    )
    #expect(proposal.profile.hfs == HFSManualThresholds())
    #expect(proposal.isAllAutomatic)
    #expect(proposal.skipped.contains { $0.sourceLabel == ManualAnnotationLabel.highFrequencySpiking.rawValue && $0.reason == .noMappableField })
}

@Test
func contributionsCarryConfidenceAndCounts() {
    let proposal = LearnedManualThresholdBuilder.build(
        from: summary([calibRow(label: burstFamily, isFamily: true, annotationCount: 6, trainCount: 3, coveredISICount: 40, q90: 0.030, q95: 0.045)])
    )
    let seed = proposal.contributions.first { $0.field == "seed_upper_sec" }
    #expect(seed?.annotationCount == 6)
    #expect(seed?.trainCount == 3)
    #expect(seed?.coveredISICount == 40)
    // Train-clustered support = min(runs, trains) / (min(runs, trains) + 6) = 3/9.
    #expect(abs((seed?.confidence ?? 0) - (1.0 / 3.0)) < 1e-12)
}

// End-to-end: real annotations → summarizer → builder yields a soft profile (burst faster than tonic).
@Test
func endToEndFromRealAnnotationsProducesSoftProfile() {
    // Tonic baseline (~0.45 s) for spikes 0–6, a tight burst (~0.03 s) for spikes 6–10, then tonic.
    let ts: [Double] = [0.0, 0.45, 0.90, 1.35, 1.80, 2.25, 2.70,
                        2.73, 2.76, 2.79, 2.82,
                        3.27, 3.72, 4.17]
    let train = SpikeTrain(name: "e2e", timestampsSec: ts)
    let annotations = [
        ManualAnnotation(trainID: train.id, label: .tonic, startSec: 0.0, endSec: 2.70),
        ManualAnnotation(trainID: train.id, label: .burst, startSec: 2.70, endSec: 2.82),
    ]
    let calib = ManualAnnotationCalibrationSummarizer.summarize(trains: [train], annotations: annotations)
    let proposal = LearnedManualThresholdBuilder.build(from: calib)

    #expect(!proposal.isAllAutomatic)
    #expect(proposal.profile.tonic.isiUpper.isActive && proposal.profile.tonic.isiUpper.mode == .softAnchor)
    #expect(proposal.profile.burst.seedUpperISI.isActive && proposal.profile.burst.seedUpperISI.mode == .softAnchor)
    // Directionally correct: the learned burst seed is faster than the learned tonic upper.
    if let burstSeed = proposal.profile.burst.seedUpperISI.valueSec,
       let tonicUpper = proposal.profile.tonic.isiUpper.valueSec {
        #expect(burstSeed < tonicUpper)
    }
}
