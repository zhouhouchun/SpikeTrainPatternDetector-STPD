import Foundation
import STPDCore
import Testing

// Phase 1D: learned-from-annotations provenance is threaded into the detector run so candidates affected by a
// learned soft anchor carry the provenance note in decisionPath / resolved_thresholds.

private let burstFamily = ManualAnnotationCalibrationSummarizer.familyBurst

private func burstFamilyRow(annotationCount: Int = 8, trainCount: Int = 3, q90: Double, q95: Double) -> ManualAnnotationCalibrationLabelSummary {
    ManualAnnotationCalibrationLabelSummary(
        label: burstFamily, displayName: "Burst family", isPositive: true, isFamily: true,
        annotationCount: annotationCount, trainCount: trainCount, coveredISICount: 40,
        minISISeconds: nil, q10ISISeconds: nil, q40ISISeconds: nil, medianISISeconds: nil,
        q90ISISeconds: q90, q95ISISeconds: q95, maxISISeconds: nil, meanISISeconds: nil, sampleCV: nil,
        isUsableForCalibration: true, source: "manual_annotations", appliedToDetector: false, method: "test", recommendationText: ""
    )
}

private func builtProposal(_ rows: [ManualAnnotationCalibrationLabelSummary]) -> LearnedThresholdProposal {
    LearnedManualThresholdBuilder.build(from: ManualAnnotationCalibrationSummary(rows: rows, skippedAnnotationCount: 0))
}

private func cumulative(_ isis: [Double]) -> [Double] {
    var t: [Double] = [0]
    for d in isis { t.append(t.last! + d) }
    return t
}

/// Tonic baseline (0.45 s) → a compact 0.03 s burst → tonic baseline. Produces a tonic and a burst candidate.
private func tonicBurstTrain() -> SpikeTrain {
    let isis = Array(repeating: 0.45, count: 8) + Array(repeating: 0.03, count: 4) + Array(repeating: 0.45, count: 6)
    return SpikeTrain(name: "tonic_burst", timestampsSec: cumulative(isis))
}

private let band = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)

// MARK: - learnedProvenanceByKey(proposal:current:) — only in-effect learned soft anchors yield a key.

@Test
func inEffectLearnedSoftAnchorProducesProvenanceKey() {
    let proposal = builtProposal([burstFamilyRow(annotationCount: 8, trainCount: 3, q90: 0.030, q95: 0.045)])
    // Applied state: burst soft with seed/bridge equal to the learned q90/q95.
    let current = ManualThresholdFieldState(burstMode: .softAnchor, burstSeedMaxISIMs: 30, burstBridgeMaxISIMs: 45)
    let map = LearnedManualThresholdApplier.learnedProvenanceByKey(proposal: proposal, current: current)
    #expect(map["burst.seed_upper_sec"] == "learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.33)")
    #expect(map["burst.bridge_upper_sec"]?.contains("stat=q95") == true)
}

@Test
func notAppliedOrManuallyChangedProducesNoKey() {
    let proposal = builtProposal([burstFamilyRow(q90: 0.030, q95: 0.045)])
    // current Auto (not yet applied)
    #expect(LearnedManualThresholdApplier.learnedProvenanceByKey(proposal: proposal, current: ManualThresholdFieldState()).isEmpty)
    // current Soft but a different (user-changed) value
    let changed = ManualThresholdFieldState(burstMode: .softAnchor, burstSeedMaxISIMs: 99, burstBridgeMaxISIMs: 99)
    #expect(LearnedManualThresholdApplier.learnedProvenanceByKey(proposal: proposal, current: changed).isEmpty)
}

@Test
func hardFamilyProducesNoLearnedProvenanceKey() {
    let proposal = builtProposal([burstFamilyRow(q90: 0.030, q95: 0.045)])
    let current = ManualThresholdFieldState(burstMode: .hardGate, burstSeedMaxISIMs: 100, burstBridgeMaxISIMs: 120)
    #expect(LearnedManualThresholdApplier.learnedProvenanceByKey(proposal: proposal, current: current).isEmpty)
}

@Test
func allAutomaticProposalProducesNoKey() {
    let proposal = builtProposal([])
    #expect(LearnedManualThresholdApplier.learnedProvenanceByKey(proposal: proposal, current: ManualThresholdFieldState()).isEmpty)
}

// MARK: - Pipeline threading: notes land on the right candidate family only.

@Test
func learnedBurstAndTonicProvenanceAppearOnRespectiveCandidates() {
    let train = tonicBurstTrain()
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [train])
    let burstNote = "learned_from_manual_annotations(label=burst_family,n=8,trains=3,stat=q90,confidence=0.57)"
    let tonicNote = "learned_from_manual_annotations(label=tonic,n=6,trains=2,stat=q90,confidence=0.50)"

    var profile = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.040)),
        tonic: TonicManualThresholds(isiUpper: ManualISIThreshold(mode: .softAnchor, valueSec: 0.55))
    )
    profile.learnedProvenanceByKey = ["burst.seed_upper_sec": burstNote, "tonic.isi_upper_sec": tonicNote]

    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: band, manualThresholdProfile: profile)
    let candidates = run.result(for: train.id)?.candidates.filter(\.selectedForAuto) ?? []
    let burst = candidates.first { $0.finalLabel.isBurstEventFamily }
    let tonic = candidates.first { $0.finalLabel == .tonic }

    #expect(burst != nil)
    #expect(tonic != nil)
    #expect(burst?.decisionPath.contains(burstNote) == true)
    #expect(tonic?.decisionPath.contains(tonicNote) == true)
    // req 5: a candidate never receives another family's learned note.
    #expect(burst?.decisionPath.contains(tonicNote) == false)
    #expect(tonic?.decisionPath.contains(burstNote) == false)
}

@Test
func handTypedSoftAnchorWithoutLearnedMapHasNoLearnedProvenance() {
    let train = tonicBurstTrain()
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [train])
    // A soft anchor with NO learned map (hand-typed) → manual provenance, but no learned-from-annotations note.
    let profile = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.040))
    )
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: band, manualThresholdProfile: profile)
    let candidates = run.result(for: train.id)?.candidates ?? []
    #expect(candidates.allSatisfy { !$0.decisionPath.contains("learned_from_manual_annotations") })
}

@Test
func allAutomaticRunHasNoLearnedProvenance() {
    let train = tonicBurstTrain()
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: band, manualThresholdProfile: .automatic)
    let candidates = run.result(for: train.id)?.candidates ?? []
    #expect(candidates.allSatisfy { !$0.decisionPath.contains("learned_from_manual_annotations") })
}

@Test
func hardGatedFamilyGetsNoLearnedProvenanceEndToEnd() {
    // The map is derived from the helper, which excludes Hard families — so a hard burst yields an empty map
    // and the burst candidate carries no learned note even end-to-end.
    let proposal = builtProposal([burstFamilyRow(q90: 0.030, q95: 0.045)])
    let map = LearnedManualThresholdApplier.learnedProvenanceByKey(
        proposal: proposal,
        current: ManualThresholdFieldState(burstMode: .hardGate, burstSeedMaxISIMs: 100, burstBridgeMaxISIMs: 120)
    )
    #expect(map.isEmpty)

    let train = tonicBurstTrain()
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [train])
    var profile = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.100))
    )
    profile.learnedProvenanceByKey = map
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: band, manualThresholdProfile: profile)
    let burst = run.result(for: train.id)?.candidates.first { $0.finalLabel.isBurstEventFamily }
    #expect(burst?.decisionPath.contains("learned_from_manual_annotations") != true)
}
