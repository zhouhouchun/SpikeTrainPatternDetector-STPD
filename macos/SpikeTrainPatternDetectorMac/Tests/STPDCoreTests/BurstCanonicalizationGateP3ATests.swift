import Foundation
import STPDCore
import Testing

// P3A — central burst canonicalization chokepoint, default OFF. These prove: the pure decide() function classifies
// the false-burst as non-canonical; and the pipeline with the flag OFF (default) is byte-identical (the false-burst
// is still selected as burst, with identical labels/lock levels/decision paths).

private func dataset(_ name: String, _ times: [Double]) throws -> SpikeDataset {
    let csv = ([name] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p3a", sourceDescription: "p3a")
}

private func falseBurstTimes() -> [Double] {
    var t = 0.0; var times = [0.0]
    for _ in 0..<5 { for _ in 0..<3 { t += 0.06; times.append(t) }; t += 1.5; times.append(t) }
    return times
}

private func classicBurstTimes() -> [Double] {
    var t = 0.0; var times = [0.0]
    for blk in 0..<5 { for _ in 0..<6 { t += 0.40; times.append(t) }; if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } } }
    return times
}

private func p3aProfile(burst: (Double, Double) = (0.003, 0.012)) -> ResolvedThresholdProfile {
    ResolvedThresholdProfile(
        burst: ResolvedFamilyThresholds(lowerSec: burst.0, upperSec: burst.1, bridgeUpperSec: burst.1 * 1.6, minSpikes: 3),
        hfs: ResolvedFamilyThresholds(lowerSec: 0.003, upperSec: 0.020, minSpikes: 30),
        hfTonic: ResolvedFamilyThresholds(lowerSec: 0.024, upperSec: 0.040, minSpikes: 6),
        tonic: ResolvedFamilyThresholds(lowerSec: 0.20, upperSec: 0.90, minSpikes: 5),
        pause: ResolvedFamilyThresholds(lowerSec: 1.0, upperSec: .infinity),
        provenance: []
    )
}

private func verdictResult(_ v: CanonicalizationVerdict, manualOverride: Bool = false) -> CanonicalizationVerdictResult {
    CanonicalizationVerdictResult(
        candidateID: "c", proposedFamily: .burst, verdict: v, reasons: [], manualSemanticOverride: manualOverride,
        softAnchorInvolved: false, hardNumericGateInvolved: false,
        burstCompatibility: nil, tonicCompatibility: nil, hfTonicCompatibility: nil)
}

/// Build evidence + verdict for a candidate against a train (mirrors the pipeline's ON-path inputs).
private func evidenceAndVerdict(_ c: ClassicAnchorCandidate, _ train: SpikeTrain)
    -> (CandidateIntervalEvidence, CanonicalizationVerdictResult) {
    let profile = p3aProfile()
    let ev = CandidateIntervalEvidence.build(
        candidate: c, train: train, profile: profile,
        eventness: ISICandidateEventnessAuditor.makeAudit(for: c, train: train, minValidISISec: 0.001))
    let bRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .burst, lowerSec: 0.003, upperSec: 0.0192, provenance: .adaptive)
    let tRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .tonic, lowerSec: 0.20, upperSec: 0.90, provenance: .adaptive)
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: tRef, hfTonicReference: nil)
    return (ev, v)
}

// MARK: - the pure function

@Test
func gate_classifiesFalseBurstAsDemoteToPossible() throws {
    let ds = try dataset("fb", falseBurstTimes())
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let burst = try #require((run.result(for: train.id)?.candidates ?? [])
        .first { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily })
    let (ev, v) = evidenceAndVerdict(burst, train)
    #expect(BurstCanonicalizationGate.decide(candidate: burst, evidence: ev, verdict: v) == .demoteToPossible)
}

@Test
func gate_keepsCanonicalForRealClassicBurst() throws {
    let ds = try dataset("cb", classicBurstTimes())
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let burst = try #require((run.result(for: train.id)?.candidates ?? [])
        .first { $0.selectedForAuto && $0.finalLabel == .burst })
    let (ev, v) = evidenceAndVerdict(burst, train)
    #expect(BurstCanonicalizationGate.decide(candidate: burst, evidence: ev, verdict: v) == .keepCanonical)
}

@Test
func gate_decisionPolicyAcrossVerdicts() throws {
    let ds = try dataset("cb", classicBurstTimes())
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let burst = try #require((run.result(for: train.id)?.candidates ?? []).first { $0.finalLabel == .burst })
    let ev = CandidateIntervalEvidence.build(candidate: burst, train: train, profile: p3aProfile(), eventness: nil)
    func decide(_ v: CanonicalizationVerdictResult) -> BurstCanonicalDecision {
        BurstCanonicalizationGate.decide(candidate: burst, evidence: ev, verdict: v)
    }
    // Keep on confirmation, and on missing/inconclusive evidence (never demote on unavailability).
    #expect(decide(verdictResult(.canonicalCandidate)) == .keepCanonical)
    #expect(decide(verdictResult(.insufficientEvidence)) == .keepCanonical)
    #expect(decide(verdictResult(.unavailableProfileEvidence)) == .keepCanonical)
    // Demote on positive non-canonical evidence.
    #expect(decide(verdictResult(.possibleBurstCandidate)) == .demoteToPossible)
    #expect(decide(verdictResult(.pauseBoundaryDrivenDensePacket)) == .demoteToPossible)
    #expect(decide(verdictResult(.bridgeWithoutCore)) == .demoteToPossible)
    #expect(decide(verdictResult(.tonicGuardConflict)) == .demoteToPossible)
    // Manual semantic burst label overrides — keep canonical even with a demoting verdict.
    #expect(decide(verdictResult(.pauseBoundaryDrivenDensePacket, manualOverride: true)) == .keepCanonical)
}

@Test
func gate_nonCanonicalCandidateIsAlwaysKept() throws {
    // A tonic candidate is not a burst → never demoted by the gate, regardless of the verdict.
    var t = 0.0; var times = [0.0]
    let cyc = [0.45, 0.46, 0.44]
    for i in 0..<29 { t += cyc[i % 3]; times.append(t) }
    let ds = try dataset("rt", times)
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let tonic = try #require((run.result(for: train.id)?.candidates ?? []).first { $0.finalLabel == .tonic })
    let ev = CandidateIntervalEvidence.build(candidate: tonic, train: train, profile: p3aProfile(), eventness: nil)
    #expect(BurstCanonicalizationGate.decide(candidate: tonic, evidence: ev,
                                             verdict: verdictResult(.pauseBoundaryDrivenDensePacket)) == .keepCanonical)
}

// MARK: - pipeline OFF is byte-identical

private func snapshot(_ run: ClassicAnchorDetectionRun, _ trainID: String) -> [String] {
    (run.result(for: trainID)?.candidates ?? [])
        .map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.anchorLockLevel.rawValue)|\($0.selectedForAuto)|\($0.selectionStatus)|\($0.decisionPath)" }
        .sorted()
}

@Test
func pipeline_flagOffIsByteIdenticalAndKeepsFalseBurst() throws {
    let ds = try dataset("fb", falseBurstTimes())
    let train = ds.trains[0]
    // Default run (flag omitted → false) vs explicit flag = false: identical candidates, labels, locks, decision paths.
    let defaultRun = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let explicitOff = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(),
                                                         useAdaptiveV2Canonicalization: false)
    #expect(snapshot(defaultRun, train.id) == snapshot(explicitOff, train.id))

    // With the flag OFF the false-burst is STILL selected as burst (no demotion happens).
    let selectedBursts = (defaultRun.result(for: train.id)?.candidates ?? [])
        .filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }
    #expect(!selectedBursts.isEmpty)
    // ...and none carry the Adaptive-v2 demotion marker.
    #expect(!(defaultRun.result(for: train.id)?.candidates ?? []).contains { $0.decisionPath.contains("adaptive_v2_canonicalization") })
}

@Test
func pipeline_flagOffClassicBurstUnchanged() throws {
    let ds = try dataset("cb", classicBurstTimes())
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let lockedClassic = (run.result(for: train.id)?.candidates ?? [])
        .filter { $0.selectedForAuto && $0.finalLabel == .burst && $0.anchorLockLevel == .lockedClassic }
    #expect(!lockedClassic.isEmpty)   // real classic bursts remain locked-classic when the flag is OFF
}
