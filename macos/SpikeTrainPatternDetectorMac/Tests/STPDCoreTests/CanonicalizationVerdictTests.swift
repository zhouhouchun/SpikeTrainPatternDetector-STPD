import Foundation
import STPDCore
import Testing

// P2 — audit-only canonicality verdicts. These prove each verdict category, profile-relative semantics, the
// soft/hard/manual distinctions, and (critically) that computing verdicts changes NO detector behavior.

// MARK: - constructed-evidence helper (unit cases)

private func evidence(
    family: IntervalFamily = .burst, candidateID: String = "c", isiCount: Int = 8,
    q90: Double? = 0.008, q95: Double? = nil, cv: Double? = 0.10, cv2: Double? = 0.10, lv: Double? = 0.05,
    burstCore: Double? = nil, burstBridge: Double? = nil, tonicCov: Double? = nil, hfTonicCov: Double? = nil,
    edge: Double? = nil, ctx: Double? = nil,
    soft: Bool = false, hardNumeric: Bool = false, manual: Bool = false,
    burstCoreCount: Int? = nil, burstBridgeExtCount: Int? = nil
) -> CandidateIntervalEvidence {
    let label: ClassicAnchorLabel
    switch family {
    case .burst: label = .burst
    case .tonic: label = .tonic
    case .hfTonic: label = .highFrequencyTonic
    case .hfs: label = .highFrequencySpiking
    case .pause: label = .pause
    }
    // For convenience, derive the exact core count from coverage when not given explicitly (so legacy constructions
    // behave as before); the builder itself never rounds — it consumes the count field directly.
    let coreCount = burstCoreCount ?? burstCore.map { Int(($0 * Double(isiCount)).rounded()) }
    return CandidateIntervalEvidence(
        candidateID: candidateID, route: "test", candidateClass: "test", proposedLabel: label, proposedFamily: family,
        isiCount: isiCount, spikeCount: isiCount + 1,
        q50Sec: q90, q75Sec: q90, q90Sec: q90, q95Sec: q95 ?? q90, cv: cv, cv2: cv2, lv: lv,
        burstCoreCoverage: burstCore, burstBridgeCoverage: burstBridge, tonicCoverage: tonicCov,
        hfTonicCoverage: hfTonicCov, pauseCoverage: nil,
        edgeContrast: edge, contextContrast: ctx, eventnessScore: nil, returnToBaselineScore: nil,
        softAnchorInvolved: soft, hardNumericGateInvolved: hardNumeric, manualSemanticLabelInvolved: manual,
        burstCoreISICount: coreCount, burstBridgeExtensionISICount: burstBridgeExtCount
    )
}

private func sample(_ values: [Double], _ family: IntervalFamily) -> ResolvedFamilyDistributionSummary {
    ResolvedFamilyDistributionSummary.from(family: family, inBandISIsSec: values, provenance: .adaptive)
}

private func dataset(_ name: String, _ times: [Double]) throws -> SpikeDataset {
    let csv = ([name] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p2", sourceDescription: "p2")
}

// MARK: - real-fixture verdicts

@Test
func verdict_classicBurst_isCanonicalCandidate() throws {
    var t = 0.0; var times = [0.0]
    for blk in 0..<5 { for _ in 0..<6 { t += 0.40; times.append(t) }; if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } } }
    let ds = try dataset("cb", times)
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let profile = bandProfile(burst: (0.003, 0.012))
    let bRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .burst, lowerSec: 0.003, upperSec: 0.012, provenance: .adaptive)
    let tRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .tonic, lowerSec: 0.20, upperSec: 0.90, provenance: .adaptive)

    let bursts = (run.result(for: train.id)?.candidates ?? []).filter { $0.selectedForAuto && $0.finalLabel == .burst }
    let verdicts = bursts.map { c -> CanonicalizationVerdict in
        let ev = CandidateIntervalEvidence.build(candidate: c, train: train, profile: profile,
            eventness: ISICandidateEventnessAuditor.makeAudit(for: c, train: train, minValidISISec: 0.001))
        return CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: tRef, hfTonicReference: nil).verdict
    }
    #expect(verdicts.contains(.canonicalCandidate))   // genuine seed/core bursts read as canonical
}

@Test
func verdict_falseBurstPauseFlanked_isPauseDrivenOrPossible_andLabelUnchanged() throws {
    var t = 0.0; var times = [0.0]
    for _ in 0..<5 { for _ in 0..<3 { t += 0.06; times.append(t) }; t += 1.5; times.append(t) }
    let ds = try dataset("fb", times)
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let profile = bandProfile(burst: (0.003, 0.012))
    let bRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .burst, lowerSec: 0.003, upperSec: 0.012, provenance: .adaptive)
    let tRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .tonic, lowerSec: 0.20, upperSec: 0.90, provenance: .adaptive)

    let bursts = (run.result(for: train.id)?.candidates ?? []).filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
    #expect(!bursts.isEmpty)   // the detector STILL labels these burst in P2 (no behavior change)
    for c in bursts {
        let ev = CandidateIntervalEvidence.build(candidate: c, train: train, profile: profile,
            eventness: ISICandidateEventnessAuditor.makeAudit(for: c, train: train, minValidISISec: 0.001))
        let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: tRef, hfTonicReference: nil).verdict
        #expect(v == .pauseBoundaryDrivenDensePacket || v == .possibleBurstCandidate)
        #expect(v != .canonicalCandidate)   // the audit flags it as NOT a canonical burst...
        // ...but the actual detector label is unchanged in P2:
        #expect(c.finalLabel.isBurstEventFamily && c.selectedForAuto)
    }
}

@Test
func verdict_regularTonic_isCanonicalCandidate() throws {
    var t = 0.0; var times = [0.0]
    let cyc = [0.45, 0.46, 0.44]
    for i in 0..<29 { t += cyc[i % 3]; times.append(t) }
    let ds = try dataset("rt", times)
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let tonic = try #require((run.result(for: train.id)?.candidates ?? []).first { $0.selectedForAuto && $0.finalLabel == .tonic })
    let ev = CandidateIntervalEvidence.build(candidate: tonic, train: train, profile: bandProfile(),
        eventness: ISICandidateEventnessAuditor.makeAudit(for: tonic, train: train, minValidISISec: 0.001))
    let tRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .tonic, lowerSec: 0.20, upperSec: 0.90, provenance: .adaptive)
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: nil, tonicReference: tRef, hfTonicReference: nil)
    #expect(v.verdict == .canonicalCandidate)
}

// MARK: - constructed-evidence verdicts (deterministic per category)

@Test
func verdict_bridgeWithoutCore() {
    // P6B-1: bridge-extension ISIs with NO tight-core ISI (core == 0) → bridge alone cannot create a canonical burst.
    let ev = evidence(burstCore: 0.0, burstBridge: 0.80, burstCoreCount: 0, burstBridgeExtCount: 6)
    let bRef = sample([0.006, 0.008, 0.010, 0.012, 0.014], .burst)
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: nil, hfTonicReference: nil)
    #expect(v.verdict == .bridgeWithoutCore)
}

// MARK: - P2.5: exact core/bridge counts

@Test
func evidence_exactBurstCountsMatchHandComputedForClassicBurst() throws {
    var t = 0.0; var times = [0.0]
    for blk in 0..<5 { for _ in 0..<6 { t += 0.40; times.append(t) }; if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } } }
    let ds = try dataset("cb", times)
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let coreLower = 0.003, coreUpper = 0.012, bridgeUpper = 0.012 * 1.6
    let profile = bandProfile(burst: (coreLower, coreUpper))
    let burst = try #require((run.result(for: train.id)?.candidates ?? []).first { $0.selectedForAuto && $0.finalLabel == .burst })

    let ev = CandidateIntervalEvidence.build(candidate: burst, train: train, profile: profile, eventness: nil)
    let isis = CandidateIntervalEvidence.candidateISIsSec(train: train, startISIIndex: burst.startISIIndex, endISIIndex: burst.endISIIndex)
    let handCore = isis.filter { $0 >= coreLower && $0 <= coreUpper }.count
    let handExt = isis.filter { $0 > coreUpper && $0 <= bridgeUpper }.count
    #expect(ev.burstCoreISICount == handCore)
    #expect(ev.burstBridgeExtensionISICount == handExt)
    #expect(handCore >= 1)   // a real burst packet has core ISIs
}

@Test
func evidence_bridgeOnlyReportsCoreZeroExtensionPositive() throws {
    // A regular ~16 ms train: every ISI sits in the bridge extension (0.012, 0.0192], none in the core (≤0.012).
    var t = 0.0; var times = [0.0]
    for _ in 0..<20 { t += 0.016; times.append(t) }
    let ds = try dataset("bridge_only", times)
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let profile = bandProfile(burst: (0.003, 0.012))   // bridgeUpper = 0.0192
    // The exact counts are selection-independent — any candidate spanning ≥2 of the ~16 ms ISIs exercises them.
    let multiISICandidates = (run.result(for: train.id)?.candidates ?? []).filter { $0.nISI >= 2 }
    let cand = try #require(multiISICandidates.max(by: { $0.nISI < $1.nISI }))

    let ev = CandidateIntervalEvidence.build(candidate: cand, train: train, profile: profile, eventness: nil)
    #expect(ev.burstCoreISICount == 0)                       // none of the ~16 ms ISIs are in the core (≤12 ms)
    #expect((ev.burstBridgeExtensionISICount ?? 0) > 0)      // all sit in the bridge extension (12–19.2 ms)
}

@Test
func verdict_usesExactCoreCountNotRoundedCoverage() {
    // coverage 0.5 × isiCount 3 = 1.5 → rounds to 2 (would pass minCore 2). The EXACT count is 1 → must NOT be
    // treated as sufficient core, so the verdict is not canonical. Proves the builder uses the count, not rounding.
    let ev = evidence(isiCount: 3, q90: 0.008, burstCore: 0.5, burstCoreCount: 1)
    let bRef = sample([0.006, 0.008, 0.010, 0.012, 0.014], .burst)   // candidate q90 within → would-be canonical
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: nil, hfTonicReference: nil)
    #expect(v.verdict != .canonicalCandidate)
    #expect(v.verdict == .possibleBurstCandidate)
    #expect(v.reasons.contains(.weakBurstCore))
}

@Test
func verdict_tonicGuardConflict_requiresWeakCorePlusTonicCompatPlusRegularity() {
    // Weak burst core + strong tonic coverage + regular + tonic q-compat → tonic guard conflict.
    let ev = evidence(family: .burst, q90: 0.45, cv: 0.08, cv2: 0.10, lv: 0.04,
                      burstCore: 0.10, burstBridge: nil, tonicCov: 0.90)
    let bRef = sample([0.006, 0.008, 0.010, 0.012, 0.014], .burst)      // candidate q90 0.45 is far outside
    let tRef = sample([0.40, 0.42, 0.44, 0.46, 0.50], .tonic)           // q90 ~0.48 ≥ 0.45 → within
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: tRef, hfTonicReference: nil)
    #expect(v.verdict == .tonicGuardConflict)
    #expect(v.reasons.contains(.strongTonicCompatibility) && v.reasons.contains(.weakBurstCore))
}

@Test
func verdict_regularityAloneDoesNotDemoteBurst() {
    // A REGULAR burst with sufficient core + burst-compatible q's but NO tonic compatibility must stay canonical —
    // regularity by itself is not a tonic-guard trigger (semantics #7).
    // P6B-1: burstBridge is the FULL burst-band coverage (core+bridge); for real evidence it is ≥ core coverage.
    let ev = evidence(family: .burst, q90: 0.008, cv: 0.05, cv2: 0.05, lv: 0.02,
                      burstCore: 0.95, burstBridge: 1.0, tonicCov: 0.0)
    let bRef = sample([0.006, 0.008, 0.010, 0.012, 0.014], .burst)      // candidate q90 0.008 ≤ ref q90 → within
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: nil, hfTonicReference: nil)
    #expect(v.verdict == .canonicalCandidate)
    #expect(v.verdict != .tonicGuardConflict)
}

@Test
func verdict_eligibilityCeilingRescuesBurstAboveTightInBandReference() {
    // P6B-1: a real burst whose q90/q95 sit ABOVE the (over-tight) in-band reference percentile but WITHIN the train's
    // burst eligibility ceiling (resolved bridgeUpper). With the legacy reference path it is demoted; with the ceiling
    // it is canonical — the P6A over-demotion fix. Support is core+bridge sufficient in both calls.
    let ev = evidence(family: .burst, q90: 0.012, q95: 0.012, cv: 0.08, cv2: 0.08, lv: 0.03,
                      burstCore: 0.6, burstBridge: 1.0, tonicCov: 0.0,
                      burstCoreCount: 3, burstBridgeExtCount: 2)
    let tightRef = sample([0.004, 0.005, 0.006, 0.007, 0.008], .burst)   // q95 ≈ 0.0079 — far below candidate 0.012

    // Legacy reference path (no ceiling): candidate q95 0.012 ≫ ref 0.0079 ⇒ NOT canonical.
    let legacy = CanonicalizationVerdictBuilder.verdict(
        evidence: ev, burstReference: tightRef, tonicReference: nil, hfTonicReference: nil)
    #expect(legacy.verdict == .possibleBurstCandidate)

    // Eligibility-ceiling path: candidate q95 0.012 ≤ ceiling 0.015 × tolerance ⇒ canonical.
    let withCeiling = CanonicalizationVerdictBuilder.verdict(
        evidence: ev, burstReference: tightRef, tonicReference: nil, hfTonicReference: nil,
        burstEligibilityCeilingSec: 0.015)
    #expect(withCeiling.verdict == .canonicalCandidate)
}

@Test
func verdict_eligibilityCeilingStillDemotesBurstFarAboveCeiling() {
    // P6B-1: the ceiling is not a free pass — a burst whose q95 is far above the eligibility ceiling (boundary
    // contamination by a slow edge ISI) stays demoted even with the ceiling supplied. This is the P6B-2 case.
    let ev = evidence(family: .burst, q90: 0.090, q95: 0.140, cv: 0.55, cv2: 0.40, lv: 0.30,
                      burstCore: 0.6, burstBridge: 0.75, tonicCov: 0.0,
                      burstCoreCount: 3, burstBridgeExtCount: 0)
    let withCeiling = CanonicalizationVerdictBuilder.verdict(
        evidence: ev, burstReference: nil, tonicReference: nil, hfTonicReference: nil,
        burstEligibilityCeilingSec: 0.045)
    #expect(withCeiling.verdict == .possibleBurstCandidate)   // q95 0.140 ≫ ceiling 0.045 ⇒ not canonical
}

@Test
func verdict_pauseBoundaryDriven_fromEventnessAloneWhenReferencesUnavailable() {
    // No usable references, but strong immediate-flank contrast with weak active-background context ⇒ pause-driven.
    let ev = evidence(family: .burst, q90: 0.6, burstCore: 0.0, burstBridge: 0.0, edge: 0.80, ctx: 0.03)
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: nil, tonicReference: nil, hfTonicReference: nil)
    #expect(v.verdict == .pauseBoundaryDrivenDensePacket)
}

@Test
func verdict_unavailableProfileEvidence_forTonicCandidateWithNoReference() {
    let ev = evidence(family: .tonic, q90: 0.45, tonicCov: 0.9)
    let tinyRef = sample([0.45], .tonic)   // too small → unavailable
    #expect(tinyRef.isAvailable == false)
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: nil, tonicReference: tinyRef, hfTonicReference: nil)
    #expect(v.verdict == .unavailableProfileEvidence)
}

@Test
func verdict_insufficientEvidence_whenCandidateHasNoQuantiles() {
    let ev = evidence(family: .burst, isiCount: 1, q90: nil)
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: sample([0.006, 0.008, 0.010, 0.012], .burst),
                                                   tonicReference: nil, hfTonicReference: nil)
    #expect(v.verdict == .insufficientEvidence)
    #expect(v.reasons.contains(.candidateQuantilesUnavailable))
}

// MARK: - soft / hard / manual distinctions

@Test
func verdict_softHardManualFlagsAreDistinguishedAndManualDoesNotChangeVerdict() {
    let bRef = sample([0.006, 0.008, 0.010, 0.012, 0.014], .burst)
    let base = evidence(family: .burst, q90: 0.008, burstCore: 0.95, burstBridge: 0.10)
    let withSoft = evidence(family: .burst, q90: 0.008, burstCore: 0.95, burstBridge: 0.10, soft: true)
    let withHard = evidence(family: .burst, q90: 0.008, burstCore: 0.95, burstBridge: 0.10, hardNumeric: true)
    let withManual = evidence(family: .burst, q90: 0.008, burstCore: 0.95, burstBridge: 0.10, manual: true)

    let vBase = CanonicalizationVerdictBuilder.verdict(evidence: base, burstReference: bRef, tonicReference: nil, hfTonicReference: nil)
    let vSoft = CanonicalizationVerdictBuilder.verdict(evidence: withSoft, burstReference: bRef, tonicReference: nil, hfTonicReference: nil)
    let vHard = CanonicalizationVerdictBuilder.verdict(evidence: withHard, burstReference: bRef, tonicReference: nil, hfTonicReference: nil)
    let vManual = CanonicalizationVerdictBuilder.verdict(evidence: withManual, burstReference: bRef, tonicReference: nil, hfTonicReference: nil)

    // The verdict category is identical regardless of the flags (flags are signals, not overrides).
    #expect(vBase.verdict == vSoft.verdict && vSoft.verdict == vHard.verdict && vHard.verdict == vManual.verdict)
    // Each flag surfaces distinctly.
    #expect(vSoft.softAnchorInvolved && vSoft.reasons.contains(.softAnchorInvolved))
    #expect(vHard.hardNumericGateInvolved && vHard.reasons.contains(.hardNumericGateInvolved))
    #expect(!vHard.manualSemanticOverride)                       // a numeric gate is NOT a semantic label
    #expect(vManual.manualSemanticOverride && vManual.reasons.contains(.manualSemanticOverridePresent))
    #expect(!vBase.manualSemanticOverride && !vBase.softAnchorInvolved && !vBase.hardNumericGateInvolved)
}

// MARK: - no behavior change

@Test
func verdict_buildingVerdictsChangesNoDetectorOutput() throws {
    var t = 0.0; var times = [0.0]
    for blk in 0..<5 { for _ in 0..<6 { t += 0.40; times.append(t) }; if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } } }
    let ds = try dataset("cb", times)
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())

    func snapshot(_ r: ClassicAnchorDetectionRun) -> [String] {
        (r.result(for: train.id)?.candidates ?? []).filter(\.selectedForAuto)
            .map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.selectionStatus)|\($0.anchorLockLevel.rawValue)" }
            .sorted()
    }
    let before = snapshot(run)

    // Build verdicts for EVERY candidate (the audit work under test).
    let profile = bandProfile(burst: (0.003, 0.012))
    let bRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .burst, lowerSec: 0.003, upperSec: 0.012, provenance: .adaptive)
    for c in run.result(for: train.id)?.candidates ?? [] {
        _ = CanonicalizationVerdictBuilder.verdict(
            evidence: CandidateIntervalEvidence.build(candidate: c, train: train, profile: profile,
                eventness: ISICandidateEventnessAuditor.makeAudit(for: c, train: train, minValidISISec: 0.001)),
            burstReference: bRef, tonicReference: nil, hfTonicReference: nil)
    }

    // Same run object is unchanged, and a fresh run is identical (determinism) — no behavior change.
    #expect(snapshot(run) == before)
    let rerun = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    #expect(snapshot(rerun) == before)
}

// MARK: - shared band profile

private func bandProfile(burst: (Double, Double) = (0.003, 0.012)) -> ResolvedThresholdProfile {
    ResolvedThresholdProfile(
        burst: ResolvedFamilyThresholds(lowerSec: burst.0, upperSec: burst.1, bridgeUpperSec: burst.1 * 1.6, minSpikes: 3),
        hfs: ResolvedFamilyThresholds(lowerSec: 0.003, upperSec: 0.020, minSpikes: 30),
        hfTonic: ResolvedFamilyThresholds(lowerSec: 0.024, upperSec: 0.040, minSpikes: 6),
        tonic: ResolvedFamilyThresholds(lowerSec: 0.20, upperSec: 0.90, minSpikes: 5),
        pause: ResolvedFamilyThresholds(lowerSec: 1.0, upperSec: .infinity),
        provenance: []
    )
}
