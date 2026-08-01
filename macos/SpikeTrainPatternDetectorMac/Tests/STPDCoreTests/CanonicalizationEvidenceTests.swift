import Foundation
import STPDCore
import Testing

// P1 — audit-only interval/profile evidence boundary. These prove: evidence matches existing route-local
// statistics; compatibility is profile-relative (not fixed-ms); and unavailable reference/candidate evidence
// stays UNAVAILABLE rather than being treated as incompatibility. No detector behavior is exercised for label
// changes — the evidence types are never consumed by detection.

// MARK: - helpers

private func regularTonicDataset() throws -> SpikeDataset {
    var t = 0.0
    var times = [0.0]
    let cyc = [0.45, 0.46, 0.44]
    for i in 0..<29 { t += cyc[i % 3]; times.append(t) }
    let csv = (["t"] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p1", sourceDescription: "p1")
}

/// A minimal resolved profile (bands only) for building candidate coverage/evidence in tests.
private func profile(burstUpper: Double = 0.012, burstBridge: Double = 0.020,
                     tonicLower: Double = 0.20, tonicUpper: Double = 0.90,
                     provenance: [ResolvedThreshold] = []) -> ResolvedThresholdProfile {
    ResolvedThresholdProfile(
        burst: ResolvedFamilyThresholds(lowerSec: 0.003, upperSec: burstUpper, bridgeUpperSec: burstBridge, minSpikes: 3),
        hfs: ResolvedFamilyThresholds(lowerSec: 0.003, upperSec: 0.020, bridgeUpperSec: 0.020, minSpikes: 30),
        hfTonic: ResolvedFamilyThresholds(lowerSec: 0.024, upperSec: 0.040, minSpikes: 6),
        tonic: ResolvedFamilyThresholds(lowerSec: tonicLower, upperSec: tonicUpper, minSpikes: 5),
        pause: ResolvedFamilyThresholds(lowerSec: 1.0, upperSec: .infinity),
        provenance: provenance
    )
}

private func candidateEvidence(q90: Double?, q95: Double? = nil) -> CandidateIntervalEvidence {
    CandidateIntervalEvidence(
        candidateID: "c", route: "test", candidateClass: "test", proposedLabel: .burst, proposedFamily: .burst,
        isiCount: q90 == nil ? 0 : 6, spikeCount: q90 == nil ? 0 : 7,
        q50Sec: q90, q75Sec: q90, q90Sec: q90, q95Sec: q95 ?? q90, cv: 0.1, cv2: 0.1, lv: 0.05,
        burstCoreCoverage: nil, burstBridgeCoverage: nil, tonicCoverage: nil, hfTonicCoverage: nil, pauseCoverage: nil,
        edgeContrast: nil, contextContrast: nil, eventnessScore: nil, returnToBaselineScore: nil,
        softAnchorInvolved: false, hardNumericGateInvolved: false, manualSemanticLabelInvolved: false
    )
}

// MARK: - evidence matches existing route-local statistics

@Test
func candidateEvidenceMatchesRouteLocalStatistics() throws {
    let ds = try regularTonicDataset()
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let cand = try #require((run.result(for: train.id)?.candidates ?? [])
        .first { $0.finalLabel == .tonic && ($0.intraQ90Sec ?? 0) > 0 })

    let evidence = CandidateIntervalEvidence.build(candidate: cand, train: train, profile: profile(), eventness: nil)

    // CV/CV2/LV are projected verbatim from the candidate → exact match by construction.
    #expect(evidence.cv == cand.cv)
    #expect(evidence.cv2 == cand.cv2)
    #expect(evidence.lv == cand.lv)
    // Observed quantiles recomputed from the candidate's own ISI slice match the route-local intra quantiles.
    if let eq90 = evidence.q90Sec, let cq90 = cand.intraQ90Sec { #expect(abs(eq90 - cq90) < 1e-6) }
    if let eq50 = evidence.q50Sec, let cq50 = cand.intraQ50Sec { #expect(abs(eq50 - cq50) < 1e-6) }
    #expect(evidence.isiCount == cand.nValidISI || evidence.isiCount == cand.nISI)
    #expect(evidence.spikeCount == cand.nSpikes)
}

@Test
func candidateEvidenceReadsFamilyScopedProvenanceFlags() throws {
    // build() must read provenance ONLY for the candidate's own family, and treat a numeric gate as distinct from
    // a semantic label. Use a real tonic candidate with a tonic SOFT anchor + an (unrelated) burst HARD gate.
    let ds = try regularTonicDataset()
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let tonicCand = try #require((run.result(for: train.id)?.candidates ?? []).first { $0.finalLabel == .tonic })

    let prov = [
        ResolvedThreshold(key: "tonic.isi_lower_sec", mode: .softAnchor, source: .userSoftAnchor,
                          adaptiveValue: 0.2, userValue: 0.18, effectiveValue: 0.18),
        ResolvedThreshold(key: "burst.seed_upper_sec", mode: .hardGate, source: .userHardGate,
                          adaptiveValue: 0.01, userValue: 0.012, effectiveValue: 0.012),
    ]
    let ev = CandidateIntervalEvidence.build(
        candidate: tonicCand, train: train, profile: profile(provenance: prov), eventness: nil)

    #expect(ev.proposedFamily == .tonic)
    #expect(ev.softAnchorInvolved == true)            // tonic.* soft anchor applies to a tonic candidate
    #expect(ev.hardNumericGateInvolved == false)      // burst.* hard gate does NOT leak onto a tonic candidate
    #expect(ev.manualSemanticLabelInvolved == false)  // numeric gate ≠ semantic label (separate explicit input)
}

// MARK: - compatibility is profile-relative, not fixed-ms

@Test
func compatibilityIsProfileRelativeNotFixedMs() {
    // SAME observed candidate q90, TWO different references → opposite within-reference verdicts. No absolute ms.
    let cand = candidateEvidence(q90: 0.050)
    let refLoose = ResolvedFamilyDistributionSummary.from(
        family: .burst, inBandISIsSec: [0.04, 0.05, 0.06, 0.07, 0.08], provenance: .adaptive)   // q90 ~ 0.076
    let refTight = ResolvedFamilyDistributionSummary.from(
        family: .burst, inBandISIsSec: [0.02, 0.025, 0.03, 0.035, 0.04], provenance: .adaptive) // q90 ~ 0.039

    let loose = IntervalCompatibility.compare(candidate: cand, reference: refLoose, coverage: nil)
    let tight = IntervalCompatibility.compare(candidate: cand, reference: refTight, coverage: nil)
    #expect(loose.q90Margin.available && loose.q90Margin.withinReference == true)   // 0.050 ≤ ~0.076
    #expect(tight.q90Margin.available && tight.q90Margin.withinReference == false)  // 0.050 > ~0.039
    // The verdict flipped on the SAME packet purely from the resolved profile — proves profile-relative.
    #expect(loose.q90Margin.withinReference != tight.q90Margin.withinReference)
}

// MARK: - unavailable evidence stays unavailable (never "incompatible")

@Test
func unavailableProfileReferenceStaysUnavailableNotIncompatible() {
    let cand = candidateEvidence(q90: 0.050)
    // Too small a sample → no reference distribution exists.
    let ref = ResolvedFamilyDistributionSummary.from(family: .burst, inBandISIsSec: [0.05], provenance: .userHardGate)
    #expect(ref.isAvailable == false)
    #expect(ref.q90Sec == nil && ref.q95Sec == nil)

    let compat = IntervalCompatibility.compare(candidate: cand, reference: ref, coverage: nil)
    #expect(compat.referenceAvailable == false)
    #expect(compat.q90Margin.available == false)
    #expect(compat.q90Margin.withinReference == nil)   // UNAVAILABLE, not `false`
    #expect(compat.q95Margin.available == false)
}

@Test
func missingCandidateQuantilesStayUnavailableNotIncompatible() {
    let cand = candidateEvidence(q90: nil)             // e.g. a <2-ISI candidate → no observed quantiles
    let ref = ResolvedFamilyDistributionSummary.from(
        family: .burst, inBandISIsSec: [0.02, 0.025, 0.03, 0.035, 0.04], provenance: .adaptive)
    #expect(ref.isAvailable)
    let compat = IntervalCompatibility.compare(candidate: cand, reference: ref, coverage: nil)
    #expect(compat.q90Margin.available == false)
    #expect(compat.q90Margin.withinReference == nil)   // missing candidate evidence ⇒ unavailable, not incompatible
}

@Test
func referenceSummaryDoesNotSynthesizeFromSingleBound() {
    // A single numeric bound (one in-band ISI, or none) must NOT yield synthesized q90/q95.
    let one = ResolvedFamilyDistributionSummary.from(family: .burst, inBandISIsSec: [0.012], provenance: .userHardGate)
    #expect(one.isAvailable == false && one.q90Sec == nil && one.sampleSize == 1)

    let none = ResolvedFamilyDistributionSummary.from(family: .tonic, inBandISIsSec: [], provenance: .userHardGate)
    #expect(none.isAvailable == false && none.q50Sec == nil && none.sampleSize == 0)
}

@Test
func referenceFromTrainUsesOnlyInBandISIs() throws {
    let ds = try regularTonicDataset()                  // ISIs ~0.44–0.46 s
    let train = ds.trains[0]
    // Tonic band [0.20, 0.90] captures all the ~0.45 s ISIs → available, q50 ≈ 0.45.
    let tonicRef = ResolvedFamilyDistributionSummary.fromTrain(
        train: train, family: .tonic, lowerSec: 0.20, upperSec: 0.90, provenance: .adaptive)
    #expect(tonicRef.isAvailable)
    if let q50 = tonicRef.q50Sec { #expect(q50 > 0.40 && q50 < 0.50) }
    #expect(tonicRef.sampleSize >= 25)

    // A burst band [0.003, 0.012] captures NONE of this regular train's ISIs → unavailable (no synthesis).
    let burstRef = ResolvedFamilyDistributionSummary.fromTrain(
        train: train, family: .burst, lowerSec: 0.003, upperSec: 0.012, provenance: .adaptive)
    #expect(burstRef.isAvailable == false && burstRef.sampleSize == 0)
}

// MARK: - decision audit is a separate projection of the current arbitration outcome

@Test
func decisionAuditProjectsCurrentArbitrationOutcome() throws {
    let ds = try regularTonicDataset()
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let cand = try #require((run.result(for: ds.trains[0].id)?.candidates ?? []).first { $0.selectedForAuto })
    let audit = CandidateDecisionAudit.from(cand)
    #expect(audit.candidateID == cand.id)
    #expect(audit.selectedForAuto == cand.selectedForAuto)
    #expect(audit.finalLabel == cand.finalLabel)
    #expect(audit.selectionStatus == cand.selectionStatus)
}
