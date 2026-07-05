@testable import STPDCore
import Testing

// MARK: - TSW-1 — tonic structural window detector (pure gate helpers; unwired).
//
// Tests the per-window regularity GATE (short compactness vs long CV/CV2/LV), the burst-contamination
// GUARD (D3 valley + QC-refractory fallback), the adjacent-ratio helper, the span→spike index convention,
// metric parity with ISISpanMetrics, scale-invariance, and that provenance uses only the allowed positive
// vocabulary (the legacy removed peakiness metric never appears).

private func tswTrain(_ name: String, isis: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for isi in isis { ts.append((ts.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

/// Evaluate the full ISI series [1 ... isis.count] of a train built from `isis`.
private func tswEvaluate(
    _ isis: [Double], burstValleySec: Double?, config: TonicStructuralWindowConfig = TonicStructuralWindowConfig()
) -> TonicWindowEvaluation {
    let train = tswTrain("t", isis: isis)
    let span = ISISpan(trainID: "t", startISIIndex: 1, endISIIndex: isis.count, familyHint: .tonic)
    return TonicStructuralWindowDetector.evaluateWindow(
        train: train, span: span, burstValleySec: burstValleySec, config: config)
}

private func tswAccepted(_ e: TonicWindowEvaluation) -> TonicStructuralWindowCandidate? {
    if case let .accepted(c) = e { return c }; return nil
}
private func tswRejectReason(_ e: TonicWindowEvaluation) -> TonicWindowBoundaryReason? {
    if case let .rejected(reason, _) = e { return reason }; return nil
}

// 1 — short regular window (4 ISIs) passes the compactness gate.
@Test
func tswShortRegularWindowPassesCompactness() {
    let e = tswEvaluate([0.050, 0.052, 0.051, 0.053], burstValleySec: nil)
    guard let c = tswAccepted(e) else { #expect(Bool(false), "expected accepted"); return }
    #expect(c.metrics.validISIValuesCount == 4)                        // short window
    #expect(c.decisionPath.contains("short_compactness"))
    #expect(c.signals.contains { $0.key == "tonic_compactness" && $0.status == .pass })
}

// 2 — short irregular window fails the compactness gate.
@Test
func tswShortIrregularWindowFailsCompactness() {
    let e = tswEvaluate([0.050, 0.052, 0.150, 0.051], burstValleySec: nil)   // 0.150 far outside median band
    #expect(tswRejectReason(e) == .notCompact)
}

// 3 — long regular window (8 ISIs) passes CV/CV2/LV.
@Test
func tswLongRegularWindowPassesCVCV2LV() {
    let e = tswEvaluate([0.050, 0.052, 0.051, 0.053, 0.049, 0.050, 0.052, 0.051], burstValleySec: nil)
    guard let c = tswAccepted(e) else { #expect(Bool(false), "expected accepted"); return }
    #expect(c.metrics.validISIValuesCount == 8)                        // long window
    #expect(c.decisionPath.contains("long_cvcv2lv"))
    #expect(c.signals.contains { $0.key == "tonic_cv" && $0.status == .pass })
    #expect(c.signals.contains { $0.key == "tonic_lv" && $0.status == .pass })
}

// 4 — long irregular window fails a CV/CV2/LV gate.
@Test
func tswLongIrregularWindowFails() {
    let e = tswEvaluate([0.030, 0.080, 0.035, 0.090, 0.028, 0.075, 0.040, 0.085], burstValleySec: nil)
    let reason = tswRejectReason(e)
    #expect(reason == .cvExceeded || reason == .cv2Exceeded || reason == .lvExceeded || reason == .adjacentRatioExceeded)
}

// 5 — a hyper-regular burst-like window passes regularity but is REJECTED by the burst-contamination
// guard using a supplied D3 valley.
@Test
func tswHyperRegularBurstRejectedByGuardWithD3Valley() {
    let e = tswEvaluate([0.004, 0.004, 0.004, 0.004, 0.004, 0.004], burstValleySec: 0.020)
    #expect(tswRejectReason(e) == .burstContamination)
    if case let .rejected(_, signals) = e {
        #expect(signals.contains { $0.key == "burst_contamination" && $0.message.contains("d3_valley") })
    }
}

// 6 — with NO D3 valley, the QC-refractory-derived fallback floor still guards the same burst-like window.
@Test
func tswNilValleyFallbackRefractoryFloorStillGuards() {
    // refractory floor 0.001 × 3 = 0.003; all ISIs 0.002 fall below it.
    let config = TonicStructuralWindowConfig(refractoryFloorSec: 0.001, refractoryFloorMultiplier: 3.0)
    let e = tswEvaluate([0.002, 0.002, 0.002, 0.002, 0.002, 0.002], burstValleySec: nil, config: config)
    #expect(tswRejectReason(e) == .burstContamination)
    if case let .rejected(_, signals) = e {
        #expect(signals.contains { $0.key == "burst_contamination" && $0.message.contains("refractory") })
    }
}

// 7 — scale invariance: scaling every ISI (and the floors/valley) by 10 preserves accept/reject and tier.
@Test
func tswScaleInvariance() {
    let regular = [0.050, 0.052, 0.051, 0.053, 0.049, 0.050, 0.052, 0.051]
    let base = tswEvaluate(regular, burstValleySec: nil)
    let cfg10 = TonicStructuralWindowConfig(refractoryFloorSec: 0.010, refractoryFloorMultiplier: 3.0)
    let scaled = tswEvaluate(regular.map { $0 * 10 }, burstValleySec: nil, config: cfg10)
    #expect(tswAccepted(base) != nil && tswAccepted(scaled) != nil)
    #expect(tswAccepted(base)?.decisionPath == tswAccepted(scaled)?.decisionPath)

    // A burst window rejected at 1× (valley 0.020) is rejected identically at 10× (valley 0.200).
    let burst = [0.004, 0.004, 0.004, 0.004, 0.004, 0.004]
    #expect(tswRejectReason(tswEvaluate(burst, burstValleySec: 0.020)) == .burstContamination)
    #expect(tswRejectReason(tswEvaluate(burst.map { $0 * 10 }, burstValleySec: 0.200)) == .burstContamination)
}

// 8 — no fixed-ms threshold: the SAME data flips accept↔reject only when the configurable QC floor
// changes, proving the burst floor is the sole absolute quantity (no hidden ms cutoff in the gate).
@Test
func tswOnlyConfigFloorIsAbsolute() {
    let window = [0.004, 0.004, 0.004, 0.004, 0.004, 0.004]              // regular; decision hinges on the floor
    // floor 0.001×3 = 0.003 < 0.004 → clean → accepted.
    let low = TonicStructuralWindowConfig(refractoryFloorSec: 0.001, refractoryFloorMultiplier: 3.0)
    #expect(tswAccepted(tswEvaluate(window, burstValleySec: nil, config: low)) != nil)
    // floor 0.002×3 = 0.006 > 0.004 → contaminated → rejected.
    let high = TonicStructuralWindowConfig(refractoryFloorSec: 0.002, refractoryFloorMultiplier: 3.0)
    #expect(tswRejectReason(tswEvaluate(window, burstValleySec: nil, config: high)) == .burstContamination)
}

// 9 — provenance uses only the allowed POSITIVE vocabulary (so the legacy removed peakiness metric can
// never appear): the decision path names a real tier, and every signal key is one of the allowed keys.
@Test
func tswNoRemovedPeakinessMetricInProvenance() {
    let e = tswEvaluate([0.050, 0.052, 0.051, 0.053, 0.049, 0.050, 0.052, 0.051], burstValleySec: nil)
    guard let c = tswAccepted(e) else { #expect(Bool(false), "expected accepted"); return }
    #expect(c.decisionPath.contains("short_compactness") || c.decisionPath.contains("long_cvcv2lv"))
    let allowedKeys: Set<String> = [
        "tonic_window_size", "tonic_compactness", "tonic_cv", "tonic_cv2", "tonic_lv",
        "tonic_adjacent_ratio", "burst_contamination", "tonic_magnitude_floor",
    ]
    for signal in c.signals { #expect(allowedKeys.contains(signal.key)) }
}

// 10 — metric parity: the candidate's metrics equal ISISpanMetrics.from for the same span (no hand-rolled
// CV/CV2/LV).
@Test
func tswMetricParityWithISISpanMetrics() {
    let isis = [0.050, 0.052, 0.051, 0.053, 0.049, 0.050, 0.052, 0.051]
    let train = tswTrain("t", isis: isis)
    let span = ISISpan(trainID: "t", startISIIndex: 1, endISIIndex: isis.count, familyHint: .tonic)
    let direct = ISISpanMetrics.from(train: train, span: span, thresholds: StructuralEvidenceThresholds())
    guard let c = tswAccepted(TonicStructuralWindowDetector.evaluateWindow(
        train: train, span: span, burstValleySec: nil)) else { #expect(Bool(false)); return }
    #expect(c.metrics == direct)
    #expect(c.metrics.cv == direct.cv && c.metrics.cv2 == direct.cv2 && c.metrics.lv == direct.lv)
}

// 11 — index convention: for a span of ISI indices [start, end], endSpikeIndex == endISIIndex + 1 and the
// spike count is nISI + 1.
@Test
func tswSpanToSpikeIndexConvention() {
    let isis = [0.050, 0.052, 0.051, 0.053]
    let train = tswTrain("t", isis: isis)                               // 5 spikes, ISI slots 1...4
    let span = ISISpan(trainID: "t", startISIIndex: 1, endISIIndex: 4, familyHint: .tonic)
    guard let c = tswAccepted(TonicStructuralWindowDetector.evaluateWindow(
        train: train, span: span, burstValleySec: nil)) else { #expect(Bool(false)); return }
    #expect(c.startSpikeIndex == 1)
    #expect(c.endSpikeIndex == 5)                                       // endISIIndex(4) + 1
    #expect(c.endSpikeIndex == c.span.endISIIndex + 1)
    #expect(c.metrics.nSpikes == c.metrics.nISI + 1)
}

// 12 — adjacent-ratio helper: scale-free, nil for < 2 values, catches a lone jump.
@Test
func tswMaxAdjacentRatioHelper() {
    #expect(TonicStructuralWindowDetector.maxAdjacentRatio([0.05]) == nil)
    #expect(TonicStructuralWindowDetector.maxAdjacentRatio([]) == nil)
    let flat = TonicStructuralWindowDetector.maxAdjacentRatio([0.05, 0.05, 0.05])
    #expect(flat != nil && abs((flat ?? 0) - 1.0) < 1e-9)
    let jump = TonicStructuralWindowDetector.maxAdjacentRatio([0.05, 0.10])
    #expect(jump != nil && abs((jump ?? 0) - 2.0) < 1e-9)
    // Scale-free: ×1000 gives the same ratio.
    let scaled = TonicStructuralWindowDetector.maxAdjacentRatio([50.0, 100.0])
    #expect(scaled != nil && abs((scaled ?? 0) - 2.0) < 1e-9)
}

// MARK: - TSW-2 — stride-1 scan + maximal-disjoint emission + boundary provenance.

private func tswScan(
    _ isis: [Double], config: TonicStructuralWindowConfig = TonicStructuralWindowConfig(),
    burstValleySec: Double? = nil, minTonicSpikes: Int? = nil
) -> [TonicStructuralWindowCandidate] {
    TonicStructuralWindowDetector.scan(
        train: tswTrain("t", isis: isis), config: config, burstValleySec: burstValleySec, minTonicSpikes: minTonicSpikes)
}

// TSW-2 #1 — a clean regular tonic run yields ONE candidate covering the full run.
@Test
func tsw2CleanTonicRunProducesOneCandidate() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.051, 0.050, 0.049, 0.051]   // 10 tight ISIs
    let cands = tswScan(isis)
    #expect(cands.count == 1)
    #expect(cands.first?.span.startISIIndex == 1)
    #expect(cands.first?.span.endISIIndex == isis.count)                                // covers [1...10]
    #expect(cands.first?.boundaryReason == nil)                                         // train ends → no boundary
    #expect(cands.first?.reviewRequired == false)
}

// TSW-2 #2 — a tonic run followed by a large pause ISI stops BEFORE the pause; boundary is recorded.
@Test
func tsw2PauseBoundaryStopsBeforePause() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.500]                         // 6 tonic + pause@7
    let cands = tswScan(isis)
    #expect(cands.count == 1)
    #expect(cands.first?.span.endISIIndex == 6)                                         // stops before the pause
    #expect(cands.first?.boundaryReason != nil)                                         // failed expansion recorded
    #expect(cands.first?.reviewRequired == true)
}

// TSW-2 #3 — a hyper-regular burst-like run produces NO tonic candidate (burst guard vetoes every seed).
@Test
func tsw2BurstContaminationProducesNoCandidate() {
    let isis = [Double](repeating: 0.004, count: 8)
    #expect(tswScan(isis, burstValleySec: 0.020).isEmpty)
}

// TSW-2 #4 — two tonic epochs separated by pause ISIs produce TWO candidates, not one.
@Test
func tsw2TwoSeparatedEpochsProduceTwoCandidates() {
    let epochA = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.051, 0.050]                // ISIs 1...8
    let gap = [0.500, 0.480]                                                             // ISIs 9,10
    let epochB = [0.050, 0.049, 0.051, 0.050, 0.052, 0.048, 0.050, 0.051]                // ISIs 11...18
    let cands = tswScan(epochA + gap + epochB)
    #expect(cands.count == 2)
    #expect(cands[0].span.startISIIndex == 1 && cands[0].span.endISIIndex == 8)
    #expect(cands[1].span.startISIIndex == 11 && cands[1].span.endISIIndex == 18)
}

// TSW-2 #5 — a long regular run consolidates overlapping seeds into ONE maximal candidate (not one per seed).
@Test
func tsw2OverlappingSeedsConsolidateToOneMaximal() {
    let isis = (0..<12).map { 0.050 + 0.001 * Double($0 % 3) }                           // 12 tight ISIs
    let cands = tswScan(isis)
    #expect(cands.count == 1)                                                            // NOT 12-4+1 = 9 seeds
    #expect(cands.first?.source == .merged)
    #expect(cands.first?.span.startISIIndex == 1 && cands.first?.span.endISIIndex == 12)
}

// TSW-2 #6 — two passing seeds must NOT merge when the full union fails a regularity gate.
@Test
func tsw2MergeRevalidationRejectsBadUnion() {
    let isis = [0.050, 0.080, 0.050, 0.080, 0.050]   // two 4-ISI seeds compact; 5-ISI union fails CV2
    let cands = tswScan(isis)
    #expect(!cands.contains { $0.span.startISIIndex == 1 && $0.span.endISIIndex == 5 })  // union NOT emitted
    #expect(cands.contains { $0.span.endISIIndex == 4 })                                 // earlier seed survives
}

// TSW-2 #7 — emitted candidates are pairwise DISJOINT and sorted by start (overlap suppression).
@Test
func tsw2CandidatesAreDisjointAndSorted() {
    let block = [0.050, 0.051, 0.049, 0.050, 0.052]
    let isis = block + [0.500] + block + [0.500] + block
    let cands = tswScan(isis)
    #expect(cands.count >= 2)
    for k in 1..<cands.count {
        #expect(cands[k - 1].span.endISIIndex < cands[k].span.startISIIndex)             // disjoint
        #expect(cands[k - 1].span.startISIIndex < cands[k].span.startISIIndex)           // sorted
    }
}

// TSW-2 #8 — the failing expanded (pause) ISI is NOT swallowed into the emitted candidate.
@Test
func tsw2BoundaryEdgeNotSwallowed() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.900]                                // 5 tonic + huge pause@6
    let cands = tswScan(isis)
    #expect(cands.count == 1)
    #expect(cands.first?.span.endISIIndex == 5)                                          // excludes ISI 6
    #expect((cands.first?.span.endISIIndex ?? 99) < 6)
    #expect(cands.first?.boundaryReason != nil)
}

// TSW-2 #9 — span→spike index convention holds for emitted candidates.
@Test
func tsw2ScanSpanToSpikeIndexConvention() {
    let cands = tswScan([0.050, 0.051, 0.049, 0.050, 0.052, 0.048])
    guard let c = cands.first else { #expect(Bool(false), "expected a candidate"); return }
    #expect(c.startSpikeIndex == c.span.startISIIndex)
    #expect(c.endSpikeIndex == c.span.endISIIndex + 1)
    #expect(c.metrics.nSpikes == c.metrics.nISI + 1)
}

// TSW-2 #10 — scale invariance: scaling all ISIs and the floors/valleys by the same factor preserves the
// candidate spans, sources, and boundary reasons.
@Test
func tsw2ScaleInvariance() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.500, 0.050, 0.051, 0.049, 0.050, 0.052]
    let base = tswScan(isis)
    let cfg10 = TonicStructuralWindowConfig(refractoryFloorSec: 0.010, refractoryFloorMultiplier: 3.0)
    let scaled = tswScan(isis.map { $0 * 10 }, config: cfg10)
    #expect(base.count == scaled.count)
    for (a, b) in zip(base, scaled) {
        #expect(a.span.startISIIndex == b.span.startISIIndex)
        #expect(a.span.endISIIndex == b.span.endISIIndex)
        #expect(a.source == b.source)
        #expect(a.boundaryReason == b.boundaryReason)
    }
    // Burst-reject invariance too.
    let burst = [Double](repeating: 0.004, count: 8)
    #expect(tswScan(burst, burstValleySec: 0.020).isEmpty)
    #expect(tswScan(burst.map { $0 * 10 }, burstValleySec: 0.200).isEmpty)
}

// TSW-2 #11 — boundary provenance uses only the allowed positive tokens (no removed-metric terminology).
@Test
func tsw2BoundaryProvenanceUsesCleanTokens() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.500]
    guard let c = tswScan(isis).first, let reason = c.boundaryReason else {
        #expect(Bool(false), "expected a boundary reason"); return
    }
    #expect(c.decisionPath.contains("right_boundary_fail="))
    #expect(c.decisionPath.contains(reason.rawValue))
    let allowed: Set<TonicWindowBoundaryReason> = [
        .insufficientData, .notCompact, .adjacentRatioExceeded, .cvExceeded, .cv2Exceeded, .lvExceeded,
        .burstContamination, .invalidNextISI,
    ]
    #expect(allowed.contains(reason))
}

// TSW-2 #12 — determinism: repeated scans of the same train + config are identical.
@Test
func tsw2ScanIsDeterministic() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.500, 0.050, 0.051, 0.049, 0.050]
    #expect(tswScan(isis) == tswScan(isis))
}

// TSW-2 #13 (review regression) — a LATER-starting seed that forms a LONGER valid candidate wins over an
// early short one: ISI 1 (0.090) passes the 4-seed but caps [1,5] (CV over max), while the run 2..8 forms
// a longer valid candidate. The earlier [1,4] must NOT be emitted in its place.
@Test
func tsw2LaterStartLongerCandidateWinsOverEarlyShort() {
    let isis = [0.090, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050]
    let cands = tswScan(isis)
    #expect(cands.count == 1)
    #expect(cands.first?.span.startISIIndex == 2)                 // later start wins
    #expect(cands.first?.span.endISIIndex == 8)                  // longer candidate
    #expect(!cands.contains { $0.span.startISIIndex == 1 })      // the early short [1,4] is suppressed
}

// TSW-2 #14 (review regression) — the candidate reaches the MAXIMAL valid span even when the trailing
// minimum seed fails: [1..7]=0.055, [8]=0.030; the 4-seed [5..8] fails compactness, but the full [1..8]
// is valid under the long tier, so the candidate must reach 8 (not truncate at 7).
@Test
func tsw2MaximalExtensionBeyondFailingTrailingSeed() {
    let isis = [0.055, 0.055, 0.055, 0.055, 0.055, 0.055, 0.055, 0.030]
    let cands = tswScan(isis)
    #expect(cands.count == 1)
    #expect(cands.first?.span.startISIIndex == 1)
    #expect(cands.first?.span.endISIIndex == 8)
}

// TSW-2 #15 (review regression) — a QC-invalid (sub-floor) next ISI is recorded as a boundary and NOT
// swallowed: ISI slot 6 = 0.0005 (< minValid 0.001); the tonic run stops at 5 and flags invalidNextISI.
@Test
func tsw2QCInvalidNextISIRecordedAsBoundaryNotSwallowed() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.0005, 0.050, 0.051, 0.049]   // sub-floor ISI at slot 6
    let cands = tswScan(isis)
    guard let first = cands.first(where: { $0.span.startISIIndex == 1 }) else {
        #expect(Bool(false), "expected a leading candidate"); return
    }
    #expect(first.span.endISIIndex == 5)                         // does not cross the sub-floor ISI 6
    #expect(first.boundaryReason == .invalidNextISI)
    #expect(first.reviewRequired == true)
}

// MARK: - TSW-2A — family / magnitude routing.

private func tsw2aMetrics(_ isis: [Double]) -> ISISpanMetrics { ISISpanMetrics.compute(orderedValidISISec: isis) }
private func tsw2aGate(
    _ isis: [Double], valley: Double? = nil, valleySupport: Int? = nil, fallback: Double? = nil,
    config: TonicStructuralWindowConfig = TonicStructuralWindowConfig()
) -> (route: TonicStructuralWindowRoute, signal: EvidenceSignal, trust: String, floorSec: Double) {
    TonicStructuralWindowDetector.classicTonicMagnitudeGate(
        metrics: tsw2aMetrics(isis), burstValleySec: valley, burstValleySupportCount: valleySupport,
        fallbackFloorSec: fallback, config: config)
}
// Default physiological floor = classicTonicMinRefractoryMultiple(15) × refractoryFloorSec(0.001) = 15 ms.

// TSW-2A #1 (5x5 slow tonic stays classic) — a slow regular ~450ms window with a tonic-core-lower fallback
// (well above the physiological floor) routes classic tonic.
@Test
func tsw2aSlowRegularTonicRoutesClassic() {
    let g = tsw2aGate([0.44, 0.46, 0.45, 0.47, 0.45, 0.46], fallback: 0.434)   // D3 tonic core lower ~434ms
    #expect(g.route == .classicTonic)
    #expect(g.trust == "d3_tonic_core_lower")
}

// TSW-2A #2 (KEY regression — Grechishnikova ~6.6-11.5ms NOT classic even with a low q25) — a regular
// window whose median (~9ms) is below the physiological floor is too fast for classic tonic, and its
// fast-dominated q25 (~6ms) is classified ambiguous, not trusted.
@Test
func tsw2aGrechishnikovaFastWindowNotClassicEvenWithLowQ25() {
    let g = tsw2aGate([0.0066, 0.0090, 0.0075, 0.0114, 0.0088, 0.0096], fallback: 0.0060)
    #expect(g.route != .classicTonic)
    #expect(g.route == .tooFastForClassicTonic)          // below the 15ms physiological floor
    #expect(g.trust == "ambiguous_fast_q25")             // the low q25 is not trusted to confirm classic
}

// TSW-2A #3 (fast-dominated q25 is ambiguous) — a window ABOVE the physiological floor whose only guard is
// a fast-dominated q25 cannot confirm classic; it is routed review.
@Test
func tsw2aFastDominatedQ25IsClassifiedAmbiguous() {
    let g = tsw2aGate([0.020, 0.021, 0.019, 0.022, 0.020], fallback: 0.0060)   // median 20ms; q25 6ms (HF regime)
    #expect(g.trust == "ambiguous_fast_q25")
    #expect(g.route == .possibleTonicReview)
    #expect(g.route != .classicTonic)
}

// TSW-2A #4 (reliable valley downgrades a moderately-fast window) — above the physiological floor but below
// a reliable burst valley → high-frequency route, not classic.
@Test
func tsw2aReliableValleyDowngradesModeratelyFastWindow() {
    let g = tsw2aGate([0.020, 0.021, 0.019, 0.022, 0.020], valley: 0.050, valleySupport: 20)
    #expect(g.route != .classicTonic)
    #expect(g.trust == "d3_valley")
    #expect(g.route == .highFrequencyTonic || g.route == .highFrequencySpiking)
}

// TSW-2A #5 (degenerate valley ignored) — a single-point/low-support valley is not trusted: a genuine slow
// tonic is NOT rejected, and a fast window is NOT rescued to classic via the tiny valley.
@Test
func tsw2aDegenerateValleyIgnored() {
    let slow = tsw2aGate([0.44, 0.46, 0.45, 0.47, 0.45, 0.46], valley: 0.00145, valleySupport: 1, fallback: 0.434)
    #expect(slow.route == .classicTonic)                 // not a false rejection
    #expect(slow.trust == "d3_tonic_core_lower")          // degenerate valley ignored, reliable fallback used
    let fast = tsw2aGate([0.0066, 0.0090, 0.0075, 0.0114, 0.0088, 0.0096], valley: 0.00145, valleySupport: 1, fallback: 0.0060)
    #expect(fast.route != .classicTonic)                 // tiny valley did not rescue it to classic
}

// TSW-2A #6 (no reliable guard → review, not classic) — a window above the physiological floor with no
// valley and no fallback routes review rather than confirmed classic tonic.
@Test
func tsw2aNoGuardRoutesReviewNotClassic() {
    let g = tsw2aGate([0.020, 0.021, 0.019, 0.022, 0.020])
    #expect(g.trust == "unavailable")
    #expect(g.route == .possibleTonicReview)
}

// TSW-2A #7 (tooFastForClassicTonic reachable) — the physiological floor is a HARD guard: a window below
// it is too fast for classic tonic even when a reliable valley sits below the window.
@Test
func tsw2aTooFastRouteIsReachable() {
    let g = tsw2aGate([0.008, 0.0085, 0.008, 0.0082, 0.008], valley: 0.004, valleySupport: 20)   // 8ms < 15ms floor
    #expect(g.route == .tooFastForClassicTonic)
}

// TSW-2A #8 (HF family sub-typing by dimensionless spike count) — above the physiological floor but below a
// reliable valley: a long dense run → HF spiking; a short run → HF tonic.
@Test
func tsw2aLongDenseFastRunRoutesHFSpiking() {
    let long = tsw2aGate(Array(repeating: 0.020, count: 40), valley: 0.050, valleySupport: 20)   // nSpikes 41 >= 30
    #expect(long.route == .highFrequencySpiking)
    let short = tsw2aGate([0.020, 0.020, 0.020, 0.020, 0.020], valley: 0.050, valleySupport: 20)
    #expect(short.route == .highFrequencyTonic)
}

// TSW-2A #9 (scale invariance) — scaling ALL ISIs, the valley/fallback, AND the refractory floor by k
// preserves the route (for both a classic and a too-fast case).
@Test
func tsw2aScaleInvarianceWithRefractoryScaled() {
    func classicRoute(_ k: Double) -> TonicStructuralWindowRoute {
        let cfg = TonicStructuralWindowConfig(refractoryFloorSec: 0.001 * k)
        return tsw2aGate([0.50, 0.52, 0.51, 0.53, 0.50].map { $0 * k }, valley: 0.020 * k, valleySupport: 20, config: cfg).route
    }
    #expect(classicRoute(1.0) == .classicTonic)
    #expect(classicRoute(10.0) == .classicTonic)
    func fastRoute(_ k: Double) -> TonicStructuralWindowRoute {
        let cfg = TonicStructuralWindowConfig(refractoryFloorSec: 0.001 * k)
        return tsw2aGate([0.005, 0.0052, 0.005, 0.0053, 0.005].map { $0 * k }, fallback: 0.006 * k, config: cfg).route
    }
    #expect(fastRoute(1.0) == .tooFastForClassicTonic)
    #expect(fastRoute(10.0) == fastRoute(1.0))
}

// TSW-2A #10 (provenance uses allowed positive tokens) — decisionPath carries route + floor tokens from
// the allowed set only (so no removed peakiness metric can appear).
@Test
func tsw2aProvenanceUsesAllowedRouteAndFloorTokens() {
    let train = tswTrain("t", isis: [0.44, 0.46, 0.45, 0.47, 0.45, 0.46])
    let span = ISISpan(trainID: "t", startISIIndex: 1, endISIIndex: 6, familyHint: .tonic)
    guard case let .accepted(c) = TonicStructuralWindowDetector.evaluateWindow(
        train: train, span: span, burstValleySec: nil, fallbackFloorSec: 0.434) else {
        #expect(Bool(false), "expected accepted"); return
    }
    #expect(c.route == .classicTonic)
    #expect(c.decisionPath.contains("route=classicTonic"))
    #expect(c.decisionPath.contains("floor=d3_tonic_core_lower"))
    let allowedTokens: Set<String> = [
        "route=classicTonic", "route=highFrequencyTonic", "route=highFrequencySpiking",
        "route=possibleTonicReview", "route=tooFastForClassicTonic",
        "floor=d3_valley", "floor=d3_tonic_core_lower", "floor=ambiguous_fast_q25", "floor=unavailable",
    ]
    for part in c.decisionPath.split(separator: "/").map(String.init)
    where part.hasPrefix("route=") || part.hasPrefix("floor=") {
        #expect(allowedTokens.contains(part))
    }
}

// TSW-2A #11 (scan-level integration) — maximal-disjoint scan unchanged; slow candidates route classic,
// Grechishnikova-like fast candidates are still emitted as structural evidence but route too-fast, NOT classic.
@Test
func tsw2aScanRoutesSlowClassicAndFastNonClassic() {
    let slow = tswTrain("slow", isis: [0.44, 0.46, 0.45, 0.47, 0.45, 0.46, 0.44, 0.46, 0.45, 0.47])
    let sc = TonicStructuralWindowDetector.scan(train: slow, burstValleySec: nil, fallbackFloorSec: 0.434)
    #expect(!sc.isEmpty)
    #expect(sc.allSatisfy { $0.route == .classicTonic })
    let fast = tswTrain("fast", isis: [0.0066, 0.0090, 0.0075, 0.0114, 0.0088, 0.0096, 0.0075, 0.0090, 0.0080, 0.0100])
    let fc = TonicStructuralWindowDetector.scan(train: fast, burstValleySec: nil, fallbackFloorSec: 0.0060)
    #expect(!fc.isEmpty)                                   // still found as structural evidence
    #expect(fc.allSatisfy { $0.route != .classicTonic })  // but not classic tonic
    #expect(fc.contains { $0.route == .tooFastForClassicTonic })
}

// TSW-2A #12 (physiological floor tracks the configured QC/refractory floor) — a fast window with no
// distribution guard routes tooFast, and the reported floor is classicTonicMinRefractoryMultiple ×
// refractoryFloorSec using the CONFIGURED floor (0.0009), not the 0.001 default.
@Test
func tsw2aPhysiologicalFloorUsesConfiguredRefractoryFloor() {
    let cfg = TonicStructuralWindowConfig(refractoryFloorSec: 0.0009)
    let g = tsw2aGate([0.005, 0.0052, 0.005, 0.0053, 0.005], config: cfg)
    #expect(g.route == .tooFastForClassicTonic)
    #expect(abs(g.floorSec - 15.0 * 0.0009) < 1e-12)   // 15 × 0.0009, NOT 15 × 0.001
}

// MARK: - TSW-3 — boundary refinement (asymmetric edge trim + bounded single bridge; unwired).

private func tswRefine(
    _ isis: [Double], config: TonicStructuralWindowConfig = TonicStructuralWindowConfig(),
    burstValleySec: Double? = nil, burstValleySupportCount: Int? = nil,
    fallbackFloorSec: Double? = nil, pauseFloorSec: Double? = nil, minTonicSpikes: Int? = nil
) -> [TonicStructuralWindowCandidate] {
    TonicStructuralWindowDetector.scanRefined(
        train: tswTrain("t", isis: isis), config: config, burstValleySec: burstValleySec,
        minTonicSpikes: minTonicSpikes, burstValleySupportCount: burstValleySupportCount,
        fallbackFloorSec: fallbackFloorSec, pauseFloorSec: pauseFloorSec)
}

// TSW-3 #1 — LOW-side trim: a leading ISI below the classic-tonic magnitude floor (0.012 < 15×0.001) is
// shrunk off the fast edge; the refined span starts one ISI later and carries TSW-3 provenance.
@Test
func tsw3LowSideTrimDropsLeadingFastISI() {
    let isis = [0.012, 0.020, 0.021, 0.019, 0.020, 0.021]   // leading 0.012 is fast-regime, rest ~0.020
    let scanCands = tswScan(isis)
    #expect(scanCands.first?.span.startISIIndex == 1)        // scan includes the fast leading ISI
    let refined = tswRefine(isis)
    #expect(refined.count == 1)
    guard let c = refined.first else { return }
    #expect(c.source == .refined)
    #expect(c.span.startISIIndex == 2)                       // fast leading ISI trimmed
    #expect(c.span.endISIIndex == 6)
    #expect(c.refinement?.lowSideTrimmed == 1)
    #expect(c.refinement?.highSideTrimmed == 0)
    #expect(c.refinement?.bridgeCount == 0)
    #expect(c.originalSpan?.startISIIndex == 1)              // provenance back to the raw scan span
}

// TSW-3 #2 — ASYMMETRY: with no pause floor supplied, a TRAILING fast ISI is NOT trimmed (only the LOW
// side is trimmed against the classic-tonic floor). Leading 0.012 goes; trailing 0.012 stays.
@Test
func tsw3TrimIsAsymmetricLowSideOnly() {
    let isis = [0.012, 0.020, 0.021, 0.019, 0.020, 0.012]
    let refined = tswRefine(isis)
    guard let c = refined.first else { #expect(Bool(false), "expected a candidate"); return }
    #expect(c.refinement?.lowSideTrimmed == 1)               // leading fast ISI trimmed
    #expect(c.refinement?.highSideTrimmed == 0)              // trailing fast ISI NOT trimmed (asymmetric)
    #expect(c.span.startISIIndex == 2)
    #expect(c.span.endISIIndex == 6)                         // trailing fast ISI retained
}

// TSW-3 #3 — HIGH-side trim: given a pause floor, a trailing pause-like ISI that scan absorbed into a long
// window is shrunk off the slow edge.
@Test
func tsw3HighSideTrimDropsTrailingPauseISI() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.080]   // trailing 0.080 is pause-like (≥ 0.070)
    #expect(tswScan(isis).first?.span.endISIIndex == 7)            // scan absorbs 0.080 into the long window
    let refined = tswRefine(isis, pauseFloorSec: 0.070)
    guard let c = refined.first else { #expect(Bool(false), "expected a candidate"); return }
    #expect(c.source == .refined)
    #expect(c.span.endISIIndex == 6)                              // pause ISI trimmed off the high edge
    #expect(c.refinement?.highSideTrimmed == 1)
    #expect(c.refinement?.lowSideTrimmed == 0)
}

// TSW-3 #4 — bounded single BRIDGE: two regular runs split by exactly one tonic-magnitude-compatible ISI
// (0.095, which breaks the strict adjacent-ratio cap so scan yields TWO) merge into ONE refined window.
@Test
func tsw3BoundedBridgeMergesTwoRuns() {
    let isis = [0.050, 0.051, 0.049, 0.050] + [0.095] + [0.050, 0.049, 0.051, 0.050]   // A, bridge, B
    #expect(tswScan(isis).count == 2)                             // scan splits at the bridge
    let refined = tswRefine(isis)
    #expect(refined.count == 1)                                  // TSW-3 bridges them
    guard let c = refined.first else { return }
    #expect(c.source == .refined)
    #expect(c.span.startISIIndex == 1 && c.span.endISIIndex == 9)
    #expect(c.refinement?.bridgeCount == 1)
    #expect(c.refinement?.bridgeSide == .high)
    #expect(c.originalSpan?.endISIIndex == 4)                    // original = the LEFT run
}

// TSW-3 #5 — the bridge NEVER swallows a BURST ISI: the separating ISI (0.002) is below the D3 burst
// floor (0.010), so no bridge is taken and the two runs stay separate.
@Test
func tsw3BridgeRefusesBurstFloorISI() {
    let isis = [0.050, 0.051, 0.049, 0.050] + [0.002] + [0.050, 0.049, 0.051, 0.050]
    let refined = tswRefine(isis, burstValleySec: 0.010)
    #expect(refined.count == 2)                                  // NOT bridged across the burst ISI
    #expect(refined.allSatisfy { ($0.refinement?.bridgeCount ?? 0) == 0 })
}

// TSW-3 #6 — the bridge NEVER swallows a PAUSE ISI: the same 0.095 separator that bridges in #4 is refused
// once it is at/above the supplied pause floor (0.070), so the runs stay separate.
@Test
func tsw3BridgeRefusesPauseFloorISI() {
    let isis = [0.050, 0.051, 0.049, 0.050] + [0.095] + [0.050, 0.049, 0.051, 0.050]
    let refined = tswRefine(isis, pauseFloorSec: 0.070)
    #expect(refined.count == 2)                                  // 0.095 ≥ pause floor → not bridged
    #expect(refined.allSatisfy { ($0.refinement?.bridgeCount ?? 0) == 0 })
}

// TSW-3 #7 — AT MOST ONE bridge per refined window: three runs split by two bounded ISIs merge the first
// pair (one bridge) but never chain into a single triple-run window.
@Test
func tsw3AtMostOneBridgePerWindow() {
    let run: [Double] = [0.050, 0.051, 0.049, 0.050]
    let isis = run + [0.095] + [0.050, 0.049, 0.051, 0.050] + [0.095] + run
    #expect(tswScan(isis).count == 3)
    let refined = tswRefine(isis)
    #expect(refined.count == 2)                                  // A+B bridged; C stands alone (≤ 1 bridge)
    #expect(refined.first?.refinement?.bridgeCount == 1)
    #expect(refined.first?.span.startISIIndex == 1 && refined.first?.span.endISIIndex == 9)
    #expect(refined.last?.span.startISIIndex == 11 && refined.last?.span.endISIIndex == 14)
    #expect(!refined.contains { $0.span.startISIIndex == 1 && $0.span.endISIIndex == 14 })   // no triple merge
}

// TSW-3 #8 — an already-clean run passes through UNCHANGED: no trim, no bridge, source preserved (not
// .refined), and no TSW-3 provenance is invented.
@Test
func tsw3UnchangedCandidatePassesThroughVerbatim() {
    let isis = [0.050, 0.051, 0.049, 0.050, 0.052, 0.048, 0.051, 0.050, 0.049, 0.051]
    let refined = tswRefine(isis)
    #expect(refined.count == 1)
    guard let c = refined.first else { return }
    #expect(c.source == .merged)                                // scan's source preserved, NOT .refined
    #expect(c.refinement == nil)
    #expect(c.originalSpan == nil)
    #expect(c.span.startISIIndex == 1 && c.span.endISIIndex == 10)
}

// TSW-3 #9 — refinement is scale-invariant: scaling every ISI and the refractory floor by 10 leaves the
// trimmed span indices and the trim count unchanged (the classic-tonic floor is refractory-relative).
@Test
func tsw3RefinementIsScaleInvariant() {
    let isis = [0.012, 0.020, 0.021, 0.019, 0.020, 0.021]
    let base = tswRefine(isis)
    let cfg10 = TonicStructuralWindowConfig(refractoryFloorSec: 0.010)
    let scaled = tswRefine(isis.map { $0 * 10 }, config: cfg10)
    #expect(base.count == scaled.count)
    guard let b = base.first, let s = scaled.first else { #expect(Bool(false), "expected candidates"); return }
    #expect(b.span.startISIIndex == s.span.startISIIndex)       // same trim point at both scales
    #expect(b.span.endISIIndex == s.span.endISIIndex)
    #expect(b.refinement?.lowSideTrimmed == s.refinement?.lowSideTrimmed)
    #expect(b.source == s.source)
}

// TSW-3 #10 (route regression) — a slow regular tonic with a low-side boundary ISI still routes CLASSIC
// after refinement when the D3 tonic-core-lower fallback is supplied: the boundary ISI is trimmed, and the
// surviving slow run is magnitude-compatible with classic tonic.
@Test
func tsw3RefinedSlowTonicRemainsClassicWithD3Fallback() {
    let isis = [0.40, 0.44, 0.46, 0.45, 0.47, 0.45, 0.46, 0.44]   // leading 0.40 below the 0.434 core lower
    let refined = tswRefine(isis, fallbackFloorSec: 0.434)
    #expect(refined.count == 1)
    guard let c = refined.first else { return }
    #expect(c.route == .classicTonic)                            // still classic after refinement
    #expect(c.source == .refined)                               // it WAS refined (low-side trim)
    #expect(c.refinement?.lowSideTrimmed == 1)
    #expect(c.span.startISIIndex == 2 && c.span.endISIIndex == 8)
}

// TSW-3 #11 (route regression) — a Grechishnikova-like fast regular window (6.6–11.5 ms) is STILL emitted
// as structural evidence after `scanRefined`, but NO candidate routes classic tonic; at least one routes
// tooFast. The uniformly-fast window is not gutted by the low-side trim (no tonic core above the floor).
@Test
func tsw3RefinedFastWindowStaysNonClassic() {
    let isis = [0.0066, 0.0090, 0.0075, 0.0114, 0.0088, 0.0096, 0.0075, 0.0090, 0.0080, 0.0100]
    let refined = tswRefine(isis, fallbackFloorSec: 0.0060)
    #expect(!refined.isEmpty)                                    // still surfaced as structural evidence
    #expect(refined.allSatisfy { $0.route != .classicTonic })   // never promoted to classic by refinement
    #expect(refined.contains { $0.route == .tooFastForClassicTonic })
    // uniformly-fast → no tonic core above the floor → not truncated, span preserved from scan.
    #expect(refined.allSatisfy { ($0.refinement?.lowSideTrimmed ?? 0) == 0 })
}
