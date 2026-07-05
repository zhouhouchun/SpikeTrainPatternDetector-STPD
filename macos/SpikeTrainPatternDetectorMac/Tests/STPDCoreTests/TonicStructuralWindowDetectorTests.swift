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
        "tonic_adjacent_ratio", "burst_contamination",
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
