import Foundation
import STPDCore
import Testing

// MARK: - BCB-1 — in-band contamination edge trimming (detector-only unit tests)
//
// The original BCB-1 core-first trim only removed boundary ISIs OUTSIDE the burst seed band (`!isSeed`). A boundary ISI
// that is technically INSIDE a wide burst band but grossly incompatible with the candidate's OWN compact core (e.g. a
// 105 ms / 164 ms leading ISI attached to a ~10 ms core) slipped through. This adds a candidate-INTERNAL compact-core
// criterion: a boundary ISI is trimmed when it exceeds the median of the REST of the span by the SAME asymmetric
// ratios (leading 2.0× / trailing 3.0×). It is scale-free (a ratio to the candidate's own compact core — no fixed-ms
// cutoff), applies only to boundary ISIs, preserves whole compact moderate bursts, and preserves tolerated tails.
//
// These exercise ONLY ClassicAnchorDetector.detect + ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack.

private func inBandCumulative(_ isis: [Double]) -> [Double] {
    var t: [Double] = [0]
    for d in isis { t.append((t.last ?? 0) + d) }
    return t
}

private func inBandSelectedBursts(_ isis: [Double],
                                  lower: Double, upper: Double, bridge: Double) -> [ClassicAnchorCandidate] {
    let train = SpikeTrain(name: "bcb1_in_band_synthetic", timestampsSec: inBandCumulative(isis))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: lower,
        burstBandUpperSec: upper,
        burstBridgeUpperSec: bridge,
        burstBandSource: .structure,
        burstBandIsStructureDerived: true,
        burstCoreReferenceUpperSec: upper
    )
    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    return ClassicAnchorCandidateArbitrator
        .arbitrateBySemanticTrack(result.candidates)
        .filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
}

/// Detect, then run the arbitration path `arbitrations` times (simulating the pipeline's re-arbitration after its
/// demotion passes), returning the selected burst-family candidates.
private func inBandReArbitratedBurstFamily(_ isis: [Double],
                                           lower: Double, upper: Double, bridge: Double,
                                           arbitrations: Int) -> [ClassicAnchorCandidate] {
    let train = SpikeTrain(name: "bcb1_in_band_synthetic", timestampsSec: inBandCumulative(isis))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: lower,
        burstBandUpperSec: upper,
        burstBridgeUpperSec: bridge,
        burstBandSource: .structure,
        burstBandIsStructureDerived: true,
        burstCoreReferenceUpperSec: upper
    )
    var candidates = ClassicAnchorDetector.detect(train: train, settings: settings).candidates
    for _ in 0..<max(1, arbitrations) {
        candidates = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(candidates)
    }
    return candidates.filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
}

// MARK: 1 — real 5x5 burst_response_1_s [84...86] pattern: trim the GROSS 105.4 ms leading edge, KEEP the retained
// 2-ISI core [85..86] = {44.3, 8.5} as a canonical minimum-size burst (do NOT demote for internal breadth). (AUDIT F1)

@Test
func bcb1InBand_grossLeadingEdgeTrimmed_retainedMinCoreStaysCanonical_105_44_9() {
    // ISI 2 = 105.4 ms (gross boundary/noise, IN-BAND under the wide band) is trimmed; ISI 3..4 = {44.3, 8.5} ms
    // remain — a 2-ISI / 3-spike minimum-size burst — kept canonical even though the retained core is internally broad.
    let bursts = inBandSelectedBursts([0.5, 0.1054, 0.0443, 0.0085, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })   // ISI 84-equivalent (105.4) excluded
    let core = try? #require(bursts.first { $0.startISIIndex == 3 && $0.endISIIndex == 4 })
    #expect(core?.finalLabel.isCanonicalBurstFamily ?? false)                       // ISI 85..86 kept canonical
    #expect(core?.decisionPath.contains("retained_core_meets_min_size") ?? false)
    #expect(core?.decisionPath.contains("bcb1_inband_trim(") ?? false)
}

// MARK: 2 — same for a 164.1 ms in-band leading contamination: keep {33.2, 11.5} canonical

@Test
func bcb1InBand_grossLeadingEdgeTrimmed_retainedMinCoreStaysCanonical_164_33_11() {
    let bursts = inBandSelectedBursts([0.5, 0.1641, 0.0332, 0.0115, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })   // 164.1 excluded
    let core = try? #require(bursts.first { $0.startISIIndex == 3 && $0.endISIIndex == 4 })
    #expect(core?.finalLabel.isCanonicalBurstFamily ?? false)                       // {33.2, 11.5} kept canonical
    #expect(core?.decisionPath.contains("retained_core_meets_min_size") ?? false)
}

// MARK: 3 — a gross edge that CANNOT be removed without dropping below minimum → demote to possible_burst

@Test
func bcb1InBand_grossEdgeBlockedByMinSizeDemotedToPossibleBurst() {
    // [105.4, 8.5]: to remove the gross 105.4 ms leading edge, only the single ISI {8.5} would remain — below the
    // minimum burst size. The gross edge cannot be kept canonical, so the packet is routed to possible_burst.
    let bursts = inBandSelectedBursts([0.5, 0.1054, 0.0085, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    #expect(!bursts.contains { $0.finalLabel.isCanonicalBurstFamily })              // no canonical burst
    #expect(!bursts.contains { $0.startISIIndex == $0.endISIIndex })               // never a one-ISI burst
    let possible = bursts.first { $0.finalLabel == .possibleBurst }
    #expect(possible != nil)
    #expect(possible?.decisionPath.contains("bcb1_inband_trim_blocked_core_below_min") ?? false)
    #expect(possible?.decisionPath.contains("routed_to_possible_burst_below_min_core") ?? false)
}

// MARK: 3d — the blocked-below-min demotion is NEVER re-promoted to canonical by a later arbitration pass

@Test
func bcb1InBand_blockedDemotionNotRePromotedUnderReArbitration() {
    // The pipeline re-arbitrates after its demotion passes; running the arbitration path repeatedly must keep the
    // blocked-below-min packet a possible_burst — never re-promoted to a canonical burst.
    let bursts = inBandReArbitratedBurstFamily([0.5, 0.1054, 0.0085, 0.5],
                                               lower: 0.001, upper: 0.200, bridge: 0.300, arbitrations: 3)
    #expect(!bursts.contains { $0.finalLabel.isCanonicalBurstFamily })   // never re-promoted to canonical
    #expect(bursts.contains { $0.finalLabel == .possibleBurst })         // stays possible_burst
}

// MARK: 3b — a true contaminated burst with a VALID (>= minimum) clean core trims the edge and stays canonical

@Test
func bcb1InBand_leadingContaminationWithValidCore_105_10_11_staysCanonical() {
    // Span [2..4] = {0.105, 0.010, 0.011}: 105 is an in-band leading edge, and the clean core {10,11} is exactly the
    // minimum burst size (2 ISIs) — so the 105 is excluded and the compact core survives as a canonical burst.
    let bursts = inBandSelectedBursts([0.5, 0.105, 0.010, 0.011, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })   // 105 excluded from any burst
    let core = bursts.first { $0.finalLabel.isCanonicalBurstFamily }
    #expect(core != nil)
    #expect((core.map { $0.endISIIndex - $0.startISIIndex + 1 } ?? 0) >= 2)         // >= minimum size
}

@Test
func bcb1InBand_leadingContaminationWithValidCore_105_10_11_12_staysCanonical() {
    // Clean core {10,11,12} is 3 ISIs (comfortably >= minimum) → 105 excluded, compact core canonical.
    let bursts = inBandSelectedBursts([0.5, 0.105, 0.010, 0.011, 0.012, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })
    let core = bursts.first { $0.finalLabel.isCanonicalBurstFamily }
    #expect(core != nil)
    #expect((core.map { $0.endISIIndex - $0.startISIIndex + 1 } ?? 0) >= 3)
}

// MARK: 3c — INVARIANT: no selected canonical burst is ever below the minimum size (2 ISIs / 3 spikes)

@Test
func bcb1InBand_noCanonicalBurstIsBelowMinimumSize() {
    for arr in [[0.5, 0.105, 0.044, 0.009, 0.5], [0.5, 0.164, 0.033, 0.012, 0.5]] {
        let bursts = inBandSelectedBursts(arr, lower: 0.001, upper: 0.200, bridge: 0.300)
        for b in bursts where b.finalLabel.isCanonicalBurstFamily {
            #expect(b.endISIIndex - b.startISIIndex + 1 >= 2)
            #expect(b.nSpikes >= 3)
        }
    }
}

// MARK: 4 — a WHOLE compact moderate burst [31,40] ms is KEPT (not demoted for being slower than a HF core)

@Test
func bcb1InBand_wholeCompactModerateBurst_31_40_isKept() {
    // 40/31 ≈ 1.29× ≪ 3.0× trailing ratio → not an edge. The whole moderate burst is preserved, untrimmed.
    let bursts = inBandSelectedBursts([0.5, 0.031, 0.040, 0.5], lower: 0.001, upper: 0.050, bridge: 0.100)
    let burst = bursts.first { $0.startISIIndex == 2 && $0.endISIIndex == 3 }
    #expect(burst != nil)
    #expect(burst?.finalLabel.isCanonicalBurstFamily ?? false)
    #expect(!(burst?.decisionPath.contains("core_first_boundary_trim") ?? true))   // kept whole, no trim
}

// MARK: 5 — a moderate TOLERATED TAIL [26,20,44] ms is KEPT (44 ms ≈ 1.9× core, under 3.0× trailing tolerance)

@Test
func bcb1InBand_moderateToleratedTail_26_20_44_isKept() {
    // Trailing 44 ms vs median({26,20})=23 ms → 1.91× < 3.0× trailing tolerance → a tolerated natural tail, KEPT.
    let bursts = inBandSelectedBursts([0.5, 0.026, 0.020, 0.044, 0.5], lower: 0.001, upper: 0.050, bridge: 0.100)
    let burst = bursts.first { $0.startISIIndex == 2 && $0.endISIIndex == 4 }
    #expect(burst != nil)
    #expect(burst?.finalLabel.isCanonicalBurstFamily ?? false)
    #expect(!(burst?.decisionPath.contains("right=1") ?? true))   // trailing tail NOT trimmed
}

// MARK: 6 — a moderate TOLERATED TAIL [11,17,33] ms is KEPT (33 ms ≈ 2.4× core, under 3.0× trailing tolerance)

@Test
func bcb1InBand_moderateToleratedTail_11_17_33_isKept() {
    // Trailing 33 ms vs median({11,17})=14 ms → 2.36× < 3.0× trailing tolerance → a tolerated natural tail, KEPT.
    let bursts = inBandSelectedBursts([0.5, 0.011, 0.017, 0.033, 0.5], lower: 0.001, upper: 0.050, bridge: 0.100)
    let burst = bursts.first { $0.startISIIndex == 2 && $0.endISIIndex == 4 }
    #expect(burst != nil)
    #expect(burst?.finalLabel.isCanonicalBurstFamily ?? false)
    #expect(!(burst?.decisionPath.contains("right=1") ?? true))   // trailing tail NOT trimmed
}
