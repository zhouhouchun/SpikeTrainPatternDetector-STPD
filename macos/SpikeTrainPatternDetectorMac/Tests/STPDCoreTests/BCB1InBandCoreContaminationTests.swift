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

// MARK: 1 — in-band leading contamination: 105 ms edge trimmed, compact core kept as canonical burst

@Test
func bcb1InBand_leadingContamination105msTrimmedCompactCorePreserved() {
    // Wide band (upper 0.200) so the 105 ms leading ISI is IN-BAND (a seed) — the original !isSeed criterion cannot
    // touch it. Span [2..4] = {0.105, 0.044, 0.009}. 105/median({44,9})=3.96× and 44/9=4.9× both exceed the 2.0×
    // leading ratio → both incompatible leading edges trimmed; the compact core (ISI 4) survives as a burst.
    let bursts = inBandSelectedBursts([0.5, 0.105, 0.044, 0.009, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    // The 105 ms leading contamination (ISI index 2) is in NO selected burst.
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })
    // A compact core survives as a canonical burst, anchored on the tightest ISI (index 4).
    let core = bursts.first { $0.endISIIndex == 4 }
    #expect(core != nil)
    #expect(core?.finalLabel.isCanonicalBurstFamily ?? false)
    #expect(core?.decisionPath.contains("in_band_edge_incompatible_with_compact_core") ?? false)
}

// MARK: 2 — in-band leading contamination: 164 ms edge trimmed, compact core kept as canonical burst

@Test
func bcb1InBand_leadingContamination164msTrimmedCompactCorePreserved() {
    // Span [2..4] = {0.164, 0.033, 0.012}. 164/median({33,12})=7.3× and 33/12=2.75× exceed the 2.0× leading ratio →
    // trimmed; the compact core (ISI 4) survives as a canonical burst.
    let bursts = inBandSelectedBursts([0.5, 0.164, 0.033, 0.012, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })   // 164 ms edge excluded
    let core = bursts.first { $0.endISIIndex == 4 }
    #expect(core != nil)
    #expect(core?.finalLabel.isCanonicalBurstFamily ?? false)
    #expect(core?.decisionPath.contains("in_band_edge_incompatible_with_compact_core") ?? false)
}

// MARK: 3 — in-band trim carries the required provenance tokens

@Test
func bcb1InBand_provenanceTokensPresentOnInBandTrim() {
    let bursts = inBandSelectedBursts([0.5, 0.105, 0.044, 0.009, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    let core = try? #require(bursts.first { $0.endISIIndex == 4 })
    let path = core?.decisionPath ?? ""
    #expect(path.contains("core_first_boundary_trim"))
    #expect(path.contains("in_band_edge_incompatible_with_compact_core"))
    #expect(path.contains("edge_ratio_to_candidate_core"))
    #expect(path.contains("compact_core_preserved"))
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
