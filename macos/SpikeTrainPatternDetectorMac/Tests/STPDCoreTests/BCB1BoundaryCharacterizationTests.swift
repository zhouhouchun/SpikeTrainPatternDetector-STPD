import Foundation
import STPDCore
import Testing

// MARK: - BCB-1 boundary characterization net (detector-only)
//
// Two kinds of tests:
//   • INVARIANT (A1-A3): properties BCB-1 must always keep — future algorithm slices must NOT break these.
//   • CURRENT-LIMIT CHARACTERIZATION (B4-B6): pins of what BCB-1 does TODAY, given its median-ratio-only trim. These
//     document current limits (no bridge-count/fraction trimming beyond the admission gates, no q90/q95-vs-core edge
//     check, no possible_burst routing of trimmed cores); a future algorithm slice is EXPECTED to update these
//     expectations deliberately.
//
// All tests use ClassicAnchorDetector.detect + arbitrateBySemanticTrack over explicit structural settings — no
// pipeline / band resolver / manual thresholds. Core = seed-band ISIs in [lower, upper]; leading trim ratio 2.0×
// core median, trailing 3.0×; non-seed edges only; core never erased.
//
// ISI indexing note: startISIIndex/endISIIndex are 1-based over the isis array, so isis[0] is ISI index 1. A train
// [flank, edge, core0, core1, core2, flank] places the pre-flank at ISI 1, the leading edge at ISI 2, and the core
// at ISI 3..5. Only cores of >= burstCoreMinISI (3) ISIs are eligible for trimming; a 2-ISI core is left intact.

private func cumulativeChar(_ isis: [Double]) -> [Double] {
    var t: [Double] = [0]
    for d in isis { t.append((t.last ?? 0) + d) }
    return t
}

private func charSelectedBursts(_ isis: [Double],
                                lower: Double = 0.001, upper: Double = 0.012, bridge: Double = 0.080) -> [ClassicAnchorCandidate] {
    let train = SpikeTrain(name: "bcb1_char", timestampsSec: cumulativeChar(isis))
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

// MARK: A — future-stable invariants

// A1: a leading bridge <= the leading ratio limit (2.0x core median) is a COMPATIBLE extension: kept inside the
// span and the candidate stays a canonical burst. INVARIANT.
@Test
func bcb1_invariant_compatibleLeadingBridgeKeptAndCanonical() {
    // leading bridge 0.018 -> ISI 2, core {0.010,0.011,0.012} -> ISI 3..5 (median 0.011).
    // 0.018 = 1.6x median (<= 2.0x limit 0.022) -> compatible -> kept, so the span opens at ISI 2.
    let bursts = charSelectedBursts([0.5, 0.018, 0.010, 0.011, 0.012, 0.5])
    let b = try? #require(bursts.first { $0.startISIIndex == 2 && $0.endISIIndex == 5 })
    #expect(b?.finalLabel.isCanonicalBurstFamily ?? false)         // stays canonical
    #expect(!(b?.decisionPath.contains("left=1") ?? false))        // NOT trimmed
    #expect(!(b?.decisionPath.contains("left=2") ?? false))
}

// A2: a trailing bridge <= the trailing ratio limit (3.0x core median) is compatible: kept, candidate stays canonical.
@Test
func bcb1_invariant_compatibleTrailingBridgeRemainsCanonicalBurst() {
    // core {0.010,0.011,0.012} (median 0.011); trailing bridge 0.020 = 1.8x median (<= 3.0x limit 0.033) -> kept.
    let bursts = charSelectedBursts([0.5, 0.010, 0.011, 0.012, 0.020, 0.5])
    let b = try? #require(bursts.first { $0.startISIIndex <= 2 && $0.endISIIndex >= 5 })
    #expect(b?.endISIIndex == 5)                                   // trailing bridge (idx 5) preserved
    #expect(b?.finalLabel.isCanonicalBurstFamily ?? false)        // stays canonical
    #expect(!(b?.decisionPath.contains("right=1") ?? false))      // NOT trimmed
    #expect(!(b?.decisionPath.contains("right=2") ?? false))
}

// A3: even when BOTH edges are trimmed away, the compact 3-ISI seed core is never lost and remains a canonical
// burst — the trim can shrink the span but never erase the core. INVARIANT.
@Test
func bcb1_invariant_compactCoreNeverLostWhenBothEdgesTrimmed() {
    // leading 0.055 -> ISI 2, core {0.009,0.010,0.011} -> ISI 3..5 (median 0.010), trailing 0.075 -> ISI 6.
    // 0.055 = 5.5x median and 0.075 = 7.5x median: both incompatible -> trimmed, leaving the intact core [3..5].
    let bursts = charSelectedBursts([0.5, 0.055, 0.009, 0.010, 0.011, 0.075, 0.5])
    #expect(bursts.contains { $0.finalLabel.isCanonicalBurstFamily && $0.startISIIndex == 3 && $0.endISIIndex == 5 })
    #expect(!bursts.contains { $0.startISIIndex <= 2 && $0.endISIIndex >= 2 })   // leading 0.055 (ISI 2) excluded
    #expect(!bursts.contains { $0.startISIIndex <= 6 && $0.endISIIndex >= 6 })   // trailing 0.075 (ISI 6) excluded
}

// MARK: B — current-limit characterization (expected to be updated by a future algorithm slice)

// B4: the maximum admitted run of trailing bridge ISIs — each individually <= the trailing ratio limit — is kept
// today. Within the existing admission gates, BCB-1 applies no additional bridge-burden trimming; individually
// ratio-compatible bridges are retained. A future bridge-burden-aware slice may trim/demote such a run; update then.
@Test
func bcb1_characterizesCurrentMedianOnlyTrim_keepsMaxAdmittedCompatibleBridgeBurden() {
    // core {0.010,0.011,0.012} -> ISI 2..4 (median 0.011); 4 trailing bridges 0.020/0.021/0.019/0.022 -> ISI 5..8,
    // each <= 3.0x limit 0.033.
    let bursts = charSelectedBursts([0.5, 0.010, 0.011, 0.012, 0.020, 0.021, 0.019, 0.022, 0.5])
    // CURRENT: the admitted compatible-ratio bridge run is retained in a selected canonical burst (no BCB-1 right trim).
    #expect(bursts.contains { $0.finalLabel.isCanonicalBurstFamily && $0.endISIIndex == 8 })
    #expect(!bursts.contains { $0.decisionPath.contains("right=1") || $0.decisionPath.contains("right=2") })
}

// B5: a trailing edge that is <= the trailing MEDIAN ratio limit but is several times the core's own q90 (it inflates
// the span q95 relative to the tight core) is KEPT today — BCB-1 compares to the core MEDIAN only, not q90/q95. A
// future q90/q95-vs-core slice may trim it; update the expectation then.
@Test
func bcb1_characterizesCurrentMedianOnlyTrim_keepsQ95InflatingCompatibleRatioEdge() {
    // tight core {0.008,0.009,0.008} (median 0.008, q90 ~0.009); trailing bridge 0.022 = 2.75x median (<= 3.0x limit
    // 0.024) but ~2.4x the core q90.
    let bursts = charSelectedBursts([0.5, 0.008, 0.009, 0.008, 0.022, 0.5])
    // CURRENT: the q95-inflating-but-median-compatible edge (idx 5) is retained; the burst stays canonical.
    #expect(bursts.contains { $0.finalLabel.isCanonicalBurstFamily && $0.endISIIndex == 5 })
    #expect(!bursts.contains { $0.decisionPath.contains("right=1") || $0.decisionPath.contains("right=2") })
}

// B6: when BCB-1 trims an incompatible edge, the trimmed candidate STAYS canonical today — it is not routed to a
// possible_burst review tier. A future possible_burst-routing slice may demote marginal trimmed cores; update then.
@Test
func bcb1_characterizesCurrentTrimStaysCanonical_notRoutedToPossibleBurst() {
    // core {0.010,0.011,0.012}; leading 0.060 and trailing 0.070 trimmed -> compact core [3..5].
    let bursts = charSelectedBursts([0.5, 0.060, 0.010, 0.011, 0.012, 0.070, 0.5])
    let trimmed = try? #require(bursts.first { $0.startISIIndex == 3 && $0.endISIIndex == 5 })
    #expect(trimmed?.decisionPath.contains("core_first_boundary_trim") ?? false)   // it WAS trimmed
    #expect(trimmed?.finalLabel.isCanonicalBurstFamily ?? false)                   // CURRENT: stays canonical
    #expect(trimmed?.finalLabel != .possibleBurst)                                 // NOT routed to possible_burst
}
