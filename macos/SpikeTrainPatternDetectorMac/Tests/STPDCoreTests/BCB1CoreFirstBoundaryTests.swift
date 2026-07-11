import Foundation
import STPDCore
import Testing

// MARK: - BCB-1 — core-first burst boundary trimming (detector-only unit tests)
//
// The compact seed-band CORE is the anchor; left/right neighbor ISIs are OPTIONAL extensions judged by a scale-free
// RATIO to the candidate's own core median (profile-relative, never a fixed-ms cutoff). An incompatible edge is trimmed
// so the wide candidate collapses to its compatible span; the core is never erased and compatible extensions are kept.
// Thresholds are ASYMMETRIC (leading 2.0× / trailing 3.0×) because a burst has an abrupt onset but a gradually
// lengthening tail — a single symmetric threshold cannot trim a 2.3× leading edge while keeping a ~2.5× natural tail.
//
// These exercise ONLY ClassicAnchorDetector.detect + ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack (no
// pipeline / band resolver / CSV parser), so they build against the detector baseline. The end-to-end pipeline checks
// (real 5x5 fixture, TONIC-BND-1 coexistence, Grechishnikova sanity) live in the pipeline commit.

private func bcb1Cumulative(_ isis: [Double]) -> [Double] {
    var t: [Double] = [0]
    for d in isis { t.append((t.last ?? 0) + d) }
    return t
}

/// Deterministic single-train burst detection over EXPLICIT structural settings (no adaptive band resolution), then
/// semantic-track arbitration — the same path `coreFirstBoundaryTrimmedCandidates` runs on via `ClassicAnchorDetector.detect`.
private func bcb1SelectedBursts(_ isis: [Double],
                                lower: Double = 0.001, upper: Double = 0.012, bridge: Double = 0.080) -> [ClassicAnchorCandidate] {
    let train = SpikeTrain(name: "bcb1_synthetic", timestampsSec: bcb1Cumulative(isis))
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

/// Verbatim ISI sequence of the example dataset's burst_response_2_s (99 ISIs / 100 spikes); detector ISI index
/// i = isis[i-1]. The target packet is at detector indices 64...67 = {0.041, 0.011, 0.017, 0.033}, pause-flanked
/// (ISI 63 = 0.288, ISI 68 = 0.463).
private func bcb1BurstResponse2SInline() -> SpikeTrain {
    let isis: [Double] = [
        0.426, 0.503, 0.404, 0.420, 0.405, 0.450, 0.413, 0.472, 0.451, 0.473,
        0.186, 0.031, 0.040, 0.433, 0.451, 0.422, 0.460, 0.445, 0.434, 0.291,
        0.026, 0.020, 0.044, 0.484, 0.432, 0.464, 0.451, 0.411, 0.407, 0.236,
        0.012, 0.006, 0.016, 0.011, 0.422, 0.454, 0.424, 0.465, 0.472, 0.452,
        0.285, 0.031, 0.028, 0.041, 0.031, 0.438, 0.490, 0.451, 0.451, 0.470,
        0.411, 0.160, 0.037, 0.022, 0.010, 0.021, 0.437, 0.420, 0.461, 0.414,
        0.452, 0.419, 0.288, 0.041, 0.011, 0.017, 0.033, 0.463, 0.431, 0.488,
        0.399, 0.447, 0.436, 0.254, 0.011, 0.023, 0.022, 0.026, 0.012, 0.410,
        0.448, 0.490, 0.438, 0.428, 0.465, 0.236, 0.026, 0.025, 0.019, 0.015,
        0.034, 0.499, 0.476, 0.449, 0.466, 0.450, 0.429, 0.501, 0.481,
    ]
    return SpikeTrain(name: "burst_response_2_s", timestampsSec: bcb1Cumulative(isis))
}

// MARK: 1 — REVISED GROUND TRUTH: burst_response_2_s [64...67] — the leading ~41 ms onset is INCLUDED.
//
// HISTORICAL NOTE (scientific ground-truth revision, not a code correction). This case was previously
// `bcb1_burstResponse2S_leadingSlowEdgeExcluded_detectorOnly`, which required the ~41 ms leading ISI (detector index
// 64) to be TRIMMED, leaving a compact core [65,66] — the "compact-core-only" interpretation. After the user reviewed
// the SIMULATED ground truth, the expectation was REVISED: the ~41 ms onset is a COMPATIBLE MODERATE ONSET of the
// burst packet and must be INCLUDED. Under the two-evidence boundary rule a NON-SEED onset is judged against the
// FORMAL reference = the TRAIN-LOCAL SEED-BAND UPPER (here 0.035 s), NOT the observed seed-core maximum (which is
// recorded as diagnostic only): on the real fixture 40.6 / 35.2 ≈ 1.15 (here 0.041 / 0.035 ≈ 1.17), below the 2.0×
// leading-extension limit → a compatible extension, kept. This supersedes the earlier compact-core-only interpretation.
@Test
func bcb1_burstResponse2S_compatibleModerateOnsetIncluded() {
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.035,
        burstBridgeUpperSec: 0.060,
        burstBandSource: .structure,
        burstBandIsStructureDerived: true,
        burstCoreReferenceUpperSec: 0.035
    )
    let result = ClassicAnchorDetector.detect(train: bcb1BurstResponse2SInline(), settings: settings)
    let selected = ClassicAnchorCandidateArbitrator
        .arbitrateBySemanticTrack(result.candidates)
        .filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }

    // ISI 64 (~41 ms) is now INSIDE a selected canonical burst spanning at least 64...67.
    let burst = selected.first { $0.startISIIndex <= 64 && $0.endISIIndex >= 66 }
    #expect(burst != nil)
    #expect(burst?.startISIIndex == 64)                                       // leading ~41 ms onset INCLUDED
    #expect((burst?.endISIIndex ?? 0) >= 67)                                  // span at least 64...67
    #expect(burst?.finalLabel.isCanonicalBurstFamily ?? false)               // canonical burst
    // The onset is no longer excluded to a 65-start compact core.
    #expect(!selected.contains { $0.startISIIndex == 65 && $0.endISIIndex >= 66 })
}

// MARK: 2 — a compatible (modestly larger) boundary extension is PRESERVED

@Test
func bcb1_compatibleTrailingExtensionIsKept() {
    // Core {0.010,0.011,0.012} with a trailing bridge 0.020 ≈ 1.8× core median (< trailing 3.0× limit) → compatible,
    // kept inside the burst. Pause flanks (0.5) on both sides give a clean two-sided boundary.
    let bursts = bcb1SelectedBursts([0.5, 0.010, 0.011, 0.012, 0.020, 0.5])
    // ISI indices: core 2..4, compatible trailing bridge at 5.
    let burst = bursts.max { $0.endISIIndex - $0.startISIIndex < $1.endISIIndex - $1.startISIIndex }
    #expect(burst?.startISIIndex == 2)
    #expect(burst?.endISIIndex == 5)                                  // the 0.020 trailing extension is preserved
    #expect(!(burst?.decisionPath.contains("right=1") ?? false))      // trailing edge was NOT trimmed
    #expect(!(burst?.decisionPath.contains("right=2") ?? false))
}

// MARK: 3 — large pre- AND post-core ISIs (several× core) are EXCLUDED

@Test
func bcb1_largePreAndPostCoreEdgesAreExcluded() {
    // Core {0.010,0.011,0.012}; leading 0.060 ≈ 5.5× and trailing 0.070 ≈ 6.4× core median — both several× → trimmed.
    let bursts = bcb1SelectedBursts([0.500, 0.060, 0.010, 0.011, 0.012, 0.070, 0.500])
    #expect(bursts.contains {
        $0.startISIIndex == 3 && $0.endISIIndex == 5 &&
            $0.decisionPath.contains("core_first_boundary_trim(left=1,right=1,reason=edge_ratio_to_core)")
    })
    #expect(!bursts.contains { $0.startISIIndex <= 2 && $0.endISIIndex >= 2 })   // leading 0.060 (idx 2) excluded
    #expect(!bursts.contains { $0.startISIIndex <= 6 && $0.endISIIndex >= 6 })   // trailing 0.070 (idx 6) excluded
}

// MARK: 4 — flank-agnostic: a tonic-rate flank (NOT a pause) still lets the incompatible edge be trimmed

@Test
func bcb1_tonicRateFlanksAllowedAndDoNotRequirePauseToTrim() {
    // The incompatible leading bridge 0.050 (≈4.5× core) sits between a tonic-rate baseline (0.40) and the core. BCB-1
    // trims it WITHOUT requiring a pause flank; the regular 0.40 tonic-rate ISIs are never forced into the burst.
    let bursts = bcb1SelectedBursts([0.40, 0.40, 0.40, 0.050, 0.010, 0.011, 0.012, 0.40, 0.40, 0.40])
    // ISI indices: tonic-rate 1..3, leading bridge 4, core 5..7, tonic-rate 8..10.
    let burst = try? #require(bursts.first { $0.startISIIndex >= 4 && $0.endISIIndex <= 7 })
    #expect(burst?.startISIIndex == 5)                                // incompatible leading 0.050 (idx 4) trimmed
    #expect(burst.map { $0.startISIIndex <= 5 && $0.endISIIndex >= 7 } ?? false)
    #expect(!bursts.contains { $0.startISIIndex <= 4 && $0.endISIIndex >= 4 })   // 0.050 not in any selected burst
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(3) })  // tonic-rate flank not swallowed
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(8) })
}
