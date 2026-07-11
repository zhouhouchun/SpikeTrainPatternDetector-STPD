import Foundation
import STPDCore
import Testing

// MARK: - BCB-1 — in-band contamination edge trimming (detector-only unit tests)
//
// The original BCB-1 core-first trim only removed boundary ISIs OUTSIDE the burst seed band (`!isSeed`). A boundary ISI
// that is technically INSIDE a wide burst band but grossly incompatible with the candidate's OWN compact core (e.g. a
// 105 ms / 164 ms leading ISI attached to a ~10 ms core) slipped through. This adds a candidate-INTERNAL compact-core
// criterion: a boundary ISI is trimmed when it exceeds the median of the REST of the span by the SAME asymmetric
// ratios (leading 2.0× / trailing 3.0×) AND lies beyond the retained interior's high end (> restMax — the
// interior-spread guard, so a slow-but-in-range ISI of a bimodal burst is not trimmed). It is scale-free (ratios and
// the candidate's own interior max — no fixed-ms cutoff), applies only to boundary ISIs, and preserves tolerated tails.
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
    // IN-SEED trim → reference is the candidate-internal rest median/max, NOT the train-local band upper.
    #expect(core?.decisionPath.contains("boundary_ref=candidate_internal_rest_median_and_max") ?? false)
    #expect(!(core?.decisionPath.contains("boundary_ref=train_local_seed_band_upper") ?? true))
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
    // [105.4, 8.5] WIDE band (0.200) so 105.4 is an IN-SEED gross ISI: to remove it only {8.5} remains — below the
    // minimum burst size. Routed to possible_burst, tagged as IN-SEED (internal) contamination.
    let bursts = inBandSelectedBursts([0.5, 0.1054, 0.0085, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    #expect(!bursts.contains { $0.finalLabel.isCanonicalBurstFamily })              // no canonical burst
    #expect(!bursts.contains { $0.startISIIndex == $0.endISIIndex })               // never a one-ISI burst
    let possible = bursts.first { $0.finalLabel == .possibleBurst }
    #expect(possible != nil)
    #expect(possible?.decisionPath.contains("bcb1_inband_trim_blocked_core_below_min") ?? false)   // IN-SEED tokens
    #expect(possible?.decisionPath.contains("routed_to_possible_burst_below_min_core") ?? false)
    #expect(possible?.decisionPath.contains("core_below_min_after_trim") ?? false)
    // IN-SEED block → reference is candidate-internal rest median/max, NOT the train-local band upper.
    #expect(possible?.decisionPath.contains("boundary_ref=candidate_internal_rest_median_and_max") ?? false)
    #expect(!(possible?.decisionPath.contains("boundary_ref=train_local_seed_band_upper") ?? true))
    // Not mislabeled as a NON-seed block.
    #expect(!(possible?.decisionPath.contains("non_seed_boundary_contamination_blocked_by_min_size") ?? true))
}

// Blocked NON-SEED contamination: [105.4, 8.5] NARROW band (0.030) so 105.4 is OUT of band. Removing it leaves {8.5}
// (below minimum) → possible_burst tagged as NON-SEED (not described as in-band/internal).
@Test
func bcb1TwoEvidence_blockedNonSeedContaminationTaggedCorrectly() {
    let bursts = inBandSelectedBursts([0.5, 0.1054, 0.0085, 0.5], lower: 0.001, upper: 0.030, bridge: 0.300)
    #expect(!bursts.contains { $0.finalLabel.isCanonicalBurstFamily })
    let possible = bursts.first { $0.finalLabel == .possibleBurst }
    #expect(possible != nil)
    #expect(possible?.decisionPath.contains("non_seed_boundary_contamination_blocked_by_min_size") ?? false)  // NON-SEED
    #expect(possible?.decisionPath.contains("boundary_ref=train_local_seed_band_upper") ?? false)
    #expect(possible?.decisionPath.contains("seed_core_reference_upper_sec=") ?? false)   // the missing reference, now present
    #expect(possible?.decisionPath.contains("core_below_min_after_trim") ?? false)
    #expect(possible?.decisionPath.contains("routed_to_possible_burst_below_min_core") ?? false)
    // A non-seed block must NOT be described as in-band / internal contamination or reference the internal core.
    #expect(!(possible?.decisionPath.contains("in_band_edge_incompatible_with_compact_core") ?? true))
    #expect(!(possible?.decisionPath.contains("boundary_ref=candidate_internal_rest_median_and_max") ?? true))
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

// MARK: 7 — INTERIOR-SPREAD GUARD: a leading in-band ISI that is NOT larger than a retained interior ISI is KEPT
// (a bimodal/alternating burst) — it is not an in-band contamination edge just because it beats the fast-skewed median.

@Test
func bcb1InBand_leadingISIWithinInteriorSpreadIsNotTrimmed() {
    // [23.1, 9.6, 25.3, 7.2] ms — alternating slow/fast. Leading 23.1 ms is 2.4× the rest median (9.6 ms) so the old
    // criterion trimmed it, but it is SMALLER than the retained interior 25.3 ms → within the burst's own spread → KEPT.
    let bursts = inBandSelectedBursts([0.5, 0.0231, 0.0096, 0.0253, 0.0072, 0.5], lower: 0.001, upper: 0.200, bridge: 0.300)
    let core = bursts.first { $0.finalLabel.isCanonicalBurstFamily }
    #expect(core != nil)
    #expect(core.map { $0.startISIIndex <= 2 && $0.endISIIndex >= 5 } ?? false)   // leading 23.1 (ISI 2) NOT trimmed
    #expect(bursts.contains { $0.finalLabel.isCanonicalBurstFamily && ($0.startISIIndex...$0.endISIIndex).contains(2) })
}

// MARK: 8 — real 5x5 two-evidence boundary regressions (A–D)

private func inBand5x5Bursts(_ name: String) throws -> [ClassicAnchorCandidate] {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent()
        .appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    let ds = try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                            datasetName: "5x5", sourceDescription: url.path)
    let train = try #require(ds.trains.first { $0.name == name })
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    return (run.result(for: train.id)?.candidates ?? [])
        .filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }
}

// A — burst_response_1_s ISI 84 = 105.4 ms is contamination: excluded; retained burst covers ISI 85..86.
@Test
func bcb1TwoEvidence_A_burstResponse1S_isi84Excluded_retains85to86() throws {
    let bursts = try inBand5x5Bursts("burst_response_1_s")
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(84) })   // 105.4 excluded
    #expect(bursts.contains { $0.startISIIndex <= 85 && $0.endISIIndex >= 86 })       // retained [85..86] = [44.3,8.5]
}

// B — burst_response_2_s ISI 64 = 40.6 ms is a compatible onset: included; selected span at least 64..67.
@Test
func bcb1TwoEvidence_B_burstResponse2S_isi64Included_span64to67() throws {
    let bursts = try inBand5x5Bursts("burst_response_2_s")
    let packet = try #require(bursts.first { $0.startISIIndex <= 64 && $0.endISIIndex >= 64 })
    #expect(packet.startISIIndex <= 64)     // ISI 64 (40.6) included
    #expect(packet.endISIIndex >= 67)       // span at least 64..67
    // Kept compatible non-seed extension carries boundary provenance (train-local band-upper reference).
    #expect(packet.decisionPath.contains("candidate_relative_compatible_extension"))
    #expect(packet.decisionPath.contains("compatibility_reference=train_local_seed_band_upper"))
    #expect(packet.decisionPath.contains("boundary_ref=train_local_seed_band_upper"))
}

// C — burst_response_4_s ISI 33 = 33.1 ms included; span 33..37; ISI 34 (23.1, bc72614) remains included.
@Test
func bcb1TwoEvidence_C_burstResponse4S_isi33Included_span33to37_isi34Kept() throws {
    let bursts = try inBand5x5Bursts("burst_response_4_s")
    let packet = try #require(bursts.first { $0.startISIIndex <= 34 && $0.endISIIndex >= 34 })
    #expect(packet.startISIIndex <= 33)     // ISI 33 (33.1) included
    #expect(packet.endISIIndex >= 37)       // span 33..37
    #expect((packet.startISIIndex...packet.endISIIndex).contains(34))   // ISI 34 (bc72614) still in
    #expect(packet.decisionPath.contains("candidate_relative_compatible_extension"))
    #expect(packet.decisionPath.contains("compatibility_reference=train_local_seed_band_upper"))
}

// D — burst_response_1_s ISI 11 = 137.7 ms remains Other; the selected burst remains 12..16.
@Test
func bcb1TwoEvidence_D_burstResponse1S_isi11Other_burst12to16() throws {
    let bursts = try inBand5x5Bursts("burst_response_1_s")
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(11) })   // 137.7 remains Other
    #expect(bursts.contains { $0.startISIIndex <= 12 && $0.endISIIndex >= 16 })       // burst 12..16 intact
}

// MARK: 9 — non-seed boundary reference = train-local seed-band upper (observed seed-core max is diagnostic only)

// The formal reference is the TRAIN-LOCAL SEED-BAND UPPER, not the observed seed-core max. Real proof: burst_response_4_s
// packet [8.3, 9.5, 32.9] ms — observed core max 9.5 → 32.9/9.5 = 3.46× (> trailing 3.0) WOULD trim, but 32.9 vs the
// band upper (~27.6) is within 3.0× → the plausible decelerating tail (ISI 26) is KEPT.
@Test
func bcb1TwoEvidence_burstResponse4S_isi26TrailingTailIncluded() throws {
    let bursts = try inBand5x5Bursts("burst_response_4_s")
    #expect(bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(26) })   // 32.9 ms trailing tail kept
    let packet = try #require(bursts.first { $0.startISIIndex <= 26 && $0.endISIIndex >= 26 })
    #expect(packet.startISIIndex <= 24)   // the [8.3, 9.5, 32.9] packet (ISI 24..26)
}

// A non-seed contamination trim records the train-local band-upper reference (formal) plus the observed seed-core max
// (diagnostic only). Also proves the ONE-seed packet still trims (relaxed >=1-seed anchor).
@Test
func bcb1TwoEvidence_trimProvenanceUsesTrainLocalBandUpperAndDiagnosticMax() throws {
    let bursts = inBandSelectedBursts([0.5, 0.1054, 0.0443, 0.0085, 0.5], lower: 0.001, upper: 0.030, bridge: 0.300)
    let core = try #require(bursts.first { $0.startISIIndex == 3 && $0.endISIIndex == 4 })   // [44.3, 8.5] retained
    #expect(core.decisionPath.contains("boundary_ref=train_local_seed_band_upper"))          // FORMAL reference
    #expect(core.decisionPath.contains("seed_core_reference_upper_sec="))
    #expect(core.decisionPath.contains("observed_seed_core_max_sec="))                        // diagnostic only
    #expect(core.decisionPath.contains("boundary_contamination_trim"))
    #expect(core.decisionPath.contains("candidate_relative_compatible_extension"))            // 44.3 kept non-seed
    // A non-seed trim must NOT reference the internal core or carry in-seed tokens.
    #expect(!core.decisionPath.contains("boundary_ref=candidate_internal_rest_median_and_max"))
    #expect(!core.decisionPath.contains("candidate_internal_in_seed_contamination"))
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })             // 105.4 excluded
}

// One-seed COMPATIBLE micro-burst (Grechishnikova-like [15.0, 6.4]): band upper ~11 ms, 15/11 ≈ 1.36× < 2.0 → KEPT canonical.
@Test
func bcb1TwoEvidence_oneSeedCompatibleMicroBurstStaysCanonical() {
    let bursts = inBandSelectedBursts([0.5, 0.015, 0.0064, 0.5], lower: 0.001, upper: 0.011, bridge: 0.050)
    #expect(bursts.contains { $0.finalLabel.isCanonicalBurstFamily && ($0.startISIIndex...$0.endISIIndex).contains(2) })
}

// One-seed CONTAMINATED [105.4, 44.3, 8.5] (narrow band, only 8.5 a seed): 105.4 removed via band-upper fallback,
// [44.3, 8.5] retained canonical.
@Test
func bcb1TwoEvidence_oneSeedContaminated_trims105_retains44_8() {
    let bursts = inBandSelectedBursts([0.5, 0.1054, 0.0443, 0.0085, 0.5], lower: 0.001, upper: 0.030, bridge: 0.300)
    #expect(!bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(2) })   // 105.4 excluded
    let core = try? #require(bursts.first { $0.startISIIndex == 3 && $0.endISIIndex == 4 })
    #expect(core?.finalLabel.isCanonicalBurstFamily ?? false)                        // [44.3, 8.5] canonical
}

// MARK: 10 — burst boundary logic is INDEPENDENT of tonic presence (burst-only / no-tonic regression)

// A train of compact bursts {8, 10, 30} ms separated by 0.5 s inter-burst gaps — NO structurally valid tonic epoch
// and no mid-range regular run. Burst detection AND the trailing-tail boundary extension must work with ZERO tonic
// candidates, and no non-burst ISI may be auto-filled as tonic. This proves the boundary logic does not depend on a
// tonic prior. (The broader `tonic_not_established` / D3 tonic-prior authority question is a separate follow-up.)
@Test
func bcb1TwoEvidence_burstBoundaryIndependentOfTonicPresence() {
    var isis: [Double] = []
    for _ in 0..<4 { isis += [0.5, 0.008, 0.010, 0.030] }   // gap, then a compact {8,10,30} ms burst
    isis += [0.5]
    var t = 0.0; var ts: [Double] = [0.0]
    for x in isis { t += x; ts.append(t) }
    let train = SpikeTrain(name: "no_tonic_bursts", timestampsSec: ts)
    let ds = SpikeDataset(name: "nt", sourceDescription: "synthetic", trains: [train])
    // The COMPLETE pipeline (not just the burst detector) is run so any tonic-state path would show up.
    let cands = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
        .result(for: train.id)?.candidates ?? []
    let bursts = cands.filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }
    // Tonic-LIKE = both tonic-state labels; .highFrequencySpiking is NOT a tonic state and is kept separate.
    let selectedTonicLike = cands.filter {
        $0.selectedForAuto && ($0.finalLabel == .tonic || $0.finalLabel == .highFrequencyTonic)
    }

    #expect(!bursts.isEmpty)                                        // bursts detected with no tonic present
    #expect(selectedTonicLike.isEmpty)                             // NO selected tonic OR high-frequency-tonic candidate
    #expect(bursts.contains { ($0.startISIIndex...$0.endISIIndex).contains(4) })   // trailing 30 ms tail extended in
    for gapIdx in [1, 5, 9, 13] {
        // No large inter-burst gap is covered by a selected tonic-like candidate…
        #expect(!selectedTonicLike.contains { $0.startISIIndex <= gapIdx && $0.endISIIndex >= gapIdx })
        // …and is not silently filled as tonic through any other selected label path.
        let gapCovered = cands.filter { $0.selectedForAuto && $0.startISIIndex <= gapIdx && $0.endISIIndex >= gapIdx }
        #expect(gapCovered.allSatisfy { $0.finalLabel != .tonic && $0.finalLabel != .highFrequencyTonic })
    }
}

// MARK: 11 — MIXED-boundary provenance: one candidate trims a NON-SEED leading edge AND an IN-SEED trailing edge in a
// single pass → the two explicit reference tokens (not the singular boundary_ref) are emitted.
@Test
func bcb1TwoEvidence_mixedBoundaryProvenance_bothTypesTrimmedInOnePass() throws {
    // [0.100, 0.010, 0.010, 0.035] under band upper 0.040: leading 100 ms is NON-SEED (100 > 40×2 = 80) and trailing
    // 35 ms is IN-SEED candidate-internal (35 > restMedian(10,10)×3 = 30 AND 35 > restMax(10,10) = 10). One ISI is
    // trimmed from each side, retaining the two central {10,10} ISIs.
    let bursts = inBandSelectedBursts([0.5, 0.100, 0.010, 0.010, 0.035, 0.5], lower: 0.001, upper: 0.040, bridge: 0.300)
    let core = try #require(bursts.first { $0.finalLabel.isCanonicalBurstFamily })
    #expect(core.startISIIndex == 3)                      // retained exactly the two central ISIs [10,10]
    #expect(core.endISIIndex == 4)

    // Parse the decisionPath into EXACT semicolon-delimited tokens (a naive substring for `boundary_ref=` is unsafe
    // because `non_seed_boundary_ref=` / `in_seed_boundary_ref=` contain that substring).
    let tokens = Set(core.decisionPath.split(separator: ";").map(String.init))

    // Both explicit reference tokens are present, plus the trim tokens.
    #expect(tokens.contains("non_seed_boundary_ref=train_local_seed_band_upper"))
    #expect(tokens.contains("in_seed_boundary_ref=candidate_internal_rest_median_and_max"))
    #expect(tokens.contains("boundary_contamination_trim"))
    #expect(tokens.contains("candidate_internal_in_seed_contamination"))
    #expect(tokens.contains { $0.hasPrefix("seed_core_reference_upper_sec=") })
    #expect(tokens.contains { $0.hasPrefix("core_first_boundary_trim(left=1,right=1") })

    // Neither SINGULAR reference token may be emitted on the mixed path — it uses the two explicit tokens instead.
    #expect(!tokens.contains("boundary_ref=train_local_seed_band_upper"))
    #expect(!tokens.contains("boundary_ref=candidate_internal_rest_median_and_max"))
}
