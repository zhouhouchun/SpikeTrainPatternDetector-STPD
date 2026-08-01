import Foundation
import STPDCore
import Testing

@Test
func classicAnchorDetectsTwoSidedBurstRun() throws {
    let train = SpikeTrain(
        name: "unit_1",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.118, 0.205, 0.260, 0.320]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBandSource: .userPatternISILimit
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(result.candidates.filter { $0.candidateLayer == "structure_first_classic_burst_anchor" }.count == 1)
    #expect(candidate.candidateLayer == "structure_first_classic_burst_anchor")
    #expect(candidate.candidateClass == "structure_first_two_sided_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_two_sided_classic_burst_i_pass")
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.startISIIndex == 2)
    #expect(candidate.endISIIndex == 4)
    #expect(candidate.startSpikeIndex == 2)
    #expect(candidate.endSpikeIndex == 5)
    #expect(candidate.nISI == 3)
    #expect(candidate.nSpikes == 4)
    #expect(isClose(candidate.preGapSec, 0.100))
    #expect(isClose(candidate.postGapSec, 0.087))
    #expect(isClose(candidate.intraQ90Sec, 0.006))
    #expect(isClose(candidate.edgeContrastMinQ90, 14.5, tolerance: 1e-10))
    #expect(candidate.anchorBandSource == .structure)
}

@Test
func classicAnchorBridgesSingleInternalISIWithinBurstRun() throws {
    let train = SpikeTrain(
        name: "unit_bridge",
        timestampsSec: [0, 0.100, 0.111, 0.122, 0.1386, 0.144, 0.230]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.015,
        burstBridgeUpperSec: 0.020
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.finalLabel == .burst })

    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.candidateLayer == "structure_first_classic_burst_anchor")
    #expect(candidate.candidateClass == "structure_first_two_sided_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_two_sided_classic_burst_i_pass")
    #expect(candidate.startISIIndex == 2)
    #expect(candidate.endISIIndex == 5)
    #expect(candidate.nISI == 4)
    #expect(candidate.nSpikes == 5)
    #expect(isClose(candidate.maxIntraISISec, 0.0166))
    #expect((candidate.edgeContrastMinQ90 ?? 0) > candidate.anchorContrastMinRequired)
    #expect(candidate.burstSeedRunStartISI == 2)
    #expect(candidate.burstSeedRunEndISI == 5)
    #expect(isClose(candidate.burstSeedBandLowerSec, settings.minValidISISec))
    #expect(isClose(candidate.burstSeedBandUpperSec, candidate.intraQ90Sec ?? 0))
    #expect(isClose(candidate.burstBridgeBandUpperSec, candidate.intraQ90Sec ?? 0))
    #expect(isClose(candidate.burstContrastRequired, 3.0))
    #expect(isClose(candidate.burstPossibleContrastRequired, 2.0))
    let intraQ90Sec = try #require(candidate.intraQ90Sec)
    #expect(isClose(candidate.burstRequiredGapSec, intraQ90Sec * 3.0))
    #expect(isClose(candidate.burstPossibleRequiredGapSec, intraQ90Sec * 2.0))
    #expect(candidate.burstStrictBoundaryPass == true)
    #expect(candidate.burstPossibleBoundaryPass == true)
    #expect(candidate.burstBridgeCountPass == true)
    #expect(candidate.burstBridgeFractionPass == true)
    #expect(candidate.burstQ90BridgePass == true)
    #expect(candidate.burstSizeLabelBeforeReview == "burst")
}

@Test
func classicAnchorRejectsWeakTwoSidedContrast() throws {
    let train = SpikeTrain(
        name: "unit_weak",
        timestampsSec: [0, 0.011, 0.017, 0.023, 0.029, 0.040, 0.052]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBandSource: .userPatternISILimit
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first)

    #expect(result.candidates.count == 1)
    #expect(candidate.finalLabel == .reject)
    #expect(candidate.action == "reject")
    #expect(candidate.gateStatus == "event_grammar_reject")
    #expect(candidate.decisionPath.hasPrefix("flank_contrast_fail"))
    #expect(candidate.failureReason.hasPrefix("flank_contrast_fail"))
    #expect(candidate.candidateDiagnosticClass.hasPrefix("rejected__flank_contrast_fail"))
    #expect(!candidate.isEligibleForAutoSelection)
}

@Test
func classicAnchorKeepsBridgeRejectDiagnosticAuditRows() throws {
    let train = SpikeTrain(
        name: "unit_bridge_reject",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.1286, 0.230]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBridgeUpperSec: 0.020,
        burstBandSource: .userPatternISILimit,
        burstBridgeMaxCount: 0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let structureFirst = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(structureFirst.finalLabel == .burst)
    #expect(structureFirst.isEligibleForAutoSelection)
    #expect(structureFirst.anchorBandSource == .structure)
    #expect(!result.candidates.contains { candidate in
        candidate.finalLabel == .reject &&
            candidate.decisionPath.contains("too_many_bridge_isis")
    })
}

@Test
func hardThresholdBurstCandidateMatchesREventCoreSemantics() throws {
    let train = SpikeTrain(
        name: "unit_hard_threshold",
        timestampsSec: cumulativeTimestamps([
            0.200,
            0.080,
            0.050,
            0.060,
            0.040,
            0.200
        ])
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.070,
        burstHardThresholdEnabled: true
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let hard = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "isi_profile_hard_threshold_burst"
    })

    #expect(hard.candidateClass == "isi_profile_hard_threshold_burst")
    #expect(hard.finalLabel == .burst)
    #expect(hard.gateStatus == "isi_profile_hard_threshold_burst_pass")
    #expect(hard.decisionPath == "hard_threshold_direct_seed_bridge_without_flank_contrast_gate")
    #expect(hard.action == "accept")
    #expect(hard.priority == 1_450)
    #expect(isClose(hard.score, 33.0))
    #expect(hard.startISIIndex == 2)
    #expect(hard.endISIIndex == 5)
    #expect(hard.nSpikes == 5)
    #expect(hard.hardThreshold == true)
    #expect(hard.thresholdMode == "hard_threshold")
    #expect(hard.hardThresholdPattern == "burst")
    #expect(isClose(hard.hardBurstSeedUpperSec, 0.070))
    #expect(isClose(hard.hardBurstBridgeUpperSec, 0.0875))
    #expect(hard.hardBurstCoreISICount == 3)
    #expect(hard.hardThresholdSource == "ui_isi_profile_threshold_line")
}

@Test
func classicAnchorStructuralRescueAcceptsCompactClusterWithWeakLocalFlanks() throws {
    let isi = [
        0.450, 0.450, 0.450, 0.450, 0.450,
        0.090,
        0.040, 0.040, 0.040, 0.040, 0.070,
        0.090,
        0.450, 0.450, 0.450
    ]
    var timestamps = [0.0]
    for value in isi {
        timestamps.append((timestamps.last ?? 0) + value)
    }
    let train = SpikeTrain(name: "unit_structural_rescue", timestampsSec: timestamps)
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.050,
        burstBridgeUpperSec: 0.080,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 2.5,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let rescued = try #require(result.candidates.first { candidate in
        candidate.startISIIndex == 7 &&
            candidate.endISIIndex == 11 &&
            candidate.gateStatus == "event_grammar_structural_burst_rescue_pass"
    })

    #expect(rescued.finalLabel == .burst)
    #expect(rescued.action == "accept")
    #expect(rescued.anchorLockLevel == .lockedClassic)
    #expect(rescued.candidateClass == "event_grammar_seed_centered_burst")
    #expect(rescued.decisionPath == "compact_short_isi_cluster_rescued_by_train_scale_compression__burst")
    #expect((rescued.edgeContrastMinQ90 ?? 0) < rescued.anchorContrastMinRequired)
    #expect(rescued.isEligibleForAutoSelection)
}

@Test
func classicAnchorEpisodeRescuesDenseLowISIPacketOutsideStrictSeedRun() throws {
    let isi = [
        0.200,
        0.009, 0.030, 0.031, 0.029, 0.032,
        0.200, 0.200, 0.200
    ]
    var timestamps = [0.0]
    for value in isi {
        timestamps.append((timestamps.last ?? 0) + value)
    }
    let train = SpikeTrain(name: "unit_episode_rescue", timestampsSec: timestamps)
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBridgeUpperSec: 0.020,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 2.5,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { candidate in
        candidate.candidateLayer == "event_grammar_burst_episode" &&
            candidate.startISIIndex == 2 &&
            candidate.endISIIndex == 6 &&
            candidate.isEligibleForAutoSelection
    })
    #expect(result.candidates.allSatisfy { candidate in
        guard candidate.candidateLayer == "event_grammar_burst_episode" else {
            return true
        }
        guard let seedStart = candidate.burstSeedRunStartISI,
              let seedEnd = candidate.burstSeedRunEndISI else {
            return false
        }
        return seedEnd - seedStart + 1 >= settings.burstEpisodeMinSeedISI
    })
    #expect(!result.candidates.contains { candidate in
        candidate.candidateLayer == "event_grammar_burst_event" &&
            candidate.startISIIndex == 2 &&
            candidate.endISIIndex == 6 &&
            candidate.finalLabel == .burst
    })
}

@Test
func adaptiveLocalHFBurstPacketIsDetectedForTwoSidedCompressedPacketAboveTheBand() throws {
    // Reproduces the user false negative (GPi RT1D3.999_SPK 01a, packet ~0.18-0.24 s): a short fast
    // cluster (~9-11 ms ISIs) that is locally compressed — faster than its slower ~22 ms local
    // background and bounded on BOTH sides by several-fold-larger gaps. Its q90 sits ABOVE the
    // learned burst band (8 ms), so the seed-centred routes never seed it and the train-global
    // compactness ceiling (q80 * 0.35 ~ a few ms) rejects it in structure-first. The adaptive route
    // admits it via `band * headroom` (capped by the mandatory local-compression self-gate).
    let background = Array(repeating: 0.022, count: 12)
    let packet = [0.0090, 0.0105, 0.0095, 0.0110, 0.0098, 0.0102, 0.0096]   // 7 ISIs, ~9-11 ms
    let isi = background + [0.035] + packet + [0.040] + background
    let train = SpikeTrain(name: "unit_adaptive_hf_packet_a", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.008,        // learned burst band: below the packet q90 (~10.7 ms)
        burstBridgeUpperSec: 0.008,      // bridge ≈ band (the RT1D3.999 condition that hid Packet B)
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)

    let packetCandidate = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "adaptive_local_hf_burst_packet"
    })
    #expect(packetCandidate.finalLabel.isCanonicalBurstFamily)
    #expect(packetCandidate.stateHighFrequencySubtype == "hf_burst_packet")
    #expect(packetCandidate.isEligibleForAutoSelection)
    #expect(packetCandidate.action == "accept")
    #expect(packetCandidate.nISI == 7)
    #expect(packetCandidate.candidateClass == "adaptive_two_sided_local_compression_hf_packet")
    #expect(packetCandidate.decisionPath.contains("source=local_flank_compression"))
    #expect(packetCandidate.decisionPath.contains("band_collapsed=false"))
    #expect((packetCandidate.localCompressionQ90Ratio ?? 0) > 1)

    // The packet was a genuine false negative: no OTHER route produced a canonical burst-family
    // candidate covering the same ISI span.
    let otherCanonicalOnSpan = result.candidates.contains { candidate in
        candidate.candidateLayer != "adaptive_local_hf_burst_packet" &&
            candidate.finalLabel.isCanonicalBurstFamily &&
            candidate.startISIIndex <= packetCandidate.startISIIndex &&
            candidate.endISIIndex >= packetCandidate.endISIIndex &&
            candidate.isEligibleForAutoSelection
    }
    #expect(!otherCanonicalOnSpan)
}

@Test
func adaptiveLocalHFBurstPacketIsDetectedForLongerSecondPacketShape() throws {
    // Second user example (packet ~1.09-1.16 s): a 10-ISI packet, longer than the classic
    // structure-first span cap (classicMaxSpikes - 1 = 8), so even with a healthy band the
    // structure-first route cannot enumerate it. The adaptive route admits up to
    // highFrequencyBurstMaxSpikes, again gated by local compression.
    let background = Array(repeating: 0.022, count: 12)
    let packet = [0.0095, 0.0100, 0.0098, 0.0102, 0.0096, 0.0101, 0.0097, 0.0103, 0.0099, 0.0100]
    let isi = background + [0.040] + packet + [0.038] + background
    let train = SpikeTrain(name: "unit_adaptive_hf_packet_b", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.008,
        burstBridgeUpperSec: 0.008,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let packetCandidate = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "adaptive_local_hf_burst_packet"
    })
    #expect(packetCandidate.finalLabel.isCanonicalBurstFamily)
    #expect(packetCandidate.stateHighFrequencySubtype == "hf_burst_packet")
    #expect(packetCandidate.isEligibleForAutoSelection)
    #expect(packetCandidate.nISI == 10)
}

@Test
func sustainedHighFrequencyRunIsNotConvertedToAdaptiveHFBurstPackets() throws {
    // Negative control: a long uniform high-frequency run with NO bilateral large boundaries.
    // It is high-frequency activity, but it is not a discrete packet, so the adaptive route must
    // decline (no two-sided several-fold boundaries anywhere). Sustained HF must not be converted
    // wholesale into burst packets.
    let isi = Array(repeating: 0.006, count: 40)
    let train = SpikeTrain(name: "unit_sustained_hf", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.008,
        burstBridgeUpperSec: 0.020,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { $0.candidateLayer == "adaptive_local_hf_burst_packet" })
}

@Test
func oneSidedPacketDoesNotTriggerAdaptiveHFBurstPacket() throws {
    // Boundary guardrail: a compressed run with a large boundary on ONE side only (the other
    // side dissolves into the high-frequency background). The adaptive route requires TWO-sided
    // several-fold boundaries, so it must not fabricate a packet across / beside a single pause.
    let background = Array(repeating: 0.006, count: 12)
    let packet = [0.0090, 0.0102, 0.0095, 0.0110, 0.0088, 0.0100, 0.0096]
    let isi = background + [0.035] + packet + background      // NO trailing large boundary
    let train = SpikeTrain(name: "unit_one_sided_packet", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.008,
        burstBridgeUpperSec: 0.020,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { $0.candidateLayer == "adaptive_local_hf_burst_packet" })
}

@Test
func slowTonicClusterBoundedByLongPausesIsNotAnAdaptiveHFBurstPacket() throws {
    // Defense-in-depth: a genuinely SLOW run (~30 ms ISIs) flanked by long pauses (~100 ms) is a
    // tonic-with-pauses window, NOT a high-frequency burst packet — even when the learned bridge
    // ceiling is (deliberately) inflated to 40 ms. The two-sided/compression gates alone would
    // admit it (100/30 ~ 3.3 on every ratio), so the route must additionally bound the packet
    // core to the learned burst band (band-relative headroom), keeping it inside the HF regime.
    let tonic = Array(repeating: 0.030, count: 12)
    let cluster = Array(repeating: 0.030, count: 7)
    let isi = tonic + [0.100] + cluster + [0.100] + tonic
    let train = SpikeTrain(name: "unit_slow_tonic_pauses", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.008,        // band-relative headroom -> core ceiling ~16 ms
        burstBridgeUpperSec: 0.040,      // deliberately inflated bridge ceiling (~40 ms)
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { $0.candidateLayer == "adaptive_local_hf_burst_packet" })
}

@Test
func slowTonicClusterIsNotAnAdaptiveHFBurstPacketWhenStructureBandIsSlow() throws {
    // Structure-path slow-tonic guard for a SLOW learned burst band. With burstBandUpperSec 18 ms,
    // `band * 2` = 36 ms would admit a ~30 ms slow-tonic cluster (every boundary/compression ratio
    // is ~3.3), so the band ceiling alone is NOT sufficient. The MANDATORY local-compression
    // self-gate (core must be faster than its ~30 ms neighbourhood) rejects it: the cluster core
    // (~30 ms) ≈ its background, ratio ≈ 1, far below the ~1.47 threshold.
    let tonic = Array(repeating: 0.030, count: 12)
    let cluster = Array(repeating: 0.030, count: 7)
    let isi = tonic + [0.100] + cluster + [0.100] + tonic
    let train = SpikeTrain(name: "unit_slow_band_slow_tonic", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.018,        // slow learned band: band * 2 = 36 ms > the 30 ms cluster
        burstBridgeUpperSec: 0.018,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { $0.candidateLayer == "adaptive_local_hf_burst_packet" })
}

@Test
func adaptiveLocalHFBurstPacketIsDetectedWhenBurstBandCollapsedToNoStructure() throws {
    // Second-pass regression for the real screenshot trains (RT1D3.999_SPK 01a / LT1D5.083_SPK 01c)
    // where the resolution is `upper=2ms bridge=2ms source=none`: there is NO usable burst band, so
    // both the seed-centred routes AND the previous `band * 2` core ceiling (~4 ms) reject visually
    // clear HF packets whose internal ISIs are ~5-10 ms. The packet sits in a slower (tonic-like)
    // local background and is bounded by gaps several-fold its median, but its q90-based boundary
    // contrast (~2.7) is just under the strict classic gate, so structure-first also misses it. The
    // collapsed-band fallback (local-background-derived ceiling) must now recover it.
    let background = Array(repeating: 0.030, count: 14)
    let packet = [0.0050, 0.0065, 0.0052, 0.0090, 0.0068, 0.0055, 0.0085]   // median ~6.5, q90 ~8.9 ms
    let isi = background + [0.024] + packet + [0.025] + background
    let train = SpikeTrain(name: "unit_collapsed_band_packet", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.002,        // collapsed/narrow band (mirrors resolution upper=2 ms)
        burstBridgeUpperSec: 0.002,      // collapsed bridge: band ceiling ~4 ms, below the packet
        burstBandSource: .none,          // source=none => canUseSeedBand false (seed routes off)
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let packetCandidate = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "adaptive_local_hf_burst_packet"
    })
    #expect(packetCandidate.finalLabel.isCanonicalBurstFamily)
    #expect(packetCandidate.stateHighFrequencySubtype == "hf_burst_packet")
    #expect(packetCandidate.isEligibleForAutoSelection)
    #expect(packetCandidate.nISI == 7)
    #expect(packetCandidate.decisionPath.contains("band_collapsed=true"))
    #expect(packetCandidate.decisionPath.contains("core_ceiling_source=local_background_q75_fraction"))

    // Genuine false negative: no other route produced a canonical burst-family candidate here.
    let otherCanonicalOnSpan = result.candidates.contains { candidate in
        candidate.candidateLayer != "adaptive_local_hf_burst_packet" &&
            candidate.finalLabel.isCanonicalBurstFamily &&
            candidate.startISIIndex <= packetCandidate.startISIIndex &&
            candidate.endISIIndex >= packetCandidate.endISIIndex &&
            candidate.isEligibleForAutoSelection
    }
    #expect(!otherCanonicalOnSpan)
}

@Test
func slowTonicClusterIsNotAnAdaptiveHFBurstPacketEvenWhenBandCollapsed() throws {
    // Defense-in-depth for the collapsed-band fallback: a genuinely slow ~30 ms cluster flanked by
    // long pauses, with NO usable burst band (source=none). Every boundary/compression ratio is
    // high, so the two-sided gates pass — but the local-background ceiling rejects it, because the
    // cluster core (~30 ms) is NOT faster than its own ~30 ms neighbourhood. Slow tonic must never
    // be converted into an HF burst packet, with or without a collapsed band.
    let tonic = Array(repeating: 0.030, count: 12)
    let cluster = Array(repeating: 0.030, count: 7)
    let isi = tonic + [0.100] + cluster + [0.100] + tonic
    let train = SpikeTrain(name: "unit_collapsed_slow_tonic", timestampsSec: cumulativeTimestamps(isi))
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.002,
        burstBridgeUpperSec: 0.002,
        burstBandSource: .none,
        burstContrastMin: 3.0,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { $0.candidateLayer == "adaptive_local_hf_burst_packet" })
}

@Test
func classicAnchorAcceptsStartBoundaryBurstWithSinglePostFlank() throws {
    let train = SpikeTrain(
        name: "unit_start_boundary",
        timestampsSec: [0, 0.005, 0.010, 0.015, 0.100, 0.160]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(result.candidates.filter { $0.candidateLayer == "structure_first_classic_burst_anchor" }.count == 1)
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.candidateClass == "structure_first_endpoint_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_endpoint_classic_burst_i_pass")
    #expect(candidate.startISIIndex == 1)
    #expect(candidate.endISIIndex == 3)
    #expect(candidate.preGapSec == nil)
    #expect(isClose(candidate.postGapSec, 0.085))
    #expect((candidate.edgeContrastMinQ90 ?? 0) > candidate.anchorContrastMinRequired)
}

@Test
func classicAnchorAcceptsEndBoundaryBurstWithSinglePreFlank() throws {
    let train = SpikeTrain(
        name: "unit_end_boundary",
        timestampsSec: [0, 0.060, 0.150, 0.155, 0.160, 0.165]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(result.candidates.filter { $0.candidateLayer == "structure_first_classic_burst_anchor" }.count == 1)
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.candidateClass == "structure_first_endpoint_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_endpoint_classic_burst_i_pass")
    #expect(candidate.startISIIndex == 3)
    #expect(candidate.endISIIndex == 5)
    #expect(isClose(candidate.preGapSec, 0.090))
    #expect(candidate.postGapSec == nil)
    #expect((candidate.edgeContrastMinQ90 ?? 0) > candidate.anchorContrastMinRequired)
}

@Test
func classicAnchorDemotesOnlyMinimalRefractorySuspectBurst() throws {
    let train = SpikeTrain(
        name: "unit_minimal_refractory",
        timestampsSec: [0, 0.100, 0.1011, 0.1022, 0.205]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        refractorySuspectSec: 0.0015,
        refractoryAction: .demoteToPossible
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first {
        $0.startISIIndex == 2 && $0.endISIIndex == 3
    })

    #expect(candidate.nISI == 2)
    #expect(candidate.finalLabel == .possibleBurst)
    #expect(candidate.refractorySuspectCount == 2)
    #expect(candidate.refractorySuspectAction == .demoteToPossible)
    #expect(candidate.anchorLockLevel == .strongCandidate)
}

@Test
func classicAnchorKeepsLongerRefractorySuspectBurstCanonical() throws {
    let train = SpikeTrain(
        name: "unit_refractory",
        timestampsSec: [0, 0.100, 0.1011, 0.1022, 0.1033, 0.205]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        refractorySuspectSec: 0.0015,
        refractoryAction: .demoteToPossible
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first {
        $0.startISIIndex == 2 && $0.endISIIndex == 4
    })

    #expect(candidate.nISI == 3)
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.refractorySuspectCount == 3)
    #expect(candidate.refractorySuspectAction == .warnOnly)
    #expect(candidate.anchorLockLevel == .lockedClassic)
}

@Test
func optionalPDSTNClassicAnchorSubsetMatchesRReference() throws {
    guard let path = ProcessInfo.processInfo.environment["STPD_COMPLEX_CSV"], !path.isEmpty else {
        return
    }

    let csvURL = URL(fileURLWithPath: path)
    let csv = try String(contentsOf: csvURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "PD_STN",
        sourceDescription: csvURL.path,
        unit: .seconds,
        duplicatePolicy: .collapseExact
    )

    try assertAnchorCounts(
        dataset: dataset,
        trainName: "LT1D00.732F001-nw-11 (flag 1)",
        settings: ClassicAnchorSettings(
            minValidISISec: 0.001,
            burstBandLowerSec: 0.001218035,
            burstBandUpperSec: 0.035
        ),
        expectedCount: 8,
        expectedLockedCount: 4,
        expectedFirstStartISI: 16,
        expectedFirstEndISI: 18,
        expectedFirstLabel: .burst,
        expectedFirstMinContrast: 3.348,
        contrastTolerance: 0.001
    )

    try assertAnchorCounts(
        dataset: dataset,
        trainName: "RT2D03.535_nw-5 (flag 1)",
        settings: ClassicAnchorSettings(
            minValidISISec: 0.001,
            burstBandLowerSec: 0.001315,
            burstBandUpperSec: 0.015
        ),
        expectedCount: 185,
        expectedLockedCount: 102,
        expectedFirstStartISI: 12,
        expectedFirstEndISI: 17,
        expectedFirstLabel: .burst,
        expectedFirstMinContrast: 2.631,
        contrastTolerance: 0.001
    )
}

private func assertAnchorCounts(
    dataset: SpikeDataset,
    trainName: String,
    settings: ClassicAnchorSettings,
    expectedCount: Int,
    expectedLockedCount: Int,
    expectedFirstStartISI: Int,
    expectedFirstEndISI: Int,
    expectedFirstLabel: ClassicAnchorLabel,
    expectedFirstMinContrast: Double,
    contrastTolerance: Double
) throws {
    let train = try #require(dataset.trains.first { $0.name == trainName })
    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let referenceCandidates = result.candidates.filter { candidate in
        candidate.isEligibleForAutoSelection &&
            candidate.candidateClass == "classic_anchor_short_isi_packet"
    }
    let first = try #require(referenceCandidates.first)

    #expect(referenceCandidates.count == expectedCount)
    #expect(referenceCandidates.filter { $0.anchorLockLevel == .lockedClassic }.count == expectedLockedCount)
    #expect(first.startISIIndex == expectedFirstStartISI)
    #expect(first.endISIIndex == expectedFirstEndISI)
    #expect(first.finalLabel == expectedFirstLabel)
    #expect(isClose(first.edgeContrastMinQ90, expectedFirstMinContrast, tolerance: contrastTolerance))
    #expect(result.candidates.contains { $0.finalLabel == .reject && $0.action == "reject" })
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}

private func cumulativeTimestamps(_ isi: [Double]) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(isi.count + 1)
    for value in isi {
        values.append((values.last ?? 0) + value)
    }
    return values
}
