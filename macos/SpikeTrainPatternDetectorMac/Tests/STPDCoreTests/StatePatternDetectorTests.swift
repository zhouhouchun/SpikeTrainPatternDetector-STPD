import Foundation
@testable import STPDCore
import Testing

@Test
func statePatternDetectorDetectsStableTonicRun() throws {
    let train = SpikeTrain(
        name: "tonic_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 8)
    )

    let result = StatePatternDetector.detect(train: train)
    let tonic = try #require(result.candidates.first { $0.finalLabel == .tonic })

    #expect(tonic.candidateLayer == "event_core_tonic_state")
    #expect(tonic.startISIIndex == 1)
    #expect(tonic.endISIIndex == 8)
    #expect(tonic.startSpikeIndex == 1)
    #expect(tonic.endSpikeIndex == 9)
    #expect(tonic.nSpikes == 9)
    #expect(tonic.nISI == 8)
    #expect(isClose(tonic.anchorBandLowerSec, 0.040))
    #expect(isClose(tonic.anchorBandUpperSec, 0.040))
    #expect(tonic.anchorBandSource == .structure)
    #expect((tonic.lv ?? 1) <= 0.50)
}

@Test
func statePatternDetectorDetectsHighFrequencyTonicRun() throws {
    let train = SpikeTrain(
        name: "hf_tonic_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.015, count: 7)
    )

    let result = StatePatternDetector.detect(
        train: train,
        settings: StatePatternDetectorSettings(
            highFrequencyTonicFloorSec: 0.010,
            highFrequencyTonicUpperSec: 0.030
        )
    )
    let highFrequencyTonic = try #require(result.candidates.first { $0.finalLabel == .highFrequencyTonic })

    #expect(highFrequencyTonic.candidateLayer == "event_core_hf_tonic_state")
    #expect(highFrequencyTonic.startISIIndex == 1)
    #expect(highFrequencyTonic.endISIIndex == 7)
    #expect(highFrequencyTonic.startSpikeIndex == 1)
    #expect(highFrequencyTonic.endSpikeIndex == 8)
    #expect(highFrequencyTonic.nSpikes == 8)
    #expect(highFrequencyTonic.nISI == 7)
    #expect((highFrequencyTonic.cv ?? 1) <= 0.30)
    #expect((highFrequencyTonic.lv ?? 1) <= 0.35)
}

@Test
func stableFastPacketIsNotAcceptedAsTonic() throws {
    // A stable very-short-ISI packet (~7-9 ms here) has excellent CV/CV2/LV but is
    // intra-burst / HFS-like fast structure, not tonic. When the burst seed upper is
    // underestimated (collapsed toward minValidISI, here 3 ms), the burst-seed-fraction
    // veto alone is toothless and the packet would pass as tonic. The adaptive fast
    // structural core (derived here from the non-collapsing HFS short structure) must
    // still exclude it. The 10 ms boundary is NOT hard-coded — it is 0.5 * the HFS short
    // upper supplied through settings, and scales in `fastPacketExclusionFollowsAdaptiveStructuralScale`.
    let train = SpikeTrain(
        name: "fast_packet_tonic",
        timestampsSec: cumulativeTimestamps(fromISI: fastPacketISIs(scale: 1))
    )
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.003,
        highFrequencySpikingShortUpperSec: 0.020
    )

    let result = StatePatternDetector.detect(train: train, settings: settings)

    #expect(!result.candidates.contains { $0.finalLabel == .tonic })
    let reject = try #require(result.candidates.first {
        $0.finalLabel == .reject &&
            $0.decisionPath.contains("reject_tonic_fast_packet_core_occupancy")
    })
    #expect(reject.decisionPath.contains("fast_packet_core_upper_sec="))
    #expect(reject.decisionPath.contains("fast_packet_fraction="))
}

@Test
func stableFastPacketIsNotAcceptedAsHighFrequencyTonic() throws {
    // Same packet, now offered an HF-tonic band that covers it while the HF-tonic floor is
    // also collapsed (3 ms) so the low-tail veto cannot catch it. Low CV/CV2/LV must not
    // override the adaptive fast structural core exclusion.
    let train = SpikeTrain(
        name: "fast_packet_hf_tonic",
        timestampsSec: cumulativeTimestamps(fromISI: fastPacketISIs(scale: 1))
    )
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.003,
        highFrequencyTonicFloorSec: 0.003,
        highFrequencyTonicUpperSec: 0.012,
        highFrequencySpikingShortUpperSec: 0.020
    )

    let result = StatePatternDetector.detect(train: train, settings: settings)

    #expect(!result.candidates.contains { $0.finalLabel == .highFrequencyTonic })
    let reject = try #require(result.candidates.first {
        $0.finalLabel == .reject &&
            $0.decisionPath.contains("reject_hf_tonic_fast_packet_core_occupancy")
    })
    #expect(reject.decisionPath.contains("fast_packet_core_upper_sec="))
}

@Test
func validHighFrequencyTonicAboveFastPacketCoreIsAccepted() throws {
    // Guard against over-blocking: a regular high-frequency run that sits ABOVE the adaptive
    // fast structural core (18 ms vs a 10 ms core) must still be accepted as HF tonic.
    let train = SpikeTrain(
        name: "valid_hf_tonic_above_core",
        timestampsSec: cumulativeTimestamps(repeating: 0.018, count: 8)
    )
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.006,
        highFrequencyTonicFloorSec: 0.010,
        highFrequencyTonicUpperSec: 0.030,
        highFrequencySpikingShortUpperSec: 0.020
    )

    let result = StatePatternDetector.detect(train: train, settings: settings)
    let highFrequencyTonic = try #require(result.candidates.first { $0.finalLabel == .highFrequencyTonic })

    #expect(highFrequencyTonic.candidateLayer == "event_core_hf_tonic_state")
    #expect((highFrequencyTonic.cv ?? 1) <= 0.30)
}

@Test
func fastPacketExclusionFollowsAdaptiveStructuralScale() throws {
    // The exclusion must follow the adaptive structural boundary, not a fixed 1-10 ms rule.
    // Scaling the train AND the structural settings by the same factor must preserve both
    // outcomes: the fast packet stays excluded and the valid HF tonic stays accepted. A
    // non-integer scale (3.7x) avoids any coincidental alignment with a millisecond grid.
    for scale in [1.0, 3.7] {
        let packetTrain = SpikeTrain(
            name: "scaled_fast_packet_\(scale)",
            timestampsSec: cumulativeTimestamps(fromISI: fastPacketISIs(scale: scale))
        )
        let packetSettings = StatePatternDetectorSettings(
            burstSeedUpperSec: 0.003 * scale,
            highFrequencyTonicFloorSec: 0.003 * scale,
            highFrequencyTonicUpperSec: 0.012 * scale,
            highFrequencySpikingShortUpperSec: 0.020 * scale
        )
        let packetResult = StatePatternDetector.detect(train: packetTrain, settings: packetSettings)
        #expect(!packetResult.candidates.contains {
            $0.finalLabel == .tonic || $0.finalLabel == .highFrequencyTonic
        })

        let validTrain = SpikeTrain(
            name: "scaled_valid_hf_tonic_\(scale)",
            timestampsSec: cumulativeTimestamps(repeating: 0.018 * scale, count: 8)
        )
        let validSettings = StatePatternDetectorSettings(
            burstSeedUpperSec: 0.006 * scale,
            highFrequencyTonicFloorSec: 0.010 * scale,
            highFrequencyTonicUpperSec: 0.030 * scale,
            highFrequencySpikingShortUpperSec: 0.020 * scale
        )
        let validResult = StatePatternDetector.detect(train: validTrain, settings: validSettings)
        #expect(validResult.candidates.contains { $0.finalLabel == .highFrequencyTonic })
    }
}

@Test
func relativeSlowUniformHFTonicIsRecalledRelativeToTrainBaseline() throws {
    // The user-observed missed pattern: a sustained, uniform epoch (~26 ms ISIs) that is clearly
    // faster than the train's own ~60 ms background, but too slow for the fixed HFS q80/q90 ceilings
    // and too short (20 spikes) for the >=30-spike sustainedRun route, and outside a deliberately low
    // fixed HF-tonic band. Today it has no detection home; the relative-baseline recall route should
    // select it as .highFrequencyTonic / hf_tonic_spiking.
    let train = SpikeTrain(
        name: "relative_hf_tonic_recall",
        timestampsSec: cumulativeTimestamps(fromISI: relativeHFTonicTrainISIs(scale: 1.0))
    )
    let settings = relativeHFTonicSettings(scale: 1.0)
    let result = StatePatternDetector.detect(train: train, settings: settings)

    let recalled = try #require(result.candidates.first { candidate in
        candidate.finalLabel == .highFrequencyTonic &&
            candidate.decisionPath.contains("hf_tonic_recall_route=relative_baseline")
    })
    #expect(recalled.stateTonicSubtype == "high_frequency")
    #expect(recalled.stateHighFrequencySubtype == "hf_tonic_spiking")
    #expect(recalled.decisionPath.contains("relative_hf_background_q50_sec="))
    #expect(recalled.decisionPath.contains("relative_hf_candidate_q90_background_ratio="))
    #expect(recalled.decisionPath.contains("relative_hf_percentile_ceiling="))
    #expect(recalled.decisionPath.contains("relative_hf_valid_isi_n="))
    #expect((recalled.cv ?? 1) <= settings.highFrequencyTonicCVMax)
    // The recalled state spans (most of) the 20-ISI uniform epoch, not just a fragment.
    #expect(recalled.nISI >= 16)
    // It was genuinely a false negative: the fixed HFS path did not accept it.
    #expect(!result.candidates.contains { $0.finalLabel == .highFrequencySpiking })
}

@Test
func relativeHFTonicRecallFollowsTrainScale() throws {
    // Scaling all ISIs AND the structural settings by a non-integer factor must preserve the same
    // relative decision and boundaries — proving the route is ratio/percentile-based, not fixed-ms.
    for scale in [1.0, 3.7] {
        let train = SpikeTrain(
            name: "relative_hf_tonic_scaled_\(scale)",
            timestampsSec: cumulativeTimestamps(fromISI: relativeHFTonicTrainISIs(scale: scale))
        )
        let result = StatePatternDetector.detect(train: train, settings: relativeHFTonicSettings(scale: scale))
        let recalled = try #require(result.candidates.first { candidate in
            candidate.finalLabel == .highFrequencyTonic &&
                candidate.decisionPath.contains("hf_tonic_recall_route=relative_baseline")
        })
        #expect(recalled.stateTonicSubtype == "high_frequency")
        #expect(recalled.stateHighFrequencySubtype == "hf_tonic_spiking")
        #expect(recalled.nISI >= 16)
    }
}

@Test
func uniformClassicTonicAtBaselineIsNotPromotedToHFTonic() throws {
    // Guardrail for the candidate-vs-background separation: a uniform classic-tonic train has no slow
    // background to be fast relative to — its "fastest" ISIs are only the lower-noise tail of the
    // same baseline. It must NOT be promoted to relative HF tonic; it must stay classic tonic.
    let train = SpikeTrain(
        name: "uniform_classic_tonic_baseline",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 60)
    )
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.006,
        highFrequencyTonicFloorSec: 0.010,
        highFrequencyTonicUpperSec: 0.015,
        highFrequencySpikingShortUpperSec: 0.020
    )
    let result = StatePatternDetector.detect(train: train, settings: settings)

    #expect(!result.candidates.contains { $0.decisionPath.contains("hf_tonic_recall_route=relative_baseline") })
    #expect(!result.candidates.contains { $0.finalLabel == .highFrequencyTonic })
    // It still resolves as classic tonic (no regression to tonic-family detection).
    let tonic = try #require(result.candidates.first { $0.finalLabel == .tonic })
    #expect(tonic.stateTonicSubtype == "classic")
}

@Test
func burstPacketTrainDoesNotBecomeRelativeHFTonicWholesale() throws {
    // Guardrail vs burst-packet morphology: short ultra-fast packets (burst-core ISIs) separated by
    // slow gaps must NOT be merged into a broad relative HF tonic state. The burst-core floor keeps
    // them out of the relative seed, and the burst/fast-packet/classic-boundary vetoes remain active.
    let packet = Array(repeating: 0.005, count: 4)   // burst-core ISIs (below burstSeedUpper)
    var isi: [Double] = []
    for _ in 0..<40 { isi += packet + [0.060] }      // 40 packets each closed by a slow gap
    let train = SpikeTrain(name: "burst_packet_not_hf_tonic", timestampsSec: cumulativeTimestamps(fromISI: isi))
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.008,                     // 5 ms packet ISIs are burst-core
        highFrequencyTonicFloorSec: 0.010,
        highFrequencyTonicUpperSec: 0.015,
        highFrequencySpikingShortUpperSec: 0.020
    )
    let result = StatePatternDetector.detect(train: train, settings: settings)

    #expect(!result.candidates.contains { candidate in
        candidate.finalLabel == .highFrequencyTonic &&
            candidate.decisionPath.contains("hf_tonic_recall_route=relative_baseline")
    })
}

@Test
func relativeHFTonicDoesNotBridgeLongPause() throws {
    // Pause guardrail: two uniform fast epochs separated by a long pause-scale gap must NOT be bridged
    // into one relative HF tonic state. The pause ISI is far above the relative ceiling, so run
    // formation stops at it (the state-transition boundary is preserved).
    let background = Array(repeating: 0.060, count: 20)
    let epoch = Array(repeating: 0.026, count: 15)
    let isi = background + epoch + [0.200] + epoch + background   // long pause between the two epochs
    let train = SpikeTrain(name: "relative_hf_tonic_pause_split", timestampsSec: cumulativeTimestamps(fromISI: isi))
    let settings = relativeHFTonicSettings(scale: 1.0)
    let result = StatePatternDetector.detect(train: train, settings: settings)

    let relativeCandidates = result.candidates.filter { candidate in
        candidate.finalLabel == .highFrequencyTonic &&
            candidate.decisionPath.contains("hf_tonic_recall_route=relative_baseline")
    }
    #expect(!relativeCandidates.isEmpty)                          // the epochs are recalled
    #expect(relativeCandidates.allSatisfy { $0.nISI <= 20 })     // but none bridges the 30-ISI total span
}

@Test
func tonicSubtypeIsClassicForRegularTonic() throws {
    let train = SpikeTrain(
        name: "classic_tonic_subtype",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 12)
    )
    let result = StatePatternDetector.detect(train: train)
    let tonic = try #require(result.candidates.first { $0.finalLabel == .tonic })

    #expect(tonic.stateTonicSubtype == "classic")
    #expect((tonic.cv ?? 1) <= 0.30)
    #expect(tonic.decisionPath.contains("tonic_subtype=classic"))
    #expect(tonic.decisionPath.contains("classic_regularity_pass=true"))
    #expect(tonic.decisionPath.contains("tonic_family_structural_eligibility_pass=true"))
}

@Test
func tonicSubtypeIsHighFrequencyForHighFrequencyTonic() throws {
    let train = SpikeTrain(
        name: "hf_tonic_subtype",
        timestampsSec: cumulativeTimestamps(repeating: 0.015, count: 10)
    )
    let result = StatePatternDetector.detect(
        train: train,
        settings: StatePatternDetectorSettings(
            highFrequencyTonicFloorSec: 0.010,
            highFrequencyTonicUpperSec: 0.030
        )
    )
    let hft = try #require(result.candidates.first { $0.finalLabel == .highFrequencyTonic })

    #expect(hft.stateTonicSubtype == "high_frequency")
    #expect(hft.decisionPath.contains("tonic_subtype=high_frequency"))
}

@Test
func tonicSubtypeIsIrregularForModeratelyVariableSustainedRun() throws {
    // Sustained, moderately-variable run resembling the GPi example (no long silence, no
    // burst/fast-packet dominance, CV/CV2 moderately higher than classic tonic). It must
    // still be tonic-family: finalLabel .tonic with subtype "irregular".
    let train = SpikeTrain(
        name: "irregular_tonic",
        timestampsSec: cumulativeTimestamps(fromISI: irregularTonicISIs())
    )
    let result = StatePatternDetector.detect(train: train)
    // Pick a PRIMARY irregular structural-window candidate (layer event_core_tonic_state, not a boundary-
    // rescue extension) so the per-run subtype-audit provenance asserted below applies. Under the TSW
    // primary path the moderately-variable run is surfaced as irregular structural windows and the per-run
    // gate assigns the irregular subtype from the span's actual cv/cv2/lv.
    let tonic = try #require(result.candidates.first {
        $0.finalLabel == .tonic && $0.stateTonicSubtype == "irregular"
            && $0.candidateLayer == "event_core_tonic_state"
    })

    #expect(tonic.finalLabel == .tonic)
    #expect(tonic.stateTonicSubtype == "irregular")
    #expect((tonic.cv ?? 0) > 0.30)
    #expect((tonic.cv ?? 1) <= 0.60)
    #expect(tonic.decisionPath.contains("tonic_subtype=irregular"))
    #expect(tonic.decisionPath.contains("irregular_tonic"))
    #expect(tonic.decisionPath.contains("classic_regularity_pass=false"))
    #expect(tonic.decisionPath.contains("irregular_regularity_pass=true"))
    #expect(tonic.decisionPath.contains("tonic_family_structural_eligibility_pass=true"))
    #expect(tonic.decisionPath.contains("cv="))
    #expect(tonic.decisionPath.contains("cv2="))
    #expect(tonic.decisionPath.contains("lv="))
    #expect(tonic.decisionPath.contains("max_isi_sec="))
    #expect(tonic.decisionPath.contains("burst_seed_fraction="))
    #expect(tonic.decisionPath.contains("core_burst_run_length="))
    #expect(tonic.decisionPath.contains("fast_packet_fraction="))
    // The irregular candidate itself is a tonic state, not an HFS/burst relabel.
    #expect(tonic.finalLabel != .highFrequencySpiking)
    #expect(!tonic.finalLabel.isBurstEventFamily)
}

@Test
func irregularTonicIsNotABackdoorForBurstFastPacketOrPause() throws {
    // (a) A stable fast packet inside the adaptive fast-packet core stays rejected as
    // tonic / HF-tonic and never becomes irregular tonic.
    let fastTrain = SpikeTrain(
        name: "fp_backdoor",
        timestampsSec: cumulativeTimestamps(fromISI: fastPacketISIs(scale: 1))
    )
    let fastResult = StatePatternDetector.detect(
        train: fastTrain,
        settings: StatePatternDetectorSettings(
            burstSeedUpperSec: 0.003,
            highFrequencySpikingShortUpperSec: 0.020
        )
    )
    #expect(!fastResult.candidates.contains { $0.finalLabel == .tonic })
    #expect(!fastResult.candidates.contains { $0.stateTonicSubtype == "irregular" })

    // (b) A train with strong compact burst packets does not become irregular tonic.
    let burstISI = (0..<8).flatMap { _ in [0.004, 0.004, 0.004, 0.004, 0.400] }
    let burstTrain = SpikeTrain(
        name: "burst_backdoor",
        timestampsSec: cumulativeTimestamps(fromISI: burstISI)
    )
    let burstResult = StatePatternDetector.detect(train: burstTrain)
    #expect(!burstResult.candidates.contains {
        $0.finalLabel == .tonic && $0.stateTonicSubtype == "irregular"
    })

    // (c) A pause-heavy train does not become one irregular tonic spanning the silences.
    let pauseISI = (0..<6).flatMap { _ in Array(repeating: 0.025, count: 6) + [0.400] }
    let pauseTrain = SpikeTrain(
        name: "pause_backdoor",
        timestampsSec: cumulativeTimestamps(fromISI: pauseISI)
    )
    let pauseResult = StatePatternDetector.detect(train: pauseTrain)
    #expect(!pauseResult.candidates.contains { $0.stateTonicSubtype == "irregular" })
}

@Test
func tonicSubtypeAnnotationCarriesSubtypeAndDisplayNames() throws {
    // Classic tonic → "Classic tonic".
    let classicTrain = SpikeTrain(name: "ann_classic", timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 12))
    let classicTonic = try #require(StatePatternDetector.detect(train: classicTrain).candidates.first { $0.finalLabel == .tonic })
    let classicAnn = ClassicAnchorEventAnnotation(candidate: classicTonic, train: classicTrain)
    #expect(classicAnn.stateTonicSubtype == "classic")
    #expect(classicAnn.displaySubtypeName == "Classic tonic")

    // Irregular tonic → "Irregular tonic".
    let irregularTrain = SpikeTrain(name: "ann_irregular", timestampsSec: cumulativeTimestamps(fromISI: irregularTonicISIs()))
    let irregularTonic = try #require(StatePatternDetector.detect(train: irregularTrain).candidates.first {
        $0.finalLabel == .tonic && $0.stateTonicSubtype == "irregular"
    })
    let irregularAnn = ClassicAnchorEventAnnotation(candidate: irregularTonic, train: irregularTrain)
    #expect(irregularAnn.stateTonicSubtype == "irregular")
    #expect(irregularAnn.displaySubtypeName == "Irregular tonic")

    // HF tonic → "HF tonic".
    let hfTrain = SpikeTrain(name: "ann_hf", timestampsSec: cumulativeTimestamps(repeating: 0.015, count: 10))
    let hfTonic = try #require(StatePatternDetector.detect(
        train: hfTrain,
        settings: StatePatternDetectorSettings(highFrequencyTonicFloorSec: 0.010, highFrequencyTonicUpperSec: 0.030)
    ).candidates.first { $0.finalLabel == .highFrequencyTonic })
    let hfAnn = ClassicAnchorEventAnnotation(candidate: hfTonic, train: hfTrain)
    #expect(hfAnn.stateTonicSubtype == "high_frequency")
    #expect(hfAnn.displaySubtypeName == "HF tonic")
}

@Test
func selectedTonicSubtypeCountsAndStatusSummary() throws {
    // Empty summary when no tonic-family candidates.
    #expect(TonicSubtypeCounts(classic: 0, irregular: 0, highFrequency: 0).statusSummary == "")
    #expect(TonicSubtypeCounts(classic: 4, irregular: 9, highFrequency: 2).statusSummary == "Tonic: classic 4, irregular 9, HF 2")

    // Run-level helper counts selected tonic subtypes through the adaptive pipeline.
    let tonicTrain = SpikeTrain(name: "count_irregular", timestampsSec: cumulativeTimestamps(fromISI: irregularTonicPipelineISIs()))
    let dataset = SpikeDataset(name: "subtype counts", sourceDescription: "unit-test", trains: [tonicTrain])
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let counts = run.selectedTonicSubtypeCounts
    #expect(counts.irregular >= 1)
    #expect(counts.total == counts.classic + counts.irregular + counts.highFrequency)
    #expect(!counts.statusSummary.isEmpty)
    #expect(counts.statusSummary.contains("irregular \(counts.irregular)"))
}

@Test
func irregularTonicMicroGapMergeAcceptsTinyNonEventGap() throws {
    // Two irregular-tonic fragments (ISI 1-30 and 32-61) separated by one tiny non-event gap
    // (ISI index 31, 45 ms). The merge accepts and revalidates a single spanning state.
    let train = mergeTestTrain(gapISISec: 0.045)
    let settings = mergeTestSettings()
    var left = testCandidate(id: "irreg_left", label: .tonic, startISIIndex: 1, endISIIndex: 28, priority: 1_100)
    left.stateTonicSubtype = "irregular"
    var right = testCandidate(id: "irreg_right", label: .tonic, startISIIndex: 30, endISIIndex: 57, priority: 1_100)
    right.stateTonicSubtype = "irregular"

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train, candidates: [left, right], selectedEvents: [], selectedGaps: [],
        settings: settings, authorizedFragmentIDs: [left.id, right.id]
    )

    let merged = try #require(result.first { $0.finalLabel == .tonic && $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(merged.stateTonicSubtype == "irregular")
    #expect(merged.decisionPath.contains("irregular_tonic_micro_gap_merge=true"))
    #expect(merged.decisionPath.contains("merge_revalidated=true"))
    #expect(merged.decisionPath.contains("merged_fragment_count=2"))
    #expect(merged.decisionPath.contains("merged_gap_count="))
    #expect(merged.decisionPath.contains("max_merged_gap_sec="))
    #expect(merged.decisionPath.contains("micro_gap_cap_sec="))
    #expect(merged.decisionPath.contains("micro_gap_source=adaptive_neighbor_tonic_scale"))
    // Child fragments are consumed (replaced by the merged state).
    #expect(!result.contains { $0.id == "irreg_left" })
    #expect(!result.contains { $0.id == "irreg_right" })
}

@Test
func irregularTonicMicroGapMergeDoesNotMergeAcrossSelectedPause() throws {
    let train = mergeTestTrain(gapISISec: 0.045)
    let settings = mergeTestSettings()
    var left = testCandidate(id: "irreg_left", label: .tonic, startISIIndex: 1, endISIIndex: 28, priority: 1_100)
    left.stateTonicSubtype = "irregular"
    var right = testCandidate(id: "irreg_right", label: .tonic, startISIIndex: 30, endISIIndex: 57, priority: 1_100)
    right.stateTonicSubtype = "irregular"
    let pause = testCandidate(id: "pause", label: .pause, startISIIndex: 29, endISIIndex: 29, priority: 900)

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train, candidates: [left, right, pause], selectedEvents: [], selectedGaps: [pause],
        settings: settings, authorizedFragmentIDs: [left.id, right.id]
    )

    #expect(!result.contains { $0.finalLabel == .tonic && $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(result.contains { $0.id == "irreg_left" })
    #expect(result.contains { $0.id == "irreg_right" })
}

@Test
func irregularTonicMicroGapMergeDoesNotMergeAcrossSelectedBurstEvent() throws {
    let train = mergeTestTrain(gapISISec: 0.045)
    let settings = mergeTestSettings()
    var left = testCandidate(id: "irreg_left", label: .tonic, startISIIndex: 1, endISIIndex: 28, priority: 1_100)
    left.stateTonicSubtype = "irregular"
    var right = testCandidate(id: "irreg_right", label: .tonic, startISIIndex: 30, endISIIndex: 57, priority: 1_100)
    right.stateTonicSubtype = "irregular"
    let burst = testCandidate(id: "burst", label: .burst, startISIIndex: 29, endISIIndex: 29, priority: 1_500)

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train, candidates: [left, right, burst], selectedEvents: [burst], selectedGaps: [],
        settings: settings, authorizedFragmentIDs: [left.id, right.id]
    )

    #expect(!result.contains { $0.finalLabel == .tonic && $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(result.contains { $0.id == "irreg_left" })
}

@Test
func irregularTonicMicroGapMergeDoesNotMergeAcrossTooLargeGap() throws {
    // Gap (200 ms) is below no formal pause selection here, but clearly above the adaptive cap.
    let train = mergeTestTrain(gapISISec: 0.200)
    let settings = mergeTestSettings()
    var left = testCandidate(id: "irreg_left", label: .tonic, startISIIndex: 1, endISIIndex: 28, priority: 1_100)
    left.stateTonicSubtype = "irregular"
    var right = testCandidate(id: "irreg_right", label: .tonic, startISIIndex: 30, endISIIndex: 57, priority: 1_100)
    right.stateTonicSubtype = "irregular"

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train, candidates: [left, right], selectedEvents: [], selectedGaps: [],
        settings: settings, authorizedFragmentIDs: [left.id, right.id]
    )

    #expect(!result.contains { $0.finalLabel == .tonic && $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(result.contains { $0.id == "irreg_left" })
}

@Test
func irregularTonicMicroGapMergeDoesNotMergeAcrossHFSRegion() throws {
    let train = mergeTestTrain(gapISISec: 0.045)
    let settings = mergeTestSettings()
    var left = testCandidate(id: "irreg_left", label: .tonic, startISIIndex: 1, endISIIndex: 28, priority: 1_100)
    left.stateTonicSubtype = "irregular"
    var right = testCandidate(id: "irreg_right", label: .tonic, startISIIndex: 30, endISIIndex: 57, priority: 1_100)
    right.stateTonicSubtype = "irregular"
    // An eligible HFS candidate overlapping the gap blocks the merge.
    let hfs = testCandidate(
        id: "hfs", label: .highFrequencySpiking,
        startISIIndex: 20, endISIIndex: 35, priority: 1_000
    ).withAutoSelection(selectedForAuto: true, selectionStatus: "selected")

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train, candidates: [left, right, hfs], selectedEvents: [], selectedGaps: [],
        settings: settings, authorizedFragmentIDs: [left.id, right.id]
    )

    #expect(!result.contains { $0.finalLabel == .tonic && $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(result.contains { $0.id == "irreg_left" })
}

@Test
func irregularTonicMicroGapMergeRespectsZeroMaxBridgeISI() throws {
    // Same one-ISI gap that normally merges, but tonicMaxBridgeISI = 0 must disable tonic
    // bridging entirely: even a tiny one-ISI gap must not be merged.
    let train = mergeTestTrain(gapISISec: 0.045)
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.010,
        tonicBridgeUpperSec: 0.075,
        tonicMaxBridgeISI: 0,
        highFrequencySpikingShortUpperSec: 0.020
    )
    var left = testCandidate(id: "irreg_left", label: .tonic, startISIIndex: 1, endISIIndex: 28, priority: 1_100)
    left.stateTonicSubtype = "irregular"
    var right = testCandidate(id: "irreg_right", label: .tonic, startISIIndex: 30, endISIIndex: 57, priority: 1_100)
    right.stateTonicSubtype = "irregular"

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train, candidates: [left, right], selectedEvents: [], selectedGaps: [],
        settings: settings, authorizedFragmentIDs: [left.id, right.id]
    )

    // No merged candidate spanning both fragments; both child fragments remain present.
    #expect(!result.contains { $0.finalLabel == .tonic && $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(result.contains { $0.id == "irreg_left" })
    #expect(result.contains { $0.id == "irreg_right" })
}

@Test
func tonicCandidateCoefficientOfVariationUsesSharedSampleStandard() throws {
    // Mildly jittered regular firing so the sample (n - 1) and population (n) CV
    // diverge; the tonic candidate must report the shared sample-CV (R stats::sd).
    let isi: [Double] = [0.040, 0.043, 0.037, 0.041, 0.039, 0.042, 0.038, 0.040]
    let train = SpikeTrain(
        name: "tonic_sample_cv_unit",
        timestampsSec: cumulativeTimestamps(fromISI: isi)
    )

    let result = StatePatternDetector.detect(train: train)
    let tonic = try #require(result.candidates.first { $0.finalLabel == .tonic })

    let coveredISI = (tonic.startISIIndex...tonic.endISIIndex).compactMap { train.isiSec[$0] }
    let sampleCV = try #require(STPDStatistics.coefficientOfVariation(coveredISI))
    let populationCV = try #require(populationCoefficientOfVariation(coveredISI))

    #expect(isClose(tonic.cv, sampleCV))
    #expect(sampleCV > populationCV)
    #expect(!isClose(tonic.cv, populationCV, tolerance: 1e-6))
    #expect((tonic.cv ?? 1) <= 0.30)
}

@Test
func highFrequencyTonicCandidateCoefficientOfVariationUsesSharedSampleStandard() throws {
    let isi: [Double] = [0.015, 0.016, 0.014, 0.016, 0.015, 0.014, 0.016, 0.015]
    let train = SpikeTrain(
        name: "hf_tonic_sample_cv_unit",
        timestampsSec: cumulativeTimestamps(fromISI: isi)
    )

    let result = StatePatternDetector.detect(
        train: train,
        settings: StatePatternDetectorSettings(
            highFrequencyTonicFloorSec: 0.010,
            highFrequencyTonicUpperSec: 0.030
        )
    )
    let highFrequencyTonic = try #require(result.candidates.first { $0.finalLabel == .highFrequencyTonic })

    let coveredISI = (highFrequencyTonic.startISIIndex...highFrequencyTonic.endISIIndex).compactMap { train.isiSec[$0] }
    let sampleCV = try #require(STPDStatistics.coefficientOfVariation(coveredISI))
    let populationCV = try #require(populationCoefficientOfVariation(coveredISI))

    #expect(isClose(highFrequencyTonic.cv, sampleCV))
    #expect(sampleCV > populationCV)
    #expect(!isClose(highFrequencyTonic.cv, populationCV, tolerance: 1e-6))
    #expect((highFrequencyTonic.cv ?? 1) <= 0.30)
}

@Test
func statePatternDetectorDetectsHighFrequencySpikingRun() throws {
    let train = SpikeTrain(
        name: "hfs_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )

    let result = StatePatternDetector.detect(train: train)
    let hfs = try #require(result.candidates.first { $0.finalLabel == .highFrequencySpiking })

    #expect(hfs.candidateLayer == "event_grammar_hf_spiking_state")
    #expect(hfs.startISIIndex == 1)
    #expect(hfs.endISIIndex == 32)
    #expect(hfs.startSpikeIndex == 1)
    #expect(hfs.endSpikeIndex == 33)
    #expect(hfs.nSpikes == 33)
    #expect(hfs.nISI == 32)
    #expect((hfs.intraQ90Sec ?? 1) <= 0.025)
}

@Test
func highFrequencySpikingMergesSupportRunsAcrossModerateGapLikeREventGrammar() throws {
    let isi = Array(repeating: 0.010, count: 16) + [0.050] + Array(repeating: 0.010, count: 16)
    let train = SpikeTrain(
        name: "hfs_moderate_gap_unit",
        timestampsSec: cumulativeTimestamps(fromISI: isi)
    )

    let result = StatePatternDetector.detect(train: train)
    let hfs = try #require(result.candidates.first { $0.finalLabel == .highFrequencySpiking })

    #expect(hfs.startISIIndex == 1)
    #expect(hfs.endISIIndex == 33)
    #expect(hfs.nISI == 33)
    #expect(hfs.nSpikes == 34)
    #expect(isClose(hfs.hfSpikingToleratedGapSec, 0.075))
    #expect(isClose(hfs.hfSpikingShortFraction, 32.0 / 33.0))
}

@Test
func highFrequencySpikingKeepsClassicBoundaryCandidatesForRStyleHFProtection() throws {
    let isi = [0.200] + Array(repeating: 0.010, count: 32) + [0.200]
    let train = SpikeTrain(
        name: "classic_boundary_hfs_veto_unit",
        timestampsSec: cumulativeTimestamps(fromISI: isi)
    )

    let result = StatePatternDetector.detect(train: train)
    let hfs = try #require(result.candidates.first { $0.finalLabel == .highFrequencySpiking })

    #expect(hfs.candidateLayer == "event_grammar_hf_spiking_state")
    #expect(hfs.startISIIndex == 2)
    #expect(hfs.endISIIndex == 33)
}

@Test
func classicAnchorPipelineIncludesStateCandidatesAndAutoSelection() throws {
    let tonicTrain = SpikeTrain(
        name: "pipeline_tonic_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 8)
    )
    let hfsTrain = SpikeTrain(
        name: "pipeline_hfs_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )
    let dataset = SpikeDataset(
        name: "synthetic state pipeline",
        sourceDescription: "unit-test",
        trains: [tonicTrain, hfsTrain]
    )

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    #expect(run.tonicCount >= 1)
    #expect(run.highFrequencySpikingCount >= 1)
    #expect(run.selectedAutoCount >= 2)
    #expect(run.candidates.contains { $0.finalLabel == .tonic })
    #expect(run.candidates.contains { $0.finalLabel == .highFrequencySpiking })
    #expect(run.eventAnnotations(in: dataset).allSatisfy { annotation in
        run.candidates.first { $0.id == annotation.candidateID }?.selectedForAuto == true
    })
}

@Test
func classicAnchorPipelineAppliesStateTuningOverrides() throws {
    let hfsTrain = SpikeTrain(
        name: "tuned_hfs_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )
    let dataset = SpikeDataset(
        name: "synthetic tuned state pipeline",
        sourceDescription: "unit-test",
        trains: [hfsTrain]
    )

    let defaultRun = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let tunedRun = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
        stateTuning: StatePatternDetectorTuning(highFrequencySpikingMinSpikes: 40)
    )

    #expect(defaultRun.highFrequencySpikingCount >= 1)
    #expect(tunedRun.highFrequencySpikingCount == 0)
}

@Test
func arbitratorBlocksLowerPriorityOverlappingCandidate() throws {
    let burst = testCandidate(
        id: "burst",
        label: .burst,
        startISIIndex: 2,
        endISIIndex: 6,
        priority: 1_375
    )
    let hfs = testCandidate(
        id: "hfs",
        label: .highFrequencySpiking,
        startISIIndex: 3,
        endISIIndex: 7,
        priority: 1_220
    )

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrate([hfs, burst])
    let arbitratedBurst = try #require(arbitrated.first { $0.id == "burst" })
    let arbitratedHFS = try #require(arbitrated.first { $0.id == "hfs" })

    #expect(arbitratedBurst.selectedForAuto)
    #expect(arbitratedBurst.selectionStatus == "selected_by_event_core_weighted_interval_grammar")
    #expect(!arbitratedHFS.selectedForAuto)
    #expect(arbitratedHFS.selectionStatus == "not_selected")
}

@Test
func legacyHFSProtectionSuppressesCompactBurstKernelsInsideLongHFState() throws {
    var hfs = testCandidate(
        id: "hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 32,
        priority: 1_220
    )
    hfs.hfSpikingShortFraction = 0.90
    hfs.hfSpikingBridgeFraction = 0.96
    hfs.hfSpikingQ90MaxSec = 0.025
    let compactBurst = testCandidate(
        id: "compact_burst",
        label: .burst,
        startISIIndex: 10,
        endISIIndex: 12,
        priority: 1_375
    )
    let possibleKernel = testCandidate(
        id: "possible_kernel",
        label: .possibleBurst,
        startISIIndex: 20,
        endISIIndex: 21,
        priority: 520
    )
    let outsideBurst = testCandidate(
        id: "outside_burst",
        label: .burst,
        startISIIndex: 40,
        endISIIndex: 42,
        priority: 1_375
    )

    let protected = HFSpikingProtection.apply(
        to: [hfs, compactBurst, possibleKernel, outsideBurst],
        mode: .legacySingleLabel
    )
    let protectedHFS = try #require(protected.first { $0.id == "hfs" })
    let protectedBurst = try #require(protected.first { $0.id == "compact_burst" })
    let protectedPossible = try #require(protected.first { $0.id == "possible_kernel" })
    let protectedOutside = try #require(protected.first { $0.id == "outside_burst" })
    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)

    #expect(protectedHFS.action == "accept")
    #expect(protectedBurst.action == "suppress_embedded_burst_for_hf_spiking_state")
    #expect(protectedBurst.gateStatus == "hf_protected_suppressed")
    #expect(protectedBurst.decisionPath.contains("compact_burst_kernel_suppressed_inside_long_hf_spiking_state"))
    #expect(protectedBurst.failureReason == "compact_burst_kernel_suppressed_inside_long_hf_spiking_state")
    #expect(protectedBurst.candidateDiagnosticClass == "rejected__compact_burst_kernel_suppressed_inside_long_hf_spiking_state")
    #expect(protectedBurst.suppressedByHFSpikingState == true)
    #expect(protectedBurst.suppressedOriginalLabel == "burst")
    #expect(protectedBurst.hfSpikingSuppressorID == "hfs")
    #expect(protectedBurst.isEligibleForAutoSelection == false)
    #expect(protectedPossible.action == "accept")
    #expect(protectedPossible.isEligibleForAutoSelection)
    #expect(protectedOutside.action == "accept")
    // Updated expectation (HFS internal-packet retention fix): the long HFS suppressed its
    // internal compact burst and is not burst-dominated, so it is now retained rather than
    // erased by the overlapping internal packet. The outside burst is still selected.
    #expect(arbitrated.first { $0.id == "hfs" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "compact_burst" }?.selectedForAuto == false)
    #expect(arbitrated.first { $0.id == "possible_kernel" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "outside_burst" }?.selectedForAuto == true)
}

@Test
func hfsProtectionRejectsBurstDominatedHFStateWhileKeepingBurstEvents() throws {
    let hfs = testCandidate(
        id: "burst_dominated_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_220
    )
    let firstBurst = testCandidate(
        id: "first_burst",
        label: .burst,
        startISIIndex: 3,
        endISIIndex: 5,
        priority: 1_375
    )
    let secondBurst = testCandidate(
        id: "second_burst",
        label: .burst,
        startISIIndex: 10,
        endISIIndex: 12,
        priority: 1_375
    )
    let thirdBurst = testCandidate(
        id: "third_burst",
        label: .burst,
        startISIIndex: 17,
        endISIIndex: 19,
        priority: 1_375
    )
    let fourthBurst = testCandidate(
        id: "fourth_burst",
        label: .burst,
        startISIIndex: 24,
        endISIIndex: 26,
        priority: 1_375
    )
    let fifthBurst = testCandidate(
        id: "fifth_burst",
        label: .burst,
        startISIIndex: 31,
        endISIIndex: 33,
        priority: 1_375
    )
    let sixthBurst = testCandidate(
        id: "sixth_burst",
        label: .burst,
        startISIIndex: 38,
        endISIIndex: 40,
        priority: 1_375
    )

    let selectedEvents = [
        firstBurst,
        secondBurst,
        thirdBurst,
        fourthBurst,
        fifthBurst,
        sixthBurst
    ].map {
        $0.withAutoSelection(
            selectedForAuto: true,
            selectionStatus: "selected_by_event_track_weighted_interval_grammar"
        )
    }
    let protected = HFSpikingProtection.apply(
        to: [hfs] + selectedEvents,
        selectedEvents: selectedEvents,
        mode: .multiTrack
    )
    let protectedHFS = try #require(protected.first { $0.id == "burst_dominated_hfs" })
    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)

    #expect(protectedHFS.action == "reject_burst_dominated_hf_spiking_state")
    #expect(protectedHFS.gateStatus == "hf_protected_reject")
    #expect(protectedHFS.decisionPath.contains("reject_burst_dominated_hf_spiking_state"))
    #expect(protectedHFS.failureReason == "reject_burst_dominated_hf_spiking_state")
    #expect(protectedHFS.candidateDiagnosticClass == "rejected__reject_burst_dominated_hf_spiking_state")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 6)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 6)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.30))
    #expect(protectedHFS.hfSpikingBurstDominated == true)
    #expect(protectedHFS.hfSpikingBurstPacketLike == false)
    #expect(protected.filter { $0.finalLabel == .burst }.allSatisfy { $0.action == "accept" })
    #expect(arbitrated.first { $0.id == "burst_dominated_hfs" }?.selectedForAuto == false)
    #expect(arbitrated.first { $0.id == "first_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "second_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "third_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "fourth_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "fifth_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "sixth_burst" }?.selectedForAuto == true)
}

@Test
func hfsProtectionCountsAdjacentEmbeddedBurstsAsSeparateRGroups() throws {
    let hfs = testCandidate(
        id: "adjacent_burst_dominated_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_220
    )
    let bursts = [
        testCandidate(id: "adjacent_burst_1", label: .burst, startISIIndex: 1, endISIIndex: 3, priority: 1_375),
        testCandidate(id: "adjacent_burst_2", label: .burst, startISIIndex: 4, endISIIndex: 6, priority: 1_375),
        testCandidate(id: "adjacent_burst_3", label: .burst, startISIIndex: 7, endISIIndex: 9, priority: 1_375),
        testCandidate(id: "adjacent_burst_4", label: .burst, startISIIndex: 10, endISIIndex: 12, priority: 1_375),
        testCandidate(id: "adjacent_burst_5", label: .burst, startISIIndex: 13, endISIIndex: 15, priority: 1_375),
        testCandidate(id: "adjacent_burst_6", label: .burst, startISIIndex: 16, endISIIndex: 18, priority: 1_375)
    ].map {
        $0.withAutoSelection(
            selectedForAuto: true,
            selectionStatus: "selected_by_event_track_weighted_interval_grammar"
        )
    }

    let protected = HFSpikingProtection.apply(
        to: [hfs] + bursts,
        selectedEvents: bursts,
        mode: .multiTrack
    )
    let protectedHFS = try #require(protected.first { $0.id == "adjacent_burst_dominated_hfs" })

    #expect(protectedHFS.action == "accept")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 6)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 1)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.30))
    #expect(protectedHFS.hfSpikingBurstDominated == false)
}

@Test
func legacyHFSProtectionRejectsBurstPacketLikeVariableHFState() throws {
    var hfs = testCandidate(
        id: "packet_like_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 30,
        priority: 1_220
    )
    hfs.hfSpikingLargeFraction = 0.10
    let embeddedPacket = testCandidate(
        id: "embedded_packet",
        label: .longBurst,
        startISIIndex: 5,
        endISIIndex: 10,
        priority: 1_160
    )

    let protected = HFSpikingProtection.apply(
        to: [hfs, embeddedPacket],
        mode: .legacySingleLabel
    )
    let protectedHFS = try #require(protected.first { $0.id == "packet_like_hfs" })
    let protectedBurst = try #require(protected.first { $0.id == "embedded_packet" })

    #expect(protectedHFS.action == "reject_burst_packet_like_hf_spiking_state")
    #expect(protectedHFS.gateStatus == "hf_protected_reject")
    #expect(protectedHFS.failureReason == "reject_burst_packet_like_hf_spiking_state")
    #expect(protectedHFS.candidateDiagnosticClass == "rejected__reject_burst_packet_like_hf_spiking_state")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 1)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 1)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.20))
    #expect(protectedHFS.hfSpikingBurstDominated == false)
    #expect(protectedHFS.hfSpikingBurstPacketLike == true)
    #expect(protectedBurst.action == "accept")
}

@Test
func hfsProtectionMultiTrackRetainsPacketLikeHFStateAsBurstOverlay() throws {
    var hfs = testCandidate(
        id: "packet_like_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 30,
        priority: 1_220
    )
    hfs.hfSpikingLargeFraction = 0.10
    let embeddedPacket = testCandidate(
        id: "embedded_packet",
        label: .longBurst,
        startISIIndex: 5,
        endISIIndex: 10,
        priority: 1_160
    )

    let selectedEmbeddedPacket = embeddedPacket.withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_event_track_weighted_interval_grammar"
    )
    let protected = HFSpikingProtection.apply(
        to: [hfs, embeddedPacket],
        selectedEvents: [selectedEmbeddedPacket],
        mode: .multiTrack
    )
    let protectedHFS = try #require(protected.first { $0.id == "packet_like_hfs" })
    let protectedBurst = try #require(protected.first { $0.id == "embedded_packet" })
    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)

    #expect(protectedHFS.action == "accept")
    #expect(protectedHFS.gateStatus == "pass")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 1)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 1)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.20))
    #expect(protectedHFS.hfSpikingBurstDominated == false)
    #expect(protectedHFS.hfSpikingBurstPacketLike == true)
    #expect(protectedBurst.finalLabel == .longBurst)
    #expect(protectedBurst.action == "accept")
    #expect(protectedBurst.suppressedOriginalLabel == nil)
    #expect(protectedBurst.suppressedByHFSpikingState != true)
    // Updated expectation (HFS internal-packet retention fix): a packet-like but NOT
    // burst-dominated HFS is now retained as a packetization overlay at final arbitration,
    // selected alongside the embedded burst event rather than erased by the overlap.
    #expect(arbitrated.first { $0.id == "packet_like_hfs" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "embedded_packet" }?.selectedForAuto == true)
    #expect(
        arbitrated.first { $0.id == "packet_like_hfs" }?.selectionStatus ==
            "selected_by_state_track_weighted_interval_grammar__hfs_retained_with_internal_burst_packet_overlay"
    )
}

@Test
func hfsProtectionAuditColumnsAreAvailableInFullCSVExport() throws {
    var hfs = testCandidate(
        id: "packet_like_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 30,
        priority: 1_220
    )
    hfs.hfSpikingLargeFraction = 0.10
    let embeddedPacket = testCandidate(
        id: "embedded_packet",
        label: .longBurst,
        startISIIndex: 5,
        endISIIndex: 10,
        priority: 1_160
    )
    let selectedEmbeddedPacket = embeddedPacket.withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_event_track_weighted_interval_grammar"
    )
    let protected = HFSpikingProtection.apply(
        to: [hfs, embeddedPacket],
        selectedEvents: [selectedEmbeddedPacket],
        mode: .multiTrack
    )
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )
    let dataset = SpikeDataset(
        name: "synthetic hfs audit export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: ClassicAnchorCandidateArbitrator.arbitrate(protected)
            )
        ]
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(
            in: dataset,
            selectedOnly: false,
            tracks: Set(ClassicAnchorSemanticTrack.allCases)
        ),
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )

    #expect(csv.contains("hf_spiking_embedded_burst_count"))
    #expect(csv.contains("hf_spiking_burst_packet_like"))
    #expect(csv.contains("hfs_packet_like_but_not_dominated__retain_state_for_mutual_exclusion_arbitration"))
    #expect(csv.contains(",1,1,0.2,false,true,false,"))
}

@Test
func selectedGapDoesNotSplitHigherPriorityTonicState() throws {
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.020, count: 14)
    )
    let tonic = testCandidate(
        id: "tonic_parent",
        label: .tonic,
        startISIIndex: 1,
        endISIIndex: 10,
        priority: 420
    )
    let selectedGap = testCandidate(
        id: "selected_pause_gap",
        label: .pause,
        startISIIndex: 5,
        endISIIndex: 5,
        priority: 910
    )
    .withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_gap_track_monotonic_completion"
    )

    let fragments = StateEventCompatibilityResolver.splitStateCandidates(
        train: train,
        candidates: [tonic, selectedGap],
        selectedEvents: [],
        selectedGaps: [selectedGap]
    )

    #expect(fragments.isEmpty)
}

@Test
func sustainedDenseHFSRunWithInternalBurstPacketsIsGeneratedAndSelected() throws {
    // Case 0/1: a long, sustained, dense high-frequency run with a few internal compact
    // burst-like packets and NO pause/gap boundary. The run must generate an HFS candidate
    // (generation is not the failure) AND remain selected as a single sustained HFS state
    // through event/state arbitration — the internal packets are overlays, not a reason to
    // fragment the state into sub-threshold pieces or leave the region unmarked.
    var isi: [Double] = []
    for block in 0..<4 {
        isi.append(contentsOf: Array(repeating: 0.009, count: 25))
        if block < 3 {
            // Internal compact burst packet: short ISIs flanked by the denser base.
            isi.append(contentsOf: [0.002, 0.002, 0.002])
        }
    }
    let train = SpikeTrain(
        name: "sustained_dense_hfs_with_packets",
        timestampsSec: cumulativeTimestamps(fromISI: isi)
    )

    // Generation: the sustained dense run is recognized as HFS.
    let stateOnly = StatePatternDetector.detect(
        train: train,
        settings: StatePatternDetectorSettings(
            highFrequencySpikingMinSpikes: 30,
            highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
        )
    ).candidates
    let generatedHFS = try #require(stateOnly.first { $0.finalLabel == .highFrequencySpiking })
    #expect(generatedHFS.hfSpikingBurstDominated != true)

    // Selection: through the full pipeline the sustained HFS survives event/state
    // arbitration and is selected (not lost to internal burst packetization).
    let dataset = SpikeDataset(
        name: "sustained dense hfs",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let selectedHFS = run.candidates.filter {
        $0.finalLabel == .highFrequencySpiking && $0.selectedForAuto
    }
    #expect(!selectedHFS.isEmpty)
    // The sustained state is retained as one large epoch, not fragmented to sub-threshold bits.
    #expect(selectedHFS.contains { ($0.endISIIndex - $0.startISIIndex) >= 40 })
}

@Test
func stateSplitDoesNotCutHFSAtSelectedBurstWithoutGap() throws {
    // Updated expectation (HFS internal-packet retention fix): a selected burst alone no
    // longer fragments a sustained HFS. Without a pause/gap boundary there is nothing to
    // split on, so the whole HFS is retained (no fragments) and the internal burst packet
    // becomes an overlay; burst dominance is decided downstream, not by cutting here.
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.005, count: 70)
    )
    let settings = StatePatternDetectorSettings(
        highFrequencySpikingMinSpikes: 8,
        highFrequencySpikingMinDurationSec: 0
    )
    let hfs = testCandidate(
        id: "hfs_parent",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_220
    )
    let selectedBurst = testCandidate(
        id: "selected_compact_burst",
        label: .burst,
        startISIIndex: 10,
        endISIIndex: 12,
        priority: 1_375
    )
    .withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_event_track_weighted_interval_grammar"
    )

    let fragments = StateEventCompatibilityResolver.splitStateCandidates(
        train: train,
        candidates: [hfs, selectedBurst],
        selectedEvents: [selectedBurst],
        selectedGaps: [],
        settings: settings
    )

    #expect(fragments.isEmpty)
}

@Test
func arbitratorUsesWeightedIntervalSelectionInsteadOfGreedyPriority() throws {
    let broad = testCandidate(
        id: "broad_hf_tonic",
        label: .highFrequencyTonic,
        startISIIndex: 1,
        endISIIndex: 6,
        priority: 560
    )
    let left = testCandidate(
        id: "left_tonic",
        label: .tonic,
        startISIIndex: 1,
        endISIIndex: 3,
        priority: 420
    )
    let right = testCandidate(
        id: "right_tonic",
        label: .tonic,
        startISIIndex: 4,
        endISIIndex: 6,
        priority: 420
    )

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([broad, left, right])
    let arbitratedBroad = try #require(arbitrated.first { $0.id == "broad_hf_tonic" })
    let arbitratedLeft = try #require(arbitrated.first { $0.id == "left_tonic" })
    let arbitratedRight = try #require(arbitrated.first { $0.id == "right_tonic" })

    #expect(!arbitratedBroad.selectedForAuto)
    #expect(arbitratedLeft.selectedForAuto)
    #expect(arbitratedRight.selectedForAuto)
    #expect(arbitratedLeft.selectionStatus == "selected_by_state_track_weighted_interval_grammar")
    #expect(arbitratedRight.selectionStatus == "selected_by_state_track_weighted_interval_grammar")
}

@Test
func strongHighFrequencyStateDominatesEmbeddedClassicTonicInArbitration() throws {
    // Regression for the diagnosed user example (LT1D5.083_SPK 01c): a long, strong,
    // non-burst-dominated high-frequency state (genuinely fast — q90 ~10 ms, shortFraction
    // ~0.99, bridgeFraction 1.0) spanning the whole synthetic envelope, with three small
    // embedded classic-tonic windows fully contained inside it. Without the containment
    // demotion the weighted-interval grammar selects the three small tonics (3 * priority
    // 1100 out-values one HFS pinned at the hardcoded 1000) and rejects the long HF state.
    // After the fix the embedded classic tonic is demoted so the strong HF state wins.
    var hfs = testCandidate(
        id: "hf_envelope",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_000,
        intraQ90Sec: 0.010
    )
    hfs.hfSpikingShortFraction = 0.99
    hfs.hfSpikingBridgeFraction = 1.0
    hfs.hfSpikingBurstDominated = false
    hfs.stateHighFrequencySubtype = "hf_irregular_spiking"

    func embeddedTonic(_ id: String, _ start: Int, _ end: Int) -> ClassicAnchorCandidate {
        var tonic = testCandidate(
            id: id,
            label: .tonic,
            startISIIndex: start,
            endISIIndex: end,
            priority: 1_100
        )
        tonic.stateTonicSubtype = "classic"
        return tonic
    }
    let tonicA = embeddedTonic("classic_tonic_a", 8, 16)
    let tonicB = embeddedTonic("classic_tonic_b", 26, 34)
    let tonicC = embeddedTonic("classic_tonic_c", 44, 52)

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
        [hfs, tonicA, tonicB, tonicC]
    )
    let byID = Dictionary(uniqueKeysWithValues: arbitrated.map { ($0.id, $0) })

    // The strong HF state spans the synthetic envelope and is selected with its HF subtype.
    let selectedHF = try #require(byID["hf_envelope"])
    #expect(selectedHF.selectedForAuto)
    #expect(selectedHF.finalLabel == .highFrequencySpiking)
    #expect(selectedHF.stateHighFrequencySubtype == "hf_irregular_spiking")

    // No selected classic-tonic candidate is fully contained inside the selected HF state;
    // each embedded tonic carries the explicit nonselection reason.
    for id in ["classic_tonic_a", "classic_tonic_b", "classic_tonic_c"] {
        let tonic = try #require(byID[id])
        #expect(!tonic.selectedForAuto)
        #expect(tonic.selectionStatus == "not_selected__contained_in_strong_hf_state")
    }
    let selectedContainedTonics = arbitrated.filter {
        $0.selectedForAuto && $0.finalLabel == .tonic
    }
    #expect(selectedContainedTonics.isEmpty)
}

@Test
func burstPacketInsideStrongHighFrequencyEnvelopeStaysBurstButCarriesHFSubtype() throws {
    // A strong HF state with one short burst-family event fully inside it. The burst stays a
    // burst event (finalLabel unchanged, still selected on the event track) and the sustained
    // HF state is retained as a packetization overlay rather than erased; the burst additionally
    // carries the audit-only hf_burst_packet subtype.
    var hfs = testCandidate(
        id: "hf_envelope",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_000,
        intraQ90Sec: 0.010
    )
    hfs.hfSpikingShortFraction = 0.99
    hfs.hfSpikingBridgeFraction = 1.0
    hfs.hfSpikingBurstDominated = false
    hfs.stateHighFrequencySubtype = "hf_irregular_spiking"

    let burst = testCandidate(
        id: "internal_burst_packet",
        label: .burst,
        startISIIndex: 20,
        endISIIndex: 24,
        priority: 1_250
    )

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([hfs, burst])
    let byID = Dictionary(uniqueKeysWithValues: arbitrated.map { ($0.id, $0) })

    let resolvedBurst = try #require(byID["internal_burst_packet"])
    #expect(resolvedBurst.finalLabel == .burst)               // label unchanged
    #expect(resolvedBurst.selectedForAuto)                    // still a selected burst event
    #expect(resolvedBurst.stateHighFrequencySubtype == "hf_burst_packet")

    let resolvedHF = try #require(byID["hf_envelope"])
    #expect(resolvedHF.selectedForAuto)                       // sustained HF retained, not erased
    #expect(resolvedHF.finalLabel == .highFrequencySpiking)
    #expect(resolvedHF.stateHighFrequencySubtype == "hf_irregular_spiking")
}

@Test
func slowHighFrequencySpikingDoesNotDemoteEmbeddedTonic() throws {
    // Guardrail for the absolute q90 cap: a HFS-shaped candidate whose q90 exceeds the strong-HF
    // cap (35 ms > 30 ms) is NOT treated as a dominating HF state, so an embedded classic-tonic
    // window is arbitrated normally (it is never demoted by containment). This protects the
    // irregular-tonic / slow-train behavior from the new dominance rule.
    var hfs = testCandidate(
        id: "slow_hf",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_000,
        intraQ90Sec: 0.035
    )
    hfs.hfSpikingShortFraction = 0.99
    hfs.hfSpikingBridgeFraction = 1.0
    hfs.hfSpikingBurstDominated = false

    var tonic = testCandidate(
        id: "embedded_tonic",
        label: .tonic,
        startISIIndex: 20,
        endISIIndex: 40,
        priority: 1_100
    )
    tonic.stateTonicSubtype = "classic"

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([hfs, tonic])
    let resolvedTonic = try #require(arbitrated.first { $0.id == "embedded_tonic" })

    // The slow HFS is not a strong HF state, so the tonic is never demoted by containment and
    // wins normal weighted-interval arbitration on its own value.
    #expect(resolvedTonic.selectionStatus != "not_selected__contained_in_strong_hf_state")
    #expect(resolvedTonic.selectedForAuto)
}

@Test
func eventAnnotationsDefaultToAutoSelectedEventCandidates() throws {
    let broad = testCandidate(
        id: "broad_hf_tonic",
        label: .highFrequencyTonic,
        startISIIndex: 1,
        endISIIndex: 6,
        priority: 560
    )
    let left = testCandidate(
        id: "left_tonic",
        label: .tonic,
        startISIIndex: 1,
        endISIIndex: 3,
        priority: 420
    )
    let right = testCandidate(
        id: "right_tonic",
        label: .tonic,
        startISIIndex: 4,
        endISIIndex: 6,
        priority: 420
    )
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 8)
    )
    let dataset = SpikeDataset(
        name: "synthetic arbitration",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let candidates = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([broad, left, right])
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: candidates
            )
        ]
    )

    let selectedAnnotations = run.eventAnnotations(in: dataset)
    let selectedStateAnnotations = run.eventAnnotations(in: dataset, tracks: [.state])
    let auditAnnotations = run.eventAnnotations(
        in: dataset,
        selectedOnly: false,
        tracks: Set(ClassicAnchorSemanticTrack.allCases)
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let selectedCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: selectedAnnotations,
        reviewStatuses: [:],
        exportedAt: exportedAt
    )
    let auditCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: auditAnnotations,
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )

    #expect(selectedAnnotations.isEmpty)
    #expect(selectedStateAnnotations.map(\.candidateID).sorted() == ["left_tonic", "right_tonic"])
    #expect(auditAnnotations.map(\.candidateID).sorted() == ["broad_hf_tonic", "left_tonic", "right_tonic"])
    #expect(!selectedCSV.contains("broad_hf_tonic"))
    #expect(!selectedCSV.contains("left_tonic"))
    #expect(!selectedCSV.contains("right_tonic"))
    #expect(auditCSV.contains("broad_hf_tonic"))
}

@Test
func eventAnnotationTimeBoundsFollowISISpanEvenWhenSpikeSpanDiffers() throws {
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    )
    let dataset = SpikeDataset(
        name: "synthetic annotation",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let candidate = testCandidate(
        id: "state_with_wide_spike_span",
        label: .tonic,
        startISIIndex: 3,
        endISIIndex: 4,
        startSpikeIndex: 1,
        endSpikeIndex: 6,
        priority: 420
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: [candidate]
            )
        ]
    )

    let annotation = try #require(
        run.eventAnnotations(
            in: dataset,
            selectedOnly: false,
            tracks: Set(ClassicAnchorSemanticTrack.allCases)
        ).first
    )
    let coveredISI = try #require(annotation.coveredISIIndices(in: train))

    #expect(Array(coveredISI) == [3, 4])
    #expect(annotation.rawStartSec == train.timestampsSec[2])
    #expect(annotation.rawEndSec == train.timestampsSec[4])
    #expect(annotation.alignedStartSec == train.alignedTimestampsSec[2])
    #expect(annotation.alignedEndSec == train.alignedTimestampsSec[4])
}

private func irregularTonicISIs() -> [Double] {
    // 96 sustained, moderately-variable ISIs (seconds) resembling the GPi example: a fixed
    // deterministic i.i.d. lognormal-like draw with whole-train CV ~0.43, CV2 ~0.53, median
    // ~25 ms, max 67 ms (< the obvious pause scale), and NO sub-burst-seed short ISIs. No long
    // silence, no burst/fast-packet dominance — so it is tonic-family but not classic-regular.
    return [
        0.015, 0.0231, 0.0121, 0.0272, 0.0145, 0.0218, 0.014, 0.0264, 0.0306, 0.0278,
        0.012, 0.0344, 0.0428, 0.0327, 0.0171, 0.032, 0.0341, 0.0204, 0.0258, 0.0196,
        0.038, 0.0194, 0.0179, 0.0195, 0.0207, 0.014, 0.0378, 0.0308, 0.0154, 0.03,
        0.0536, 0.0161, 0.036, 0.0133, 0.0205, 0.067, 0.0357, 0.0154, 0.0511, 0.0248,
        0.0175, 0.034, 0.0194, 0.0236, 0.0425, 0.0487, 0.025, 0.0188, 0.0237, 0.0133,
        0.0179, 0.0242, 0.0392, 0.0138, 0.0308, 0.0131, 0.017, 0.0354, 0.012, 0.0198,
        0.0371, 0.0217, 0.031, 0.0237, 0.0192, 0.0368, 0.0191, 0.049, 0.019, 0.0246,
        0.0499, 0.0199, 0.0367, 0.0343, 0.0422, 0.03, 0.0532, 0.0436, 0.024, 0.012,
        0.0221, 0.0335, 0.0193, 0.04, 0.0184, 0.0145, 0.0208, 0.0218, 0.0501, 0.017,
        0.0358, 0.0259, 0.012, 0.036, 0.0251, 0.0362
    ]
}

private func mergeTestTrain(gapISISec: Double) -> SpikeTrain {
    // Two tight irregular-tonic segments (28 ISIs each, median ~34 ms, max/mean > 1.25 so the
    // classic regularity gate fails → irregular subtype, while the bridge fraction stays low so
    // the merged span re-passes). Joined by one intervening gap ISI at index 29.
    let segAMs: [Double] = [
        28.7, 44.5, 38.7, 46.6, 30.6, 34.4, 22.0, 32.1, 22.7, 41.9, 31.7, 55.0, 47.1, 33.5,
        33.9, 22.0, 34.6, 22.0, 28.3, 55.0, 25.0, 30.9, 55.0, 50.9, 23.1, 33.1, 23.2, 39.5
    ]
    let segBMs: [Double] = [
        22.0, 28.5, 22.0, 22.0, 40.5, 33.9, 39.3, 43.3, 42.2, 42.2, 26.0, 55.0, 28.8, 52.8,
        55.0, 42.9, 22.0, 24.1, 33.2, 55.0, 37.6, 33.6, 22.2, 29.5, 30.5, 22.0, 22.8, 46.5
    ]
    let isi = segAMs.map { $0 / 1000 } + [gapISISec] + segBMs.map { $0 / 1000 }
    var ts = [0.0]
    for value in isi { ts.append((ts.last ?? 0) + value) }
    // Train id == name, matching testCandidate's trainID so constructed fragments belong here.
    return SpikeTrain(name: "arbitration_train", timestampsSec: ts)
}

private func mergeTestSettings() -> StatePatternDetectorSettings {
    StatePatternDetectorSettings(
        burstSeedUpperSec: 0.010,
        tonicBridgeUpperSec: 0.075,
        highFrequencySpikingShortUpperSec: 0.020
    )
}

private func irregularTonicPipelineISIs() -> [Double] {
    // 120 deterministic median-35 ms lognormal ISIs (seconds). Slow/variable enough that the
    // adaptive pipeline keeps it tonic-family (not HFS/HF-tonic) and selects irregular-tonic.
    let isiMs: [Double] = [
        47.9, 65.3, 26.3, 29.0, 43.6, 39.7, 59.2, 18.0, 73.5, 40.1, 42.3, 24.4, 32.0, 34.7, 57.1,
        25.9, 40.6, 60.0, 26.1, 32.5, 38.9, 35.6, 39.3, 30.1, 75.0, 20.6, 38.3, 27.2, 50.9, 39.1,
        68.7, 64.9, 22.1, 29.6, 50.0, 34.3, 25.3, 52.1, 34.4, 18.9, 60.0, 21.1, 30.0, 33.2, 75.0,
        25.6, 30.2, 25.5, 66.8, 18.2, 26.2, 55.2, 74.5, 48.3, 34.7, 37.8, 47.6, 20.6, 33.2, 26.4,
        75.0, 35.4, 43.5, 29.3, 26.3, 51.4, 71.4, 43.5, 18.5, 34.8, 51.4, 35.5, 47.7, 18.0, 29.7,
        32.3, 56.2, 25.8, 18.7, 20.4, 39.3, 49.0, 56.1, 30.3, 27.0, 54.1, 18.0, 25.5, 35.5, 53.0,
        46.6, 34.1, 49.4, 39.8, 26.3, 40.8, 44.6, 32.6, 30.7, 34.8, 18.0, 43.5, 48.6, 43.4, 37.6,
        51.0, 37.0, 42.4, 24.3, 48.7, 38.8, 37.9, 31.2, 52.5, 74.0, 43.6, 46.6, 75.0, 18.0, 26.2
    ]
    return isiMs.map { $0 / 1000 }
}

// MARK: - MM removal regression tests

@Test
func noStatePatternDecisionPathContainsMMSignalAfterRemoval() throws {
    // After MM is removed from the state-pattern algorithm, no tonic-family candidate's decision
    // path should carry an MM pass/reject/threshold token (mm=, mm_max, mm_outlier, mm_outside).
    let train = SpikeTrain(
        name: "mm_removal_decision_path",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 14)
    )
    let result = StatePatternDetector.detect(train: train)
    #expect(result.candidates.contains { $0.finalLabel == .tonic })
    for candidate in result.candidates {
        #expect(!candidate.decisionPath.contains("mm"))
    }
}

@Test
func regularTonicRemainsClassicGovernedByCVCV2LVAfterMMRemoval() throws {
    // A clean uniform tonic run is still classic tonic, now governed by CV/CV2/LV (not MM).
    let train = SpikeTrain(
        name: "mm_removal_classic_tonic",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 14)
    )
    let result = StatePatternDetector.detect(train: train)
    let tonic = try #require(result.candidates.first { $0.finalLabel == .tonic })
    #expect(tonic.stateTonicSubtype == "classic")
    #expect((tonic.cv ?? 1) <= 0.30)
}

@Test
func tonicWithClassicRegularityButHighMMIsClassicNotIrregularAfterMMRemoval() throws {
    // A run whose CV/CV2/LV are classic-regular but whose max/mean (MM) exceeds the old classic MM
    // ceiling (1.25) was previously DOWNGRADED to "irregular" solely by MM. With MM removed, its
    // subtype is governed by CV/CV2/LV alone, so it is classified "classic".
    let core = Array(repeating: 0.020, count: 20)
    let gentleTail = [0.022, 0.024, 0.026, 0.028, 0.030]   // smooth tail -> low LV/CV2, raises MM
    let train = SpikeTrain(
        name: "mm_removal_classic_high_mm",
        timestampsSec: cumulativeTimestamps(fromISI: core + gentleTail)
    )
    let result = StatePatternDetector.detect(train: train)
    let tonic = try #require(result.candidates.first { $0.finalLabel == .tonic })
    #expect(tonic.stateTonicSubtype == "classic")
    #expect((tonic.cv ?? 1) <= 0.30)
}

@Test
func fastPacketSegmentStillExcludedFromTonicFamilyAfterMMRemoval() throws {
    // Structural guards (burst-seed fraction / burst-core veto / fast-packet occupancy), not MM,
    // keep a burst-like fast-packet run out of the tonic family.
    let train = SpikeTrain(
        name: "mm_removal_fast_packet_excluded",
        timestampsSec: cumulativeTimestamps(fromISI: fastPacketISIs(scale: 1))
    )
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.003,
        highFrequencyTonicFloorSec: 0.003,
        highFrequencyTonicUpperSec: 0.012,
        highFrequencySpikingShortUpperSec: 0.020
    )
    let result = StatePatternDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { $0.finalLabel == .tonic || $0.finalLabel == .highFrequencyTonic })
    // And the rejection is structural, never MM-based.
    for candidate in result.candidates {
        #expect(!candidate.decisionPath.contains("mm"))
    }
}

@Test
func highFrequencyTonicStillProtectedByStructuralVetoesAfterMMRemoval() throws {
    // HF tonic remains protected by the burst-core / fast-packet / low-tail vetoes (not MM): a valid
    // regular HF-tonic run above the fast-packet core is still accepted, with no MM token recorded.
    let train = SpikeTrain(
        name: "mm_removal_hf_tonic_protected",
        timestampsSec: cumulativeTimestamps(repeating: 0.018, count: 8)
    )
    let settings = StatePatternDetectorSettings(
        burstSeedUpperSec: 0.006,
        highFrequencyTonicFloorSec: 0.010,
        highFrequencyTonicUpperSec: 0.030,
        highFrequencySpikingShortUpperSec: 0.020
    )
    let result = StatePatternDetector.detect(train: train, settings: settings)
    let hft = try #require(result.candidates.first { $0.finalLabel == .highFrequencyTonic })
    #expect(!hft.decisionPath.contains("mm"))
}

private func fastPacketISIs(scale: Double) -> [Double] {
    // Stable, regular very-short-ISI packet (ratio-preserving under scaling). Low CV/CV2/LV,
    // but structurally fast (intra-burst / HFS-like) rather than tonic.
    [0.007, 0.008, 0.007, 0.008, 0.009, 0.007, 0.008, 0.007, 0.008, 0.009, 0.007, 0.008]
        .map { $0 * scale }
}

private func relativeHFTonicTrainISIs(scale: Double) -> [Double] {
    // A sustained, near-uniform ~26 ms epoch (20 ISIs) embedded in a ~60 ms slow background. The
    // epoch is clearly faster than the train's own background (q90/background ratio ~0.45) yet too
    // slow for the fixed HFS q80/q90 ceilings and too short for the >=30-spike sustained-run route.
    // The 60/26 ~ 2.3x boundary stays below classicBoundaryContrastMin (3.0) so it reads as a state
    // transition, not a burst boundary. Ratio-preserving under scaling.
    let background = Array(repeating: 0.060, count: 25)
    let epoch = [0.025, 0.027, 0.024, 0.028, 0.026, 0.025, 0.027, 0.024, 0.026, 0.028,
                 0.025, 0.027, 0.024, 0.026, 0.025, 0.028, 0.026, 0.024, 0.027, 0.025]
    return (background + epoch + background).map { $0 * scale }
}

private func relativeHFTonicSettings(scale: Double) -> StatePatternDetectorSettings {
    StatePatternDetectorSettings(
        burstSeedUpperSec: 0.006 * scale,
        highFrequencyTonicFloorSec: 0.010 * scale,
        highFrequencyTonicUpperSec: 0.015 * scale,   // fixed-band HFT deliberately below the ~26 ms epoch
        highFrequencySpikingShortUpperSec: 0.020 * scale
    )
}

private func cumulativeTimestamps(repeating isi: Double, count: Int) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(count + 1)
    for _ in 0..<count {
        values.append((values.last ?? 0) + isi)
    }
    return values
}

private func cumulativeTimestamps(fromISI isiValues: [Double]) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(isiValues.count + 1)
    for isi in isiValues {
        values.append((values.last ?? 0) + isi)
    }
    return values
}

private func testCandidate(
    id: String,
    label: ClassicAnchorLabel,
    startISIIndex: Int,
    endISIIndex: Int,
    startSpikeIndex: Int? = nil,
    endSpikeIndex: Int? = nil,
    priority: Int,
    intraQ90Sec: Double? = nil
) -> ClassicAnchorCandidate {
    ClassicAnchorCandidate(
        id: id,
        trainID: "arbitration_train",
        trainName: "arbitration_train",
        candidateLayer: "unit_test",
        candidateClass: "unit_test",
        finalLabel: label,
        gateStatus: "pass",
        decisionPath: "unit_test",
        action: "accept",
        score: 1,
        priority: priority,
        selectedForAuto: false,
        selectionStatus: "not_selected",
        startISIIndex: startISIIndex,
        endISIIndex: endISIIndex,
        startSpikeIndex: startSpikeIndex ?? startISIIndex,
        endSpikeIndex: endSpikeIndex ?? endISIIndex + 1,
        nISI: endISIIndex - startISIIndex + 1,
        nValidISI: endISIIndex - startISIIndex + 1,
        nSpikes: endISIIndex - startISIIndex + 2,
        durationSec: nil,
        intraQ10Sec: nil,
        intraQ40Sec: nil,
        intraQ50Sec: nil,
        intraQ90Sec: intraQ90Sec,
        intraQ95Sec: nil,
        maxIntraISISec: nil,
        meanIntraISISec: nil,
        cv: nil,
        lv: nil,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: "unit_test",
        anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.010,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
}

private func populationCoefficientOfVariation(_ values: [Double]) -> Double? {
    let finite = values.filter(\.isFinite)
    guard finite.count >= 2 else {
        return nil
    }
    let mean = finite.reduce(0, +) / Double(finite.count)
    guard mean > 0 else {
        return nil
    }
    let variance = finite.reduce(0) { partial, value in
        let delta = value - mean
        return partial + delta * delta
    } / Double(finite.count)
    return variance.squareRoot() / mean
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}
