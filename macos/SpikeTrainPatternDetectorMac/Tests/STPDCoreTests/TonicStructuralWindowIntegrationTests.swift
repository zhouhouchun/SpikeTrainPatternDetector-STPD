import Foundation
import STPDCore
import Testing

// MARK: - TSW-INTEGRATION — the from-sequence expandable sliding window (TonicStructuralWindowDetector)
// wired in as the PRIMARY tonic structural candidate generator inside StatePatternDetector.detectTonic.
// These pin the behavioral goals of the wiring; the burst/pause/BCB-1 regression suite is covered
// separately (BCB1*, TonicBoundaryRescue*, PauseTonicArbitration*).

private func tswiCumulative(_ isis: [Double]) -> [Double] {
    var t = 0.0; var out = [0.0]
    for x in isis { t += x; out.append(t) }
    return out
}
private func tswiTrain(_ name: String, _ isis: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: tswiCumulative(isis))
}
private func tswiAcceptedTonic(_ train: SpikeTrain, primary: Bool = true) -> [ClassicAnchorCandidate] {
    var settings = StatePatternDetectorSettings()
    settings.tonicStructuralWindowPrimary = primary
    return StatePatternDetector.detect(train: train, settings: settings).candidates
        .filter { $0.finalLabel == .tonic && $0.action == "accept" }
        .sorted { $0.startISIIndex < $1.startISIIndex }
}
private func tswiFixture5x5() throws -> SpikeDataset {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                          datasetName: "5x5", sourceDescription: url.path)
}

// 1 — a clean regular tonic run is detected as ONE long tonic candidate spanning the whole run (not
// fragmented into sub-windows), produced by the TSW structural window.
@Test
func tswIntegrationCleanRunIsOneLongCandidate() {
    let isis: [Double] = [0.040, 0.043, 0.037, 0.041, 0.039, 0.042, 0.038, 0.040,
                          0.041, 0.039, 0.043, 0.038, 0.040, 0.042, 0.037, 0.041]   // 16 clean ISIs
    let train = tswiTrain("clean_tonic_long", isis)
    let accepted = tswiAcceptedTonic(train)
    #expect(accepted.count == 1)                                       // NOT fragmented
    let c = accepted.first
    #expect(c?.startISIIndex == 1)
    #expect(c?.endISIIndex == isis.count)                             // spans the whole run
    #expect(c?.stateTonicSubtype == "classic")
    #expect(c?.decisionPath.contains("tonic_structural_window") == true)
    #expect((c?.cv ?? 1) <= 0.30)
}

// 2 — on the 5x5 dataset the pause_response trains' visually-tonic baseline segments are captured as
// selected tonic candidates produced by the TSW structural window.
@Test
func tswIntegrationPauseResponseTonicSegmentsCaptured() throws {
    let dataset = try tswiFixture5x5()
    for n in 1...5 {
        let name = "pause_response_\(n)_s"
        let train = try #require(dataset.trains.first { $0.name == name })
        let run = ClassicAnchorDetectionPipeline.run(
            dataset: SpikeDataset(name: "one", sourceDescription: dataset.sourceDescription, trains: [train]),
            bandSettings: TrainAdaptiveBandSettings()
        )
        let tonics = (run.result(for: train.id)?.candidates ?? [])
            .filter { $0.selectedForAuto && $0.finalLabel == .tonic }
        #expect(!tonics.isEmpty, "\(name): expected selected tonic baseline segments")
        #expect(tonics.contains { $0.decisionPath.contains("tonic_structural_window") },
                "\(name): tonic should be TSW-sourced")
    }
}

// 3 — a tonic run followed by a LARGE pause ISI stops BEFORE the pause: no tonic candidate covers the
// pause ISI, and it remains available for pause detection.
@Test
func tswIntegrationStopsBeforeLargePause() {
    let isis = Array(repeating: 0.050, count: 8) + [1.5] + Array(repeating: 0.050, count: 8)  // pause at ISI 9
    let train = tswiTrain("tonic_pause_tonic", isis)
    let accepted = tswiAcceptedTonic(train)
    #expect(!accepted.isEmpty)
    #expect(!accepted.contains { ($0.startISIIndex...$0.endISIIndex).contains(9) })   // never swallows the pause
    // The first tonic run ends at the last pre-pause ISI (8), not across the 1.5 s gap.
    #expect(accepted.contains { $0.startISIIndex == 1 && $0.endISIIndex == 8 })
}

// 4 — a fast, regular burst-like packet is NOT converted into classic tonic: the burst-contamination /
// magnitude guards keep the sliding window from seeding tonic on sub-refractory-fast ISIs.
@Test
func tswIntegrationFastPacketIsNotClassicTonic() {
    // Uniform 4 ms ISIs — well below the burst seed upper (10 ms) and the physiological classic-tonic floor.
    let fast = tswiTrain("fast_packet", Array(repeating: 0.004, count: 14))
    #expect(tswiAcceptedTonic(fast).isEmpty)

    // Mixed: a fast packet embedded next to a real tonic baseline — the tonic forms, but no tonic candidate
    // classifies the fast packet (ISIs 9...14) as classic tonic.
    let mixed = tswiTrain("tonic_then_fast",
                          Array(repeating: 0.050, count: 8) + Array(repeating: 0.004, count: 6))
    let accepted = tswiAcceptedTonic(mixed)
    #expect(!accepted.isEmpty)
    #expect(!accepted.contains { c in (9...14).contains { c.startISIIndex <= $0 && c.endISIIndex >= $0 } })
}

// 5 — the integration is behind a reversible flag: OFF restores the legacy band-membership path (no
// `tonic_structural_window` provenance); ON uses the sliding window. A/B / rollback safety.
@Test
func tswIntegrationFlagIsReversible() {
    let train = tswiTrain("flag_ab",
                          [0.040, 0.043, 0.037, 0.041, 0.039, 0.042, 0.038, 0.040, 0.041, 0.039, 0.043, 0.038])
    let on = tswiAcceptedTonic(train, primary: true)
    let off = tswiAcceptedTonic(train, primary: false)
    #expect(!on.isEmpty && !off.isEmpty)                                          // both still detect tonic
    #expect(on.contains { $0.decisionPath.contains("tonic_structural_window") })  // ON = sliding window
    #expect(off.allSatisfy { !$0.decisionPath.contains("tonic_structural_window") })  // OFF = legacy band scan
}

// MARK: - TSW-D5B — relative pause-like outlier carve (a pause-scale ISI must not be swallowed as tonic).

// 6 — the reported regression: pause_response_1_s ISI 11 (1.063 s, ~2.3x the ~0.45 s tonic core) was
// swallowed into a broad irregular tonic window [1...15] via the TSW-3 bridge. It must be CARVED OUT:
// no tonic candidate covers ISI 11, the true-tonic segments before (…10) and after (12…) remain tonic,
// and the carve provenance is recorded.
@Test
func tswPauseLikeCarveRemovesPauseResponse1SISI11() throws {
    let dataset = try tswiFixture5x5()
    let train = try #require(dataset.trains.first { $0.name == "pause_response_1_s" })

    // Detector level: the tonic candidate set no longer covers the pause ISI; neighbors stay tonic.
    let accepted = tswiAcceptedTonic(train)
    #expect(!accepted.contains { $0.startISIIndex <= 11 && $0.endISIIndex >= 11 })   // ISI 11 NOT tonic
    #expect(accepted.contains { $0.startISIIndex <= 10 && $0.endISIIndex >= 10 })    // segment before stays tonic
    #expect(accepted.contains { $0.startISIIndex <= 12 && $0.endISIIndex >= 12 })    // segment after stays tonic
    #expect(accepted.contains { $0.decisionPath.contains("tonic_pause_like_outlier") })
    #expect(accepted.contains { $0.decisionPath.contains("carved_from_tsw=[1...15]") })

    // Pipeline level: the final selected label at ISI 11 is not canonical tonic.
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "one", sourceDescription: dataset.sourceDescription, trains: [train]),
        bandSettings: TrainAdaptiveBandSettings()
    )
    let cs = run.result(for: train.id)?.candidates ?? []
    #expect(!cs.contains { $0.selectedForAuto && $0.finalLabel == .tonic
        && $0.startISIIndex <= 11 && $0.endISIIndex >= 11 })
}

// 7 — a moderate (bridge-spannable) internal pause between two tonic runs is carved, not swallowed: the
// pause ISI is excluded (available to pause detection) and both flanking tonic segments survive.
@Test
func tswPauseLikeCarveSplitsInternalBridgePause() {
    let isis = Array(repeating: 0.050, count: 8) + [0.120] + Array(repeating: 0.050, count: 8)  // pause @ ISI 9
    let train = tswiTrain("tonic_pause_tonic_bridge", isis)
    let accepted = tswiAcceptedTonic(train)
    #expect(!accepted.contains { ($0.startISIIndex...$0.endISIIndex).contains(9) })   // pause carved out
    #expect(accepted.contains { $0.startISIIndex == 1 && $0.endISIIndex == 8 })       // left tonic segment
    #expect(accepted.contains { $0.startISIIndex == 10 && $0.endISIIndex == 17 })     // right tonic segment
    #expect(accepted.contains { $0.decisionPath.contains("split_by_pause_like_isi") })
}

// 8 — a genuinely IRREGULAR tonic run whose largest beats sit near its own q90 (a fat unimodal tail, NOT
// a bimodal pause gap) is NOT carved: it stays a single sustained tonic candidate. Guards against the
// carve destroying legitimate irregular tonic.
@Test
func tswPauseLikeCarvePreservesIrregularTonic() {
    // CV ~0.35, max beat ~1.3x q90 — within the bulk, no isolated 2x+ gap.
    let isis: [Double] = [0.030, 0.048, 0.036, 0.052, 0.033, 0.050, 0.038, 0.046,
                          0.031, 0.049, 0.035, 0.051, 0.034, 0.047, 0.037, 0.050]
    let train = tswiTrain("irregular_no_pause", isis)
    let accepted = tswiAcceptedTonic(train)
    #expect(!accepted.isEmpty)
    #expect(accepted.allSatisfy { !$0.decisionPath.contains("tonic_pause_like_outlier") })  // NOT carved
    // Coverage is not fragmented into sub-seed shards: the run is captured as (at most a couple of) sustained
    // tonic candidates, and at least one spans a long stretch.
    #expect(accepted.contains { ($0.endISIIndex - $0.startISIIndex + 1) >= 10 })
}

// 9 — a large pause ISI at the tonic boundary (not bridge-spannable) stops expansion and is not swallowed,
// AND is not resurrected by the carve path. Complements tswIntegrationStopsBeforeLargePause.
@Test
func tswPauseLikeCarveKeepsBoundaryPauseExcluded() {
    let isis = Array(repeating: 0.050, count: 10) + [1.4] + Array(repeating: 0.050, count: 4)  // pause @ ISI 11
    let train = tswiTrain("tonic_then_bigpause", isis)
    let accepted = tswiAcceptedTonic(train)
    #expect(!accepted.contains { ($0.startISIIndex...$0.endISIIndex).contains(11) })  // pause never tonic
    #expect(accepted.contains { $0.startISIIndex == 1 && $0.endISIIndex == 10 })      // pre-pause tonic intact
}

// MARK: - Short tonic segments are a CONFIGURABLE first-stage property (minTonicSpikes), not a pause miss.

// 10 — CONTEXT-AWARE short tonic is recovered BY DEFAULT in a clean context: a 3-ISI / 4-spike tonic-range
// segment (pause_response_1_s [17...19]) is promoted to canonical tonic via the context-aware short path
// (train is tonic-dominated + classic, magnitude-consistent, pause-separated). Still works when explicitly
// configured to minTonicSpikes=3 (the primary sliding-window path).
@Test
func tswShortTonicRecoveredByDefaultInCleanContext() throws {
    let dataset = try tswiFixture5x5()
    let train = try #require(dataset.trains.first { $0.name == "pause_response_1_s" })

    let atDefault = tswiAcceptedTonic(train)                                      // default minTonicSpikes = 5
    #expect(atDefault.contains {                                                  // recovered by default…
        $0.startISIIndex <= 17 && $0.endISIIndex >= 19
            && $0.decisionPath.contains("short_tonic_island")                    // …via the pause-separated island path
            && $0.decisionPath.contains("short_tonic_context_clean")
    })

    var s3 = StatePatternDetectorSettings(); s3.tonicMinSpikes = 3               // explicit config still works
    let atThree = StatePatternDetector.detect(train: train, settings: s3).candidates
        .filter { $0.finalLabel == .tonic && $0.action == "accept" }
    #expect(atThree.contains { $0.startISIIndex <= 17 && $0.endISIIndex >= 19 })
}

// 11 — the short segment is recovered end-to-end through the PIPELINE by default, and also under explicit
// minTonicSpikes=3 (via stateTuning and via a manual tonic minSpikes hard gate).
@Test
func tswShortTonicRecoveredThroughPipeline() throws {
    let dataset = try tswiFixture5x5()
    let train = try #require(dataset.trains.first { $0.name == "pause_response_1_s" })
    let single = SpikeDataset(name: "one", sourceDescription: dataset.sourceDescription, trains: [train])
    func shortSelected(_ run: ClassicAnchorDetectionRun) -> Bool {
        (run.result(for: train.id)?.candidates ?? []).contains {
            $0.selectedForAuto && $0.finalLabel == .tonic && $0.startISIIndex <= 17 && $0.endISIIndex >= 19
        }
    }
    #expect(shortSelected(ClassicAnchorDetectionPipeline.run(dataset: single)))                  // by default now
    #expect(shortSelected(ClassicAnchorDetectionPipeline.run(                                     // via tuning
        dataset: single, stateTuning: StatePatternDetectorTuning(tonicMinSpikes: 3))))
    let profile = ManualThresholdProfile(tonic: TonicManualThresholds(
        minSpikes: ManualSpikeCountThreshold(mode: .hardGate, value: 3)))
    #expect(shortSelected(ClassicAnchorDetectionPipeline.run(                                     // via manual gate
        dataset: single, manualThresholdProfile: profile)))
}

// 12 — the miss is first-stage GENERATION, NOT pause arbitration: in this non-overlapping fixture the
// short segment's ISIs are in the tonic magnitude range and NO selected pause candidate covers them.
@Test
func tswShortTonicMissIsGenerationNotPause() throws {
    let dataset = try tswiFixture5x5()
    let train = try #require(dataset.trains.first { $0.name == "pause_response_1_s" })
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "one", sourceDescription: dataset.sourceDescription, trains: [train]))
    let cs = run.result(for: train.id)?.candidates ?? []
    let vals = (1...(train.isiSec.count - 1)).compactMap { train.isiSec[$0] }.sorted()
    let median = vals[vals.count / 2]
    for i in 17...19 {
        #expect((train.isiSec[i] ?? 0) < median * 1.8)                            // tonic-range, not a pause gap
        #expect(!cs.contains { $0.selectedForAuto && $0.finalLabel == .pause
            && $0.startISIIndex <= i && $0.endISIIndex >= i })                    // no pause claims it
    }
}

// MARK: - TSW context-aware short tonic — noisy/irregular/bursty trains must NOT gain canonical short tonic.

private func tswiGrechishnikova() throws -> SpikeDataset? {
    let repo = URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
        .deletingLastPathComponent().deletingLastPathComponent()
    let url = repo.appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    guard FileManager.default.fileExists(atPath: url.path) else { return nil }
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                          datasetName: "grech", sourceDescription: url.path, allowDerivedCSV: true)
}
private func tswiShortContextCanonical(_ train: SpikeTrain) -> [ClassicAnchorCandidate] {
    StatePatternDetector.detect(train: train).candidates.filter {
        $0.finalLabel == .tonic && $0.action == "accept"
            && $0.decisionPath.contains("short_tonic_island")
    }
}

// 13 — high-jitter (globally irregular) train: its locally-regular short windows must NOT be promoted to
// canonical tonic — the dominant tonic is not a tight classic mode (dominant_tonic_classic=false).
@Test
func tswShortTonicRejectedInHighJitterTrain() {
    var sim = SpikeTrainSimulator(seed: 42)
    let train = sim.tonicTrain(durationSec: 30, sdSec: 0.30, lowerSec: 0.05, upperSec: 2.0)
    #expect(tswiShortContextCanonical(train).isEmpty)
    // Any short island it did scan is at most possible_tonic_review, never canonical tonic.
    let review = StatePatternDetector.detect(train: train).candidates
        .filter { $0.decisionPath.contains("short_tonic_island") }
    #expect(review.allSatisfy { $0.action != "accept" || $0.finalLabel != .tonic })
}

// 14 — a lone short regular cluster embedded in otherwise irregular activity (no dominant classic tonic)
// is NOT canonical tonic.
@Test
func tswShortTonicLoneClusterNotCanonical() {
    let isis: [Double] = [0.9, 1.1, 0.8, 1.2, 0.050, 0.052, 0.049, 0.7, 1.0, 0.85, 1.15]  // 3-ISI cluster @5..7
    let train = tswiTrain("lone_cluster", isis)
    #expect(tswiShortContextCanonical(train).isEmpty)
}

// 15 — bursty STN (Grechishnikova): short regular micro-epochs must NOT be canonical tonic (burst-dominated,
// not tonic-dominated). Complements tonicBoundaryRescueIsInertOnBurstyGrechishnikova.
@Test
func tswShortTonicBurstyMicroEpochNotCanonical() throws {
    guard let ds = try tswiGrechishnikova() else { return }   // graceful skip if fixture absent
    for tr in ds.trains {
        #expect(tswiShortContextCanonical(tr).isEmpty, "\(tr.name): bursty short micro-epoch must not be canonical tonic")
    }
}

// MARK: - TSW short tonic ISLAND recovery (pause-separated islands captured in full, by default).

// 16 — the exact reported case: pause_response_5_s island [21...23] is recovered as ONE tonic candidate
// that INCLUDES ISI 21 (413ms, below the adaptive band lower — the sliding window would have trimmed it),
// together with 22/23.
@Test
func tswShortTonicIslandIncludesLeftEdgeISI() throws {
    let dataset = try tswiFixture5x5()
    let train = try #require(dataset.trains.first { $0.name == "pause_response_5_s" })
    let acc = tswiAcceptedTonic(train)
    let island = acc.first { $0.startISIIndex <= 21 && $0.endISIIndex >= 23 }
    #expect(island != nil)                                                     // full island incl. ISI 21
    #expect(island?.decisionPath.contains("short_tonic_island") == true)
    #expect(island?.decisionPath.contains("pause_separated_tonic_island=true") == true)
}

// 17 — later pause_response_* short islands between pauses are all recovered BY DEFAULT (not just the first),
// including ones the sliding window previously failed to generate.
@Test
func tswShortTonicLaterIslandsRecoveredByDefault() throws {
    let dataset = try tswiFixture5x5()
    let cases: [(String, Int, Int)] = [
        ("pause_response_2_s", 28, 30),   // previously "no candidate generated"
        ("pause_response_5_s", 21, 23),   // previously only [22...23]
        ("pause_response_1_s", 30, 32),
    ]
    for (name, s, e) in cases {
        let train = try #require(dataset.trains.first { $0.name == name })
        let acc = tswiAcceptedTonic(train)
        #expect(acc.contains { $0.startISIIndex <= s && $0.endISIIndex >= e },
                "\(name) [\(s)...\(e)] should be canonical tonic")
    }
}

// 18 — large pause ISIs flanking the islands remain NOT tonic (island recovery never swallows a pause).
@Test
func tswShortTonicIslandsLeaveLargePausesUnclaimed() throws {
    let dataset = try tswiFixture5x5()
    let train = try #require(dataset.trains.first { $0.name == "pause_response_5_s" })
    let acc = tswiAcceptedTonic(train)
    let vals = (1...(train.isiSec.count - 1)).compactMap { train.isiSec[$0] }.sorted()
    let median = vals[vals.count / 2]
    for i in [20, 24] {   // the pauses flanking island [21...23]
        #expect((train.isiSec[i] ?? 0) > median * 1.8)                          // it IS a large pause
        #expect(!acc.contains { $0.startISIIndex <= i && $0.endISIIndex >= i })  // …and never tonic
    }
}

// MARK: - AUDIT F3 — short-tonic island recovery honors the manual hard tonic gate.

// An 8-ISI classic tonic anchor (~100 ms), a pause, a 3-ISI island (~140 ms = 1.4× the anchor: magnitude-consistent
// and below the pause threshold, but ABOVE a manual hard upper of 120 ms), a pause. The island is at ISI [10...12].
private func tswiManualGateIslandTrain() -> SpikeTrain {
    let anchor = [0.098, 0.101, 0.099, 0.100, 0.102, 0.098, 0.101, 0.100]   // ISI 1...8, classic anchor
    let island = [0.140, 0.139, 0.141]                                       // ISI 10...12
    return tswiTrain("short_island_manual_gate", anchor + [0.5] + island + [0.5])
}
private func tswiShortIslandCanonical(_ train: SpikeTrain, manualLower: Double?, manualUpper: Double?) -> [ClassicAnchorCandidate] {
    var settings = StatePatternDetectorSettings()
    settings.tonicStructuralWindowPrimary = true
    settings.manualTonicHardLowerSec = manualLower
    settings.manualTonicHardUpperSec = manualUpper
    return StatePatternDetector.detect(train: train, settings: settings).candidates
        .filter { $0.finalLabel == .tonic && $0.action == "accept" && $0.decisionPath.contains("short_tonic_island") }
}

// 19a — an island OUTSIDE the manual hard tonic band is NOT recovered as canonical tonic (routed to review).
@Test
func tswShortIslandOutsideManualHardGateNotCanonical() {
    let train = tswiManualGateIslandTrain()
    let canonical = tswiShortIslandCanonical(train, manualLower: 0.001, manualUpper: 0.120)   // 0.120 < island 0.140
    #expect(!canonical.contains { $0.startISIIndex <= 10 && $0.endISIIndex >= 12 })
    // It appears as possible_tonic_review carrying the manual-gate reason.
    var s = StatePatternDetectorSettings(); s.manualTonicHardLowerSec = 0.001; s.manualTonicHardUpperSec = 0.120
    let scanned = StatePatternDetector.detect(train: train, settings: s).candidates
        .filter { $0.decisionPath.contains("short_tonic_island") }
    #expect(scanned.contains { $0.decisionPath.contains("outside_manual_tonic_hard_gate") })
    #expect(scanned.allSatisfy { $0.finalLabel != .tonic || $0.action != "accept" })
}

// 19b — the SAME island INSIDE a wider manual hard band IS recovered as canonical tonic.
@Test
func tswShortIslandInsideManualHardGateRecovered() {
    let train = tswiManualGateIslandTrain()
    let canonical = tswiShortIslandCanonical(train, manualLower: 0.001, manualUpper: 0.200)   // 0.200 > island 0.140
    #expect(canonical.contains { $0.startISIIndex <= 10 && $0.endISIIndex >= 12 })
}
