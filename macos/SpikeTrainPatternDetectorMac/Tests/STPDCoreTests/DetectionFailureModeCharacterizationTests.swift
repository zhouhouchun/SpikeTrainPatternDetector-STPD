import Foundation
import STPDCore
import Testing

// MARK: - ALGO-DIAG Phase 10: regression fixtures for known detection failure modes.
//
// This file is a SAFETY NET created BEFORE any threshold/algorithm retuning. Each test below records
// the CURRENT detector behavior for a concrete failure mode observed in the app. Some of these locks
// describe behavior that is already correct (e.g. the early-tonic-as-pause fix); others deliberately
// pin behavior that is still SUSPECTED WRONG (e.g. a brief acceleration labeled burst, a lone moderate
// doublet left "other"). They are CHARACTERIZATION tests — they assert what the pipeline does today,
// NOT final biological truth. When a later phase intentionally changes one of these behaviors, the
// matching test must be updated deliberately, which is exactly the signal this net is here to provide.
//
// Fixtures are small, deterministic, inline ISI sequences (no file or absolute-path dependency) and use
// only public detector APIs + provenance fields.

// MARK: Characterization harness

private let phase10BandSettings = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)

/// Hard Burst seed max = 100 ms (the user-facing "Hard" burst ceiling referenced by the failure modes).
private let hardBurstSeedMax100ms = ManualThresholdProfile(
    burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.100))
)

private func cumulative(_ isis: [Double]) -> [Double] {
    var timestamps: [Double] = [0]
    timestamps.reserveCapacity(isis.count + 1)
    for d in isis { timestamps.append((timestamps.last ?? 0) + d) }
    return timestamps
}

/// Stable family token derived from the machine label (`finalLabel.rawValue` / `isBurstEventFamily`),
/// not from any UI display string, so the characterization is robust to display renames.
private func familyToken(_ label: ClassicAnchorLabel) -> String {
    if label.isBurstEventFamily { return "burst" }
    switch label {
    case .pause: return "pause"
    case .tonic: return "tonic"
    case .highFrequencyTonic: return "hf_tonic"
    case .highFrequencySpiking: return "hf_spiking"
    default: return label.rawValue
    }
}

/// A read-only snapshot of one detector run over one train: per-ISI family, selected-candidate
/// coverage (with provenance layer + selection status), adaptive burst band, and family counts.
private struct FailureModeSnapshot {
    struct Selected {
        let labelRaw: String
        let isBurstFamily: Bool
        let start: Int
        let end: Int
        let maxIntraSec: Double
        let layer: String
        let status: String
    }
    let nISI: Int
    let selectedAutoCount: Int
    let burstCount: Int
    let pauseCount: Int
    let tonicCount: Int
    let highFrequencyTonicCount: Int
    let highFrequencySpikingCount: Int
    let burstSeedUpperSec: Double?
    let burstBridgeUpperSec: Double?
    /// `perISIFamily[i]` is the family token the raster overlay shows for ISI index `i`, taken from the
    /// detector's exclusive-final projection ("other" if uncovered). Index 0 unused.
    let perISIFamily: [String]
    let selected: [Selected]

    func family(_ i: Int) -> String { (i >= 0 && i < perISIFamily.count) ? perISIFamily[i] : "other" }
    func families(_ range: ClosedRange<Int>) -> [String] { range.map(family) }
    /// The highest-(numeric-)priority SELECTED candidate covering ISI `i` (the raw candidate-coverage view,
    /// distinct from the projected per-ISI family above), or nil if `i` is uncovered by any selected candidate.
    func covering(_ i: Int) -> Selected? { selected.first { $0.start <= i && i <= $0.end } }
    var selectedBurstFamily: [Selected] { selected.filter(\.isBurstFamily) }
}

private func characterize(_ train: SpikeTrain, profile: ManualThresholdProfile = .automatic) -> FailureModeSnapshot {
    let dataset = SpikeDataset(name: "phase10", sourceDescription: "characterization fixture", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: phase10BandSettings,
        manualThresholdProfile: profile
    )
    let candidates = run.result(for: train.id)?.candidates ?? []
    let selectedCandidates = candidates.filter(\.selectedForAuto)

    // Per-ISI family taken from the SAME exclusive-final projection the raster overlay uses:
    // `run.eventAnnotations(in:tracks:)` runs `exclusiveFinalProjection`, which resolves overlap by
    // label-derived projectionPriority (burst > tonic/hf-tonic > hfs > pause), then priority/score, and
    // returns non-overlapping projected runs. We rasterize those into the per-ISI array rather than
    // re-deriving a second overlap rule, so this faithfully reflects what the overlay/inspector shows.
    var perISIFamily = [String](repeating: "other", count: train.isiSec.count)
    let projectedAnnotations = run
        .eventAnnotations(in: dataset, tracks: [.event, .gap, .state])
        .filter { $0.trainID == train.id }
    for annotation in projectedAnnotations {
        guard let range = annotation.coveredISIIndices(in: train) else { continue }
        for i in range where i >= 0 && i < perISIFamily.count {
            perISIFamily[i] = familyToken(annotation.label)
        }
    }

    let selected = selectedCandidates
        .sorted { ($0.priority, $0.startISIIndex ?? -1) > ($1.priority, $1.startISIIndex ?? -1) }
        .map { c in
            FailureModeSnapshot.Selected(
                labelRaw: c.finalLabel.rawValue,
                isBurstFamily: c.finalLabel.isBurstEventFamily,
                start: c.startISIIndex ?? -1,
                end: c.endISIIndex ?? -1,
                maxIntraSec: c.maxIntraISISec ?? -1,
                layer: c.candidateLayer,
                status: c.selectionStatus
            )
        }

    let band = run.resolution(for: train.id)?.burstBand
    return FailureModeSnapshot(
        nISI: max(0, train.isiSec.count - 1),
        selectedAutoCount: run.selectedAutoCount,
        burstCount: run.burstCount,
        pauseCount: run.pauseCount,
        tonicCount: run.tonicCount,
        highFrequencyTonicCount: run.highFrequencyTonicCount,
        highFrequencySpikingCount: run.highFrequencySpikingCount,
        burstSeedUpperSec: band?.seedUpperSec,
        burstBridgeUpperSec: band?.bridgeUpperSec,
        perISIFamily: perISIFamily,
        selected: selected
    )
}

private func approx(_ value: Double?, _ target: Double, tol: Double) -> Bool {
    guard let value, value.isFinite else { return false }
    return abs(value - target) <= tol
}

// MARK: Fixtures (small, deterministic, path-independent)

/// Verbatim ISI sequence of the example dataset's `burst_response_2_s` (99 ISIs / 100 spikes): a uniform
/// ~0.40-0.50 s tonic baseline, short pre-burst transition ISIs (0.16-0.29 s), and eight compact bursts
/// (some as tight as 6-16 ms). Shared in spirit with BurstResponseHardModeRegressionTests /
/// PauseMonotonicCompletionTests; reproduced here so this safety-net file stands alone.
private func burstResponse2SFixture() -> SpikeTrain {
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
    return SpikeTrain(name: "burst_response_2_s", timestampsSec: cumulative(isis))
}

/// Deterministic stand-in for `pause_response_2_s`: a slow ~2.0 s tonic baseline broken by a single short
/// 4-ISI 0.080 s acceleration packet (ISI 7...10). Mirrors the user-reported "sparse span shown as an
/// orange burst" — the real screenshot train is approximated by this minimal inline fixture.
private func pauseResponse2SFixture() -> SpikeTrain {
    SpikeTrain(
        name: "pause_response_2_s",
        timestampsSec: [0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 12.08, 12.16, 12.24, 12.32,
                        14.32, 16.32, 18.32, 20.32, 22.32, 24.32]
    )
}

/// A lone moderate interval (a 2-spike 0.085 s doublet, ISI 4) isolated in a 2.0 s baseline.
private func moderateLoneDoubletFixture() -> SpikeTrain {
    SpikeTrain(name: "moderate_lone_doublet_85ms", timestampsSec: cumulative([2, 2, 2, 0.085, 2, 2]))
}

/// A moderate 2-ISI (3-spike) 0.090 s cluster (ISI 4...5) isolated in a 2.0 s baseline.
private func moderateTripletClusterFixture() -> SpikeTrain {
    SpikeTrain(name: "moderate_triplet_90ms", timestampsSec: cumulative([2, 2, 2, 0.090, 0.090, 2, 2]))
}

/// A uniform regular high-frequency train.
private func regularHFTrain(isiSec: Double, count: Int) -> SpikeTrain {
    SpikeTrain(name: "regular_hf_\(Int(isiSec * 1000))ms", timestampsSec: cumulative(Array(repeating: isiSec, count: count)))
}

/// A 0.010 s high-frequency baseline with embedded tight 0.003 s burst doublets (every 5th/6th ISI).
private func hfBaselineWithEmbeddedBurstsFixture() -> SpikeTrain {
    var isis: [Double] = []
    for _ in 0..<10 { isis += [0.010, 0.010, 0.010, 0.003, 0.003] }
    return SpikeTrain(name: "hf_baseline_with_embedded_bursts", timestampsSec: cumulative(isis))
}

// MARK: Failure mode 2 — brief tonic-rate acceleration is NOT a burst (pause_response_2_s) [Phase 11 fix]

// DELIBERATE UPDATE (ALGO-RETUNE Phase 11): this characterization originally pinned the false positive —
// a brief, perfectly regular 4-ISI 0.080 s acceleration (12.5 Hz, tonic-rate) embedded in a 2.0 s silent
// baseline was labeled a selected Burst purely on edge contrast against the silence. Phase 11 added an
// automatic-mode tonic-rate-regular-burst guard (intra q90 above the classical-burst core ceiling AND
// near-perfectly regular -> not a burst), so the packet is now correctly NOT a selected burst. The
// adaptive burst band itself is unchanged (the guard acts at admission, not band resolution); the packet
// is now uncovered ("other") and the surrounding tonic baseline is unchanged.
@Test
func pauseResponse2S_briefPacketIsNotBurstAfterPhase11() {
    let snap = characterize(pauseResponse2SFixture(), profile: .automatic)
    #expect(snap.nISI == 16)

    // The 0.080 s packet (ISI 7...10) is no longer a selected Burst — it is uncovered ("other").
    #expect(snap.families(7...10).allSatisfy { $0 == "other" })
    #expect(snap.selectedBurstFamily.first { $0.start <= 7 && $0.end >= 10 } == nil)
    // No selected burst-family candidate anywhere in this train.
    #expect(snap.selectedBurstFamily.isEmpty)
    // The surrounding 2.0 s baseline is tonic on both sides (unchanged).
    #expect(snap.families(1...6).allSatisfy { $0 == "tonic" })
    #expect(snap.families(11...16).allSatisfy { $0 == "tonic" })
    // The adaptive burst band still resolves up to the packet (~80 ms) — only the admission was guarded.
    #expect(approx(snap.burstSeedUpperSec, 0.080, tol: 0.005))
}

// MARK: Failure mode 3 — moderate lone cluster stays "other" under hard seed max = 100 ms

// CURRENT BEHAVIOR (characterization): a LONE moderate interval (a 2-spike 0.085 s doublet) isolated in a
// 2.0 s baseline stays "other" even under the hard 100 ms ceiling — no burst candidate is proposed and the
// adaptive burst band collapses to ~2 ms. A 2-ISI (3-spike) 0.090 s cluster IS recovered as a Burst under
// the SAME hard ceiling. This documents the current doublet/triplet recovery boundary: the hard ceiling
// relaxes the upper bound but does not synthesize a candidate for a lone doublet.
@Test
func moderateLoneClusterRemainsOtherUnderHardSeedMax_characterization() {
    let doublet = characterize(moderateLoneDoubletFixture(), profile: hardBurstSeedMax100ms)
    #expect(doublet.nISI == 6)
    #expect(doublet.selectedAutoCount == 0)
    #expect(doublet.selectedBurstFamily.isEmpty)
    #expect(doublet.family(4) == "other")                          // the lone 0.085 s interval
    #expect(approx(doublet.burstSeedUpperSec, 0.002, tol: 1e-6))   // collapsed adaptive burst band

    // Contrast: a 2-ISI 0.090 s cluster IS recovered as a selected Burst under the same hard ceiling.
    let triplet = characterize(moderateTripletClusterFixture(), profile: hardBurstSeedMax100ms)
    #expect(triplet.family(4) == "burst" && triplet.family(5) == "burst")
    #expect(triplet.selectedBurstFamily.contains { $0.start <= 4 && $0.end >= 5 })
}

// MARK: Failure mode 4 — HFS / HF-tonic / HF-burst boundary

// CURRENT BEHAVIOR (characterization): a uniform regular high-frequency train (0.008 s here, and even a
// slower 0.025 s) resolves to a single whole-train HFS state — NOT HF-tonic, NOT HF-burst, NOT classic
// tonic. Embedding tight 0.003 s burst doublets into a 0.010 s HF baseline instead fragments it into
// selected Burst cores with an uncovered ("other") baseline and SUPPRESSES the HFS state. No HF-tonic /
// HF-burst label is produced for these compact fixtures; this locks where the current boundary sits.
@Test
func regularHighFrequencyTrainResolvesToHFSState_characterization() {
    let hf8 = characterize(regularHFTrain(isiSec: 0.008, count: 60), profile: .automatic)
    #expect(hf8.selectedAutoCount == 1)
    #expect(hf8.highFrequencySpikingCount == 1)
    #expect(hf8.burstCount == 0)
    #expect(hf8.tonicCount == 0)
    #expect(hf8.highFrequencyTonicCount == 0)
    #expect(hf8.families(1...8).allSatisfy { $0 == "hf_spiking" })
    let hfState = hf8.selected.first
    #expect(hfState?.labelRaw == "high_frequency_spiking")
    #expect(hfState?.start == 1 && hfState?.end == 60)
    #expect(hfState?.layer == "event_grammar_hf_spiking_state")

    // Even a slower 0.025 s regular HF train is still HFS (not HF-tonic / classic tonic).
    let hf25 = characterize(regularHFTrain(isiSec: 0.025, count: 40), profile: .automatic)
    #expect(hf25.highFrequencySpikingCount == 1)
    #expect(hf25.highFrequencyTonicCount == 0)
    #expect(hf25.tonicCount == 0)
    #expect(hf25.families(1...8).allSatisfy { $0 == "hf_spiking" })

    // Embedded tight bursts fragment the HF baseline: 0.003 s cores are Burst, baseline is uncovered, no
    // HFS state survives.
    let embedded = characterize(hfBaselineWithEmbeddedBurstsFixture(), profile: .automatic)
    #expect(embedded.highFrequencySpikingCount == 0)
    #expect(embedded.burstCount > 0)
    #expect(embedded.family(4) == "burst" && embedded.family(5) == "burst")
    #expect(embedded.family(1) == "other")
}
