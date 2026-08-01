import Foundation
import STPDCore
import Testing

// P0 — CHARACTERIZATION PINS of CURRENT detector behavior on ambiguous burst/tonic cases. These assert what the
// detector does TODAY (not what it should do). They are expected to flip DELIBERATELY when the Adaptive-v2
// canonicalization phases (P3+) change behavior; until then they guard against accidental drift. No behavior change.
//
// Fixtures are profile-implicit synthetic trains; ms values are concrete only to construct a train, and the
// assertions are about the RESULTING families, not the ms numbers.

private struct SelectionOutcome {
    let burst: Int
    let tonic: Int
    let hfTonic: Int
    let lockedClassicBurst: Int
    let strongCandidateBurst: Int
}

private func observe(_ name: String, _ times: [Double]) throws -> SelectionOutcome {
    let csv = ([name] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    let ds = try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p0", sourceDescription: "p0")
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let selected = (run.result(for: ds.trains[0].id)?.candidates ?? []).filter(\.selectedForAuto)
    let burst = selected.filter { $0.finalLabel.isBurstEventFamily }
    return SelectionOutcome(
        burst: burst.count,
        tonic: selected.filter { $0.finalLabel == .tonic }.count,
        hfTonic: selected.filter { $0.finalLabel == .highFrequencyTonic }.count,
        lockedClassicBurst: burst.filter { $0.anchorLockLevel == .lockedClassic }.count,
        strongCandidateBurst: burst.filter { $0.anchorLockLevel == .strongCandidate }.count
    )
}

/// Repeated regular packet of `spikes` spikes at `isi` s, separated by `gap` s pause-like gaps.
private func pauseFlankedPackets(spikes: Int, isi: Double, gap: Double, repeats: Int) -> [Double] {
    var t = 0.0
    var times = [0.0]
    for _ in 0..<repeats {
        for _ in 0..<(spikes - 1) { t += isi; times.append(t) }
        t += gap
        times.append(t)
    }
    return times
}

@Test
func p0_PauseFlankedShortRegularPacket_currentlyBecomesBurstNotTonic() throws {
    // PROBLEM A pin: a regular ~60 ms packet too short for a state run, flanked by large gaps, is CURRENTLY
    // selected as burst (local-compression route) with NO tonic — the false-burst the redesign aims to demote.
    let outcome = try observe("pf_short", pauseFlankedPackets(spikes: 4, isi: 0.06, gap: 1.5, repeats: 5))
    #expect(outcome.burst >= 1)            // currently burst-family selected
    #expect(outcome.tonic == 0)            // tonic does not win (and is not generated at this size)
    // The offending bursts are strong-candidate (evidence-grade), not locked classic seed/core bursts.
    #expect(outcome.strongCandidateBurst >= 1)
}

@Test
func p0_PauseFlankedLongerRegularPacket_currentlyTonicOrHFTonicNotBurst() throws {
    // MUST-NOT-REGRESS pin: a regular fast packet with enough spikes for a state run is ALREADY handled as
    // tonic / HF-tonic today (no burst). The redesign must keep these non-burst.
    let outcome = try observe("pf_long", pauseFlankedPackets(spikes: 6, isi: 0.03, gap: 1.5, repeats: 5))
    #expect(outcome.burst == 0)
    #expect(outcome.tonic + outcome.hfTonic >= 1)
}

@Test
func p0_ClassicBurstSeedCore_currentlyDetectedAsLockedClassicBurst() throws {
    // ANCHOR pin: genuine short-ISI bursts embedded in a tonic baseline are detected as locked-classic burst.
    // The redesign must continue to detect these.
    var t = 0.0
    var times = [0.0]
    for blk in 0..<5 {
        for _ in 0..<6 { t += 0.40; times.append(t) }            // tonic baseline
        if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } }   // fast burst packet
    }
    let outcome = try observe("classic_burst", times)
    #expect(outcome.lockedClassicBurst >= 1)
    #expect(outcome.tonic >= 1)
}

@Test
func p0_PureRegularTrain_currentlyTonicNoBurst() throws {
    // BASELINE pin: a pure regular train is one tonic span, no burst.
    var t = 0.0
    var times = [0.0]
    let cyc = [0.45, 0.46, 0.44]
    for i in 0..<29 { t += cyc[i % 3]; times.append(t) }
    let outcome = try observe("regular_tonic", times)
    #expect(outcome.burst == 0)
    #expect(outcome.tonic >= 1)
}
