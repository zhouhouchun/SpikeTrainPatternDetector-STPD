import Foundation
import STPDCore
import Testing

// ISI State Space parity P1: pure mirror of stpd_make_logisi_phase_portrait. Validates the logISI /
// current-next / delta geometry, validity gating, lag, ordering by idx, and winsorization.

// MARK: - 1. Basic logISI / current-next / time geometry.

@Test
func phasePortraitProducesExpectedLogISIAndNextISI() {
    // ISIs: 0.1, 0.2, 0.3, 0.4 (timestamps cumulative).
    let ts = [0.0, 0.1, 0.3, 0.6, 1.0]
    let rows = ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: ts)

    // p = 1,2,3 (0-based) qualify (p >= 1, p + 1 <= n - 1 = 4, all valid) -> 3 transition rows.
    #expect(rows.count == 3)

    let first = rows[0]
    #expect(first.rowNumber == 2)                 // R 1-based row_number for spike position p = 1
    #expect(first.idx == 2 && first.nextIdx == 3) // default idx = seq_len(n)
    #expect(abs(first.isiSec - 0.1) < 1e-12)
    #expect(abs(first.nextISISec - 0.2) < 1e-12)
    #expect(abs(first.logISIi - log10(0.1)) < 1e-12)       // -1
    #expect(abs(first.logISINext - log10(0.2)) < 1e-12)
    #expect(abs(first.leftTimeSec - 0.0) < 1e-12)
    #expect(abs(first.rightTimeSec - 0.1) < 1e-12)
    #expect(abs(first.timeMidSec - 0.05) < 1e-12)
    #expect(first.transition == "unlabeled -> unlabeled")
    #expect(first.labelSource == "auto")

    let last = rows[2]
    #expect(abs(last.isiSec - 0.3) < 1e-12)
    #expect(abs(last.nextISISec - 0.4) < 1e-12)
    #expect(abs(last.logISINext - log10(0.4)) < 1e-12)
}

// MARK: - 2. Too few rows -> empty (R `if (nrow(dat) < 4) return(data.frame())`).

@Test
func phasePortraitReturnsEmptyForTooFewSpikes() {
    #expect(ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: [0.0, 0.1, 0.2]).isEmpty)
    #expect(ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: []).isEmpty)
}

// MARK: - 3. Validity gating: a sub-threshold (e.g. duplicate-timestamp) ISI removes its transitions.

@Test
func phasePortraitGatesSubThresholdISIs() {
    // spike index 2 duplicates spike 1's time -> isi[2] = 0 (< min) -> invalid.
    let ts = [0.0, 0.10, 0.10, 0.30, 0.55, 0.85]
    let rows = ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: ts, minValidISISec: 0.001)
    // No emitted transition may involve the invalid spike (idx 3 in 1-based) as current or next.
    #expect(rows.allSatisfy { $0.idx != 3 && $0.nextIdx != 3 })
    // All emitted ISIs are >= the threshold.
    #expect(rows.allSatisfy { $0.isiSec >= 0.001 && $0.nextISISec >= 0.001 })
}

// MARK: - 4. Lag > 1 skips the intermediate spike.

@Test
func phasePortraitHonorsLag() {
    let ts = [0.0, 0.1, 0.3, 0.6, 1.0, 1.5]   // 6 spikes
    let lag1 = ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: ts, lag: 1)
    let lag2 = ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: ts, lag: 2)
    #expect(lag2.count == lag1.count - 1)        // one fewer pair at lag 2
    // At lag 2 the next index is two spikes ahead.
    #expect(lag2.allSatisfy { $0.nextIdx == $0.idx + 2 })
}

// MARK: - 5. Labels pass through; ordering follows the supplied source idx.

@Test
func phasePortraitPassesLabelsAndOrdersByIdx() {
    let ts = [0.0, 0.1, 0.3, 0.6, 1.0]
    let labels = ["a", "b", "c", "d", "e"]
    let rows = ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: ts, labels: labels, labelSource: "manual")
    #expect(rows.first?.label == "b" && rows.first?.nextLabel == "c")  // spike position p=1 -> labels[1],[2]
    #expect(rows.first?.transition == "b -> c")
    #expect(rows.allSatisfy { $0.labelSource == "manual" })
}

// MARK: - 6. Winsorization clamps extreme logISI (mirror of stpd_winsorize_numeric).

@Test
func phasePortraitWinsorizationClampsExtremes() {
    // Mostly ~0.1 s ISIs plus one huge outlier -> winsorizing the logISI vector caps the outlier.
    var ts = [0.0]
    for _ in 0..<6 { ts.append(ts.last! + 0.1) }
    ts.append(ts.last! + 50.0)   // outlier ISI
    let raw = ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: ts, winsorize: false)
    let winsor = ISIStateSpaceLogISIPhasePortrait.build(timestampsSec: ts, winsorize: true)
    #expect(raw.count == winsor.count)
    let rawMax = raw.flatMap { [$0.logISIi, $0.logISINext] }.filter(\.isFinite).max() ?? 0
    let winsorMax = winsor.flatMap { [$0.logISIi, $0.logISINext] }.filter(\.isFinite).max() ?? 0
    #expect(rawMax > winsorMax)              // the 50 s outlier's log10 (~1.7) is clamped down
    #expect(abs(rawMax - log10(50.0)) < 1e-9)
}
