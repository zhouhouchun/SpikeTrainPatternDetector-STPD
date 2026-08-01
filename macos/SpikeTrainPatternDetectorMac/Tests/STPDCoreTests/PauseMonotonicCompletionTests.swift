import Foundation
import STPDCore
import Testing

// MARK: - PAUSE-1: tonic baseline ISIs must not be completed as pause by the monotonic floor layer.
//
// PauseMonotonicCompletionDetector infers a pause floor from already-selected pauses and then emits a
// .pause for EVERY unoccupied ISI >= that floor ("if a same-train pause minimum is established, any
// larger unoccupied ISI is a pause"). When a train's short pre-burst transition ISIs seed a pause floor
// that sits BELOW the tonic baseline, the whole baseline is wrongly completed as pause. R only flags an
// ISI as pause when it also exceeds a LOCAL median by pause_relative_local_factor (1.55) and the GLOBAL
// median by pause_relative_global_factor (1.25) — a guard the Swift completion layer omitted.

private func pause1Cumulative(_ isis: [Double]) -> [Double] {
    var t: [Double] = [0]
    for d in isis { t.append(t.last! + d) }
    return t
}

// Faithful mirror of the example dataset's burst_response_2_s: a uniform ~0.45 s tonic baseline broken
// by short pre-burst transition ISIs (~0.19-0.30 s — long vs the ~0.03 s burst, but SHORTER than the
// baseline) and compact bursts. The transition ISIs seed a pause floor (~0.28 s) below the 0.45 s
// baseline, so the floor-completion layer wrongly treats the entire baseline as pause. A genuine 1.30 s
// pause (~3x baseline) is included to confirm true pauses are preserved.
private func pause1SyntheticTrain() -> (train: SpikeTrain, longPauseISIIndex: Int) {
    let baseTmpl = [0.45, 0.50, 0.42, 0.46, 0.43, 0.47]
    let trans = [0.186, 0.291, 0.25, 0.21, 0.28, 0.27, 0.30, 0.24, 0.26]
    var isis: [Double] = []
    for cyc in 0..<9 {
        for b in baseTmpl { isis.append(b) }
        isis.append(trans[cyc % trans.count])
        for x in [0.031, 0.040, 0.030] { isis.append(x) }
        if cyc == 4 { isis.append(1.30) } // genuine long pause (~3x baseline)
    }
    for b in baseTmpl { isis.append(b) }
    let train = SpikeTrain(name: "synthetic_tonic_baseline_burst", timestampsSec: pause1Cumulative(isis))
    let longIdx = train.isiSec.firstIndex { ($0 ?? 0) > 1.0 } ?? -1
    return (train, longIdx)
}

@Test
func tonicBaselineISIsAreNotCompletedAsPause() throws {
    let fixture = pause1SyntheticTrain()
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [fixture.train])
    let band = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: band,
        manualThresholdProfile: .automatic
    )
    let candidates = run.result(for: fixture.train.id)?.candidates ?? []

    // The monotonic floor-completion layer must not generate pause candidates over the leading tonic
    // baseline (ISI 1...6, ~0.45 s): those ISIs exceed the calibrated floor (~0.28 s) but are not
    // local/background pause outliers (they ARE the baseline).
    let leadingBaselineCompletions = candidates.filter {
        $0.candidateLayer == "train_pause_floor_completion" && ($0.startISIIndex ?? .max) <= 6
    }
    #expect(leadingBaselineCompletions.isEmpty)

    // A genuine long pause (1.30 s, ~3x baseline) must still be labeled pause.
    let truePausePreserved = candidates.contains {
        $0.finalLabel == .pause && $0.selectedForAuto &&
        ($0.startISIIndex ?? -1) <= fixture.longPauseISIIndex &&
        ($0.endISIIndex ?? -1) >= fixture.longPauseISIIndex
    }
    #expect(truePausePreserved)
}
