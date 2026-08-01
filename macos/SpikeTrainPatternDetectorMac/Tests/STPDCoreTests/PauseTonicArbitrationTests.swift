import Foundation
import STPDCore
import Testing

// PAUSE-3: a clean, regular tonic baseline that merely sits at a longer ISI than a neighboring fast burst-like
// cluster must NOT be labeled pause. A preceding fast burst drives the local/global median low, so the first
// tonic ISI after it is a relative outlier — but a steady tonic baseline does not satisfy the actual pause
// criteria (it is regular, low-LV, and within the tonic band), so the anti-tonic veto / relative+global guards
// must keep it out of the gap (pause) track. This reproduces the "tonic-like early ISIs labeled pause" report
// without loosening any threshold.

private let bandSettings = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)

private func run(_ train: SpikeTrain) -> ClassicAnchorDetectionRun {
    ClassicAnchorDetectionPipeline.run(
        dataset: SpikeDataset(name: "pause-tonic arbitration", sourceDescription: "unit-test", trains: [train]),
        bandSettings: bandSettings
    )
}

/// A fast 8 ms burst (ISI indices 1...12) immediately followed by a steady 60 ms tonic baseline (ISI indices
/// 13...28). The 8 ms run pulls the global/local baseline well below 60 ms, so the transition ISI is a relative
/// outlier, but the 60 ms region is a clean regular baseline.
private func burstThenTonicTrain() -> SpikeTrain {
    var ts: [Double] = [0]
    for _ in 0..<12 { ts.append(ts.last! + 0.008) }   // fast burst       (ISI 1...12)
    for _ in 0..<16 { ts.append(ts.last! + 0.060) }   // steady tonic     (ISI 13...28)
    return SpikeTrain(name: "burst_then_tonic", timestampsSec: ts)
}

@Test
func steadyTonicBaselineAfterFastBurstIsNotLabeledPause() throws {
    let result = try #require(run(burstThenTonicTrain()).result(for: "burst_then_tonic"))
    let selectedPauses = result.candidates.filter { $0.selectedForAuto && $0.finalLabel == .pause }

    // No selected pause may cover any ISI in the steady 60 ms tonic region (indices 13...28) — including the
    // 8 ms -> 60 ms transition ISI (index 13), which is the relative-outlier trap.
    let pauseCoversTonicRegion = selectedPauses.contains { candidate in
        candidate.endISIIndex >= 13 && candidate.startISIIndex <= 28
    }
    #expect(!pauseCoversTonicRegion)
}

@Test
func steadyTonicBaselineAfterFastBurstProducesNoGapAnnotation() throws {
    let train = burstThenTonicTrain()
    let detectionRun = run(train)
    let dataset = SpikeDataset(name: "pause-tonic arbitration", sourceDescription: "unit-test", trains: [train])
    // The displayed annotations (gap track = pause) must not paint the steady tonic baseline as a pause.
    let gapAnnotations = detectionRun.eventAnnotations(in: dataset, tracks: [.gap])
    #expect(gapAnnotations.isEmpty)
}
