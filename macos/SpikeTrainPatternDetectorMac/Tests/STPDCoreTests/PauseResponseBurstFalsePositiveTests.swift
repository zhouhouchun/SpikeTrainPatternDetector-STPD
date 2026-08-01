import Foundation
import STPDCore
import Testing

// MARK: - ALGO-RETUNE Phase 11: the tonic-like ~80 ms packet in pause_response_2_s must NOT be a Burst.
//
// DESIRED-BEHAVIOR regression test (the target of the Phase 11 fix). A brief, perfectly regular 4-ISI
// 0.080 s packet (12.5 Hz, tonic-rate) isolated in a 2.0 s silent baseline is currently labeled a Burst
// purely on edge contrast against the silence. A classic burst is a transient HIGH-FREQUENCY discharge;
// this packet is tonic-rate, so it must not be selected as Burst (Other or tonic-like is acceptable).
// The fix must be local/adaptive — NOT a flat "80 ms is not burst" rule — and must leave user hard/soft
// burst gates untouched (the 90 ms triplet under hard seed max = 100 ms stays Burst; see Phase 10).

private func cumulative(_ isis: [Double]) -> [Double] {
    var t: [Double] = [0]
    for d in isis { t.append(t.last! + d) }
    return t
}

private let phase11BandSettings = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)

private let pauseResponse2S = SpikeTrain(
    name: "pause_response_2_s",
    timestampsSec: [0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 12.08, 12.16, 12.24, 12.32,
                    14.32, 16.32, 18.32, 20.32, 22.32, 24.32]
)

private func selectedBurstFamily(_ run: ClassicAnchorDetectionRun, trainID: String) -> [ClassicAnchorCandidate] {
    (run.result(for: trainID)?.candidates ?? []).filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
}

@Test
func pauseResponse80msPacketIsNotSelectedAsBurst() {
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [pauseResponse2S])
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset, bandSettings: phase11BandSettings, manualThresholdProfile: .automatic
    )
    // No selected burst-family candidate may cover the 0.080 s packet (ISI 7...10).
    let packetBursts = selectedBurstFamily(run, trainID: pauseResponse2S.id).filter {
        ($0.startISIIndex ?? .max) <= 10 && ($0.endISIIndex ?? .min) >= 7
    }
    #expect(packetBursts.isEmpty)
}

// MARK: - Phase 11A: user SOFT burst anchor must bypass the automatic-only tonic-rate guard.
//
// A SOFT burst seed/bridge anchor at this ISI scale expresses explicit user intent that ~0.08 s IS
// burst-relevant for this train. The Phase 11 guard is AUTOMATIC-ONLY, so — like a hard gate — a soft
// anchor must bypass it: the packet must stay a selected Burst. (Hard gates are covered by Phase 10's
// moderate-triplet test.)
@Test
func pauseResponse80msPacketStaysBurstUnderSoftBurstAnchor() {
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [pauseResponse2S])
    let soft = ManualThresholdProfile(
        burst: BurstManualThresholds(
            seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.085),
            bridgeUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.090)
        )
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset, bandSettings: phase11BandSettings, manualThresholdProfile: soft
    )
    let packetBursts = selectedBurstFamily(run, trainID: pauseResponse2S.id).filter {
        ($0.startISIIndex ?? .max) <= 10 && ($0.endISIIndex ?? .min) >= 7
    }
    #expect(!packetBursts.isEmpty)
}

// Preserve guard (self-contained): true high-frequency bursts in burst_response_2_s remain Burst under
// AUTOMATIC mode — the fix must not suppress genuine fast bursts (16-44 ms, irregular cv 0.18-0.42).
@Test
func phase11PreservesBurstResponseTrueBursts() {
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
    let train = SpikeTrain(name: "burst_response_2_s", timestampsSec: cumulative(isis))
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset, bandSettings: phase11BandSettings, manualThresholdProfile: .automatic
    )
    let bursts = selectedBurstFamily(run, trainID: train.id)
    // The eight compact bursts must survive.
    #expect(bursts.count >= 8)
    // The first compact cluster (ISI 12...13) and tightest cluster (ISI 31...34) are still bursts.
    #expect(bursts.contains { ($0.startISIIndex ?? .max) <= 12 && ($0.endISIIndex ?? .min) >= 13 })
    #expect(bursts.contains { ($0.startISIIndex ?? .max) <= 31 && ($0.endISIIndex ?? .min) >= 34 })
}
