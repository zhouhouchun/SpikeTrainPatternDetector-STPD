import Foundation
import STPDCore
import Testing

// MARK: - PARAM-4 / PAUSE-2: real burst_response_2_s under Hard Burst seed max = 100 ms.
//
// These lock in the real-dataset behavior the dist build regressed on (the dist build predated the
// PARAM-3 hard-threshold bridge contraction and the PAUSE-1 pause-completion relative-background guard).
// The fixture is the verbatim ISI sequence of burst_response_2_s (tonic ~0.40-0.50 s baseline, short
// pre-burst transition ISIs ~0.16-0.29 s, and eight compact bursts — one as tight as 6-16 ms). Each test
// would have failed on the pre-fix build and passes now; together they guard against re-regression.

private func burstResponse2LikeTrain() -> SpikeTrain {
    // Verbatim ISI sequence of the example dataset's burst_response_2_s (99 ISIs / 100 spikes).
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
    var ts: [Double] = [0]
    for d in isis { ts.append(ts.last! + d) }
    return SpikeTrain(name: "burst_response_2_like", timestampsSec: ts)
}

private func hardSeedMaxRun(_ seedMaxSec: Double = 0.100) -> [ClassicAnchorCandidate] {
    let train = burstResponse2LikeTrain()
    let dataset = SpikeDataset(name: "d", sourceDescription: "s", trains: [train])
    let band = TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    let hard = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: seedMaxSec))
    )
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset, bandSettings: band, manualThresholdProfile: hard)
    return run.result(for: train.id)?.candidates ?? []
}

// 1. The compact cluster ISI[31...34] (= 12.1, 6.3, 16.2, 10.6 ms — all under the 100 ms hard seed max)
//    must be a selected burst tight to the cluster, not left as "others".
@Test
func moderateCompactClusterUnderHardSeedMaxIsSelectedBurstNotOthers() throws {
    let candidates = hardSeedMaxRun()
    let clusterBurst = candidates.contains {
        $0.finalLabel.isBurstEventFamily && $0.selectedForAuto &&
        ($0.startISIIndex ?? .max) <= 31 && ($0.endISIIndex ?? .min) >= 34 &&
        ($0.maxIntraISISec ?? .infinity) <= 0.100 + 1e-9
    }
    #expect(clusterBurst)
}

// 2. The leading tonic baseline ISIs (ISI 1, 2 — ~0.43 / 0.50 s, indistinguishable from the rest of the
//    ~0.45 s baseline) must NOT be selected as pause under Hard mode.
@Test
func tonicBaselineLeadingISIsAreNotPauseUnderHardSeedMax() throws {
    let candidates = hardSeedMaxRun()
    let leadingPause = candidates.filter {
        $0.finalLabel == .pause && $0.selectedForAuto && ($0.startISIIndex ?? .max) <= 2
    }
    #expect(leadingPause.isEmpty)
}

// 3. The hard Burst seed max must be enforced by EVERY burst route (none silently ignores it): no selected
//    burst-family candidate may cover an ISI above the seed max, and the over-extended adaptive HF packet
//    that bridges the 236 ms pre-burst transition (ISI 30) into the cluster must be demoted, not selected.
//    This is the sparse / over-band false-burst guard, verified on the real sequence.
@Test
func hardSeedMaxEnforcedAcrossAllBurstRoutesNoOverBandSelectedBurst() throws {
    let candidates = hardSeedMaxRun()
    let selectedBursts = candidates.filter { $0.finalLabel.isBurstEventFamily && $0.selectedForAuto }

    // At least the eight compact clusters are recovered as bursts (sanity: the hard gate did not suppress
    // legitimate compact bursts).
    #expect(selectedBursts.count >= 8)

    // No selected burst covers an ISI above the hard seed max.
    let overSeed = selectedBursts.filter { ($0.maxIntraISISec ?? 0) > 0.100 + 1e-9 }
    #expect(overSeed.isEmpty)

    // The 236 ms transition ISI (index 30) must not be swallowed into a selected burst.
    let burstCoversTransition = selectedBursts.contains {
        ($0.startISIIndex ?? .max) <= 30 && ($0.endISIIndex ?? .min) >= 30
    }
    #expect(!burstCoversTransition)
}
