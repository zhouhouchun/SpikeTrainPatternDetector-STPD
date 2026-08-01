import Foundation
@testable import STPDCore
import Testing

// SIM-1B — tests for the pure, deterministic STPDCore spike-train simulator primitive. No document/detection state is
// involved (RasterDocument is in the app target and is never referenced). Parity with the R reference is distributional
// (moments/ranges), NOT byte-exact RNG reproduction; regularity metrics are evaluation outputs via STPDStatistics.

private func isis(_ train: SpikeTrain) -> [Double] { train.isiSec.compactMap { $0 } }

private func detectorSelectedLabels(_ train: SpikeTrain) -> Set<String> {
    let dataset = SpikeDataset(name: "sim", sourceDescription: "sim", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: dataset)
    let candidates = run.result(for: train.id)?.candidates ?? []
    return Set(candidates.filter { $0.selectedForAuto }.map { $0.finalLabel.rawValue })
}

// MARK: - determinism

@Test func sim_sameSeedProducesIdenticalISISequence() {
    var a = SpikeTrainSimulator(seed: 20260624)
    var b = SpikeTrainSimulator(seed: 20260624)
    let ta = a.tonicTrain(durationSec: 60)
    let tb = b.tonicTrain(durationSec: 60)
    #expect(ta.timestampsSec == tb.timestampsSec)
    #expect(isis(ta) == isis(tb))
    #expect(ta.timestampsSec.count > 5)
}

@Test func sim_differentSeedProducesDifferentSequence() {
    var a = SpikeTrainSimulator(seed: 1)
    var b = SpikeTrainSimulator(seed: 2)
    let ta = a.tonicTrain(durationSec: 60)
    let tb = b.tonicTrain(durationSec: 60)
    #expect(ta.timestampsSec != tb.timestampsSec)
}

// MARK: - truncation + moments

@Test func sim_allTonicISIsWithinTruncationRange() {
    var s = SpikeTrainSimulator(seed: 7)
    let train = s.tonicTrain(durationSec: 600, lowerSec: 0.38, upperSec: 0.52)
    let values = isis(train)
    #expect(values.count > 1000)
    #expect(values.allSatisfy { $0 >= 0.38 && $0 <= 0.52 })
}

@Test func sim_tonicMeanAndSDAreCloseToDefaults() {
    var s = SpikeTrainSimulator(seed: 99)
    let values = isis(s.tonicTrain(durationSec: 1200, meanSec: 0.45, sdSec: 0.03, lowerSec: 0.38, upperSec: 0.52))
    #expect(values.count > 2000)
    let mean = STPDStatistics.mean(values)!
    let sd = STPDStatistics.coefficientOfVariation(values)! * mean   // reuse the same sample-SD path
    #expect(mean > 0.445 && mean < 0.455)        // symmetric truncation leaves the mean ≈ 0.45
    #expect(sd > 0.024 && sd < 0.032)            // truncation at ±2.3σ slightly shrinks SD from 0.03
}

// MARK: - regularity metrics use the existing STPDStatistics (not duplicated)

@Test func sim_regularityMetricsComputedByStpdStatistics() {
    var s = SpikeTrainSimulator(seed: 3)
    let values = isis(s.tonicTrain(durationSec: 600))
    let cv = STPDStatistics.coefficientOfVariation(values)
    let cv2 = STPDStatistics.coefficientOfVariation2(values)
    let lv = STPDStatistics.localVariation(values)
    // A tight tonic train ⇒ low, finite regularity metrics (sd/mean ≈ 0.028/0.45 ≈ 0.06).
    #expect((cv ?? -1) > 0.03 && (cv ?? 1) < 0.12)
    #expect((cv2 ?? -1) >= 0 && (cv2 ?? 1) < 0.12)
    #expect((lv ?? -1) >= 0 && (lv ?? 1) < 0.12)
}

// MARK: - detector verdict on simulated examples

@Test func sim_lowJitterTonicTrainIsLabeledTonic() {
    var s = SpikeTrainSimulator(seed: 42)
    let train = s.tonicTrain(durationSec: 30, sdSec: 0.03, lowerSec: 0.38, upperSec: 0.52)
    let cv = STPDStatistics.coefficientOfVariation(isis(train))!
    // Passes the whole-train tonic regularity gate AND the detector labels it (cleanly) tonic.
    #expect(cv <= StatePatternDetectorSettings().tonicCVMax)
    let selected = detectorSelectedLabels(train)
    #expect(selected.contains("tonic"))
    #expect(selected == ["tonic"])               // a clean tonic train is labelled ONLY tonic
}

@Test func sim_highJitterTonicTrainFailsTonicRegularityGate() {
    var s = SpikeTrainSimulator(seed: 42)
    // Widen both the SD and the truncation window so the train is genuinely irregular.
    let train = s.tonicTrain(durationSec: 30, sdSec: 0.30, lowerSec: 0.05, upperSec: 2.0)
    let cv = STPDStatistics.coefficientOfVariation(isis(train))!
    // The whole-train CV exceeds the detector's tonic regularity gate (a whole-train tonic acceptance would fail)…
    #expect(cv > StatePatternDetectorSettings().tonicCVMax)
    // …and the train is NOT labelled cleanly tonic (unlike the low-jitter case it is not purely tonic).
    let selected = detectorSelectedLabels(train)
    #expect(selected != ["tonic"])
}

// MARK: - burst kernel helper

@Test func sim_burstKernelISIsAreInRangeAndDeterministic() {
    var a = SpikeTrainSimulator(seed: 5)
    var b = SpikeTrainSimulator(seed: 5)
    let ka = a.burstISIs(count: 200)
    let kb = b.burstISIs(count: 200)
    #expect(ka == kb)                                       // deterministic
    #expect(ka.count == 200)
    #expect(ka.allSatisfy { $0 >= 0.006 && $0 <= 0.045 })  // truncated Gamma range
    let mean = STPDStatistics.mean(ka)!
    #expect(mean > 0.012 && mean < 0.036)                  // ≈ shape·scale = 0.024, shrunk by truncation
    #expect(a.burstISIs(count: 0).isEmpty)
}

// MARK: - SIM-2B burst-response generator (tonic baseline + burst packets, reusing the burst kernel)

private let p2bOnsets: [Double] = [5, 10, 15, 20, 25]

private func burstResponse(seed: UInt64, spikes: ClosedRange<Int> = 3...6) -> SpikeTrain {
    var s = SpikeTrainSimulator(seed: seed)
    return s.burstResponseTrain(durationSec: 30, burstOnsetsSec: p2bOnsets, spikesPerBurst: spikes)
}

@Test func sim_burstResponse_sameSeedIdentical() {
    #expect(burstResponse(seed: 11).timestampsSec == burstResponse(seed: 11).timestampsSec)
}

@Test func sim_burstResponse_differentSeedDiffers() {
    #expect(burstResponse(seed: 11).timestampsSec != burstResponse(seed: 12).timestampsSec)
}

@Test func sim_burstResponse_timestampsSortedFiniteStrictlyIncreasing() {
    let ts = burstResponse(seed: 7).timestampsSec
    #expect(ts.count > 50)
    #expect(ts.allSatisfy { $0.isFinite })
    #expect(ts == ts.sorted())
    #expect(zip(ts, ts.dropFirst()).allSatisfy { $1 > $0 })
}

@Test func sim_burstResponse_packetsAtEachOnsetWithKernelISIs() throws {
    var s = SpikeTrainSimulator(seed: 7)
    let train = s.burstResponseTrain(durationSec: 30, burstOnsetsSec: p2bOnsets, spikesPerBurst: 5...5)
    let ts = train.timestampsSec
    for onset in p2bOnsets {
        let startIdx = try #require(ts.firstIndex { $0 >= onset - 1e-9 })
        #expect(abs(ts[startIdx] - onset) < 0.02)                 // the packet starts at its onset
        for k in 0..<4 {                                          // 5 spikes ⇒ 4 intra-burst ISIs in the kernel range
            let isi = ts[startIdx + 1 + k] - ts[startIdx + k]
            #expect(isi >= 0.006 && isi <= 0.045)
        }
    }
}

@Test func sim_burstResponse_packetSpikeCountsWithinRequestedRange() throws {
    var s = SpikeTrainSimulator(seed: 99)
    let train = s.burstResponseTrain(durationSec: 30, burstOnsetsSec: p2bOnsets, spikesPerBurst: 3...6)
    let ts = train.timestampsSec
    for onset in p2bOnsets {
        let startIdx = try #require(ts.firstIndex { $0 >= onset - 1e-9 })
        var k = 0
        while startIdx + 1 + k < ts.count, ts[startIdx + 1 + k] - ts[startIdx + k] <= 0.045 { k += 1 }
        #expect((3...6).contains(k + 1))                          // packet spike count in the requested range
    }
}

@Test func sim_burstResponse_baselineAndBurstISIsAreSeparatedBands() {
    let isiValues = burstResponse(seed: 7, spikes: 5...5).isiSec.compactMap { $0 }
    let baseline = isiValues.filter { $0 >= 0.38 }
    let burst = isiValues.filter { $0 <= 0.045 }
    #expect(baseline.count > 30)                                  // most ISIs are tonic baseline
    #expect(baseline.allSatisfy { $0 <= 0.52 })                  // baseline within its truncation window
    #expect(burst.count >= 4 * p2bOnsets.count)                  // ≥ 5 packets × 4 intra-burst ISIs
    #expect(burst.allSatisfy { $0 >= 0.006 })                    // burst ISIs within the kernel range
    #expect((baseline.min() ?? 1) > (burst.max() ?? 0))         // the two ISI bands are disjoint
}

@Test func sim_burstResponse_isSelfContainedNoDetectorOrDocumentState() {
    // Pure generator: no detector run / document / manual thresholds / export — two independent generations match, and
    // the output is a plain synthetic SpikeTrain (no labels). (Detector labelling is evaluated downstream, not here.)
    func gen() -> SpikeTrain {
        var s = SpikeTrainSimulator(seed: 2026)
        return s.burstResponseTrain(durationSec: 20, burstOnsetsSec: [4, 8, 12], spikesPerBurst: 4...4)
    }
    #expect(gen().timestampsSec == gen().timestampsSec)
    #expect(gen().name == "sim_burst_response")
}

// MARK: - isolation (no document / shared state)

@Test func sim_generateAndDetectIsSelfContainedAndReproducible() {
    // The whole generate → detect path is a pure function of the seed (no document, no shared mutable state): two
    // independent runs produce identical trains and identical detector verdicts.
    func generateAndDetect() -> ([Double], Set<String>) {
        var s = SpikeTrainSimulator(seed: 2026)
        let train = s.tonicTrain(durationSec: 30)
        return (train.timestampsSec, detectorSelectedLabels(train))
    }
    let first = generateAndDetect()
    let second = generateAndDetect()
    #expect(first.0 == second.0)
    #expect(first.1 == second.1)
}
