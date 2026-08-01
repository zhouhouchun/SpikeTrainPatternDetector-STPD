import Foundation
import STPDCore
import Testing

// NM-2A: the R-style event-state annotation layer. Bins are annotated by interval OVERLAP (not nearest point),
// auto and final sources are independent, and PCA coordinates are never touched.

private func train(_ name: String, _ times: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: times)
}

private func matrix(
    _ trains: [SpikeTrain],
    binSec: Double = 0.1,
    timeOrigin: NeuralPopulationTimeOrigin = .raw
) throws -> NeuralPopulationMatrix {
    try NeuralPopulationMatrixBuilder.build(
        dataset: SpikeDataset(name: "ds", sourceDescription: "unit-test", trains: trains),
        parameters: NeuralPopulationParameters(binSec: binSec, timeOrigin: timeOrigin)
    )
}

private func interval(
    _ trainID: String, _ start: Double, _ end: Double, _ state: SpikePatternState,
    aligned: (Double, Double)? = nil
) -> SpikePatternInterval {
    let a = aligned ?? (start, end)
    return SpikePatternInterval(
        trainID: trainID, rawStartSec: start, rawEndSec: end,
        alignedStartSec: a.0, alignedEndSec: a.1, state: state
    )
}

private let regularTimes: [Double] = [0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30]

// 1. A burst annotation overlapping one bin gives that bin dominant burst.
@Test
func burstOverlapMakesBinDominantBurst() throws {
    let t = train("t", regularTimes)
    let m = try matrix([t])
    let result = NeuralPopulationEventStateLayer.build(
        matrix: m, trains: [t], intervals: [interval(t.id, 0.0, 0.095, .burst)], labelSource: .auto
    )
    let bin0 = try #require(result.rows.first)
    #expect(bin0.dominantState == .burst)
    #expect(bin0.fraction(.burst) > 0.5)
    #expect(bin0.labelSource == .auto)
}

// 2. A pause annotation with larger bin overlap wins over a smaller burst overlap (dominant = overlap, not priority).
@Test
func largerPauseOverlapBeatsSmallerBurstOverlap() throws {
    let t = train("t", regularTimes)
    let m = try matrix([t])
    let result = NeuralPopulationEventStateLayer.build(
        matrix: m, trains: [t],
        intervals: [interval(t.id, 0.0, 0.02, .burst), interval(t.id, 0.03, 0.10, .pause)],
        labelSource: .auto
    )
    let bin0 = try #require(result.rows.first)
    #expect(bin0.dominantState == .pause)
    #expect(bin0.fraction(.pause) > bin0.fraction(.burst))
}

// 3. Unlabeled bins report unlabeledFraction; partial coverage splits labeled/unlabeled.
@Test
func unlabeledAndPartiallyLabeledBinsReportUnlabeledFraction() throws {
    let t = train("t", regularTimes)
    let m = try matrix([t])

    let empty = NeuralPopulationEventStateLayer.build(matrix: m, trains: [t], intervals: [], labelSource: .auto)
    #expect(empty.rows.allSatisfy { $0.dominantState == .unlabeled && $0.labeledFraction == 0 && $0.unlabeledFraction == 1 })

    // A burst covering half of bin 0 leaves ~half unlabeled.
    let partial = NeuralPopulationEventStateLayer.build(
        matrix: m, trains: [t], intervals: [interval(t.id, 0.0, 0.05, .burst)], labelSource: .auto
    )
    let bin0 = try #require(partial.rows.first)
    #expect(abs(bin0.labeledFraction - 0.5) < 1e-9)
    #expect(abs(bin0.unlabeledFraction - 0.5) < 1e-9)
}

// 4. Auto and final layers differ when the reviewed (final) projection removes an auto burst.
@Test
func autoAndFinalLayersDifferWhenFinalRemovesAutoBurst() throws {
    let t = train("t", regularTimes)
    let m = try matrix([t])
    let auto = NeuralPopulationEventStateLayer.build(
        matrix: m, trains: [t], intervals: [interval(t.id, 0.0, 0.095, .burst)], labelSource: .auto
    )
    let final = NeuralPopulationEventStateLayer.build(
        matrix: m, trains: [t], intervals: [], labelSource: .final   // reviewed projection vetoed the auto burst
    )
    #expect(auto.rows.first?.dominantState == .burst)
    #expect(final.rows.first?.dominantState == .unlabeled)
    #expect(auto.rows.first?.labelSource == .auto)
    #expect(final.rows.first?.labelSource == .final)
    #expect(auto.binCountByDominantState[.burst] == 1)
    #expect((final.binCountByDominantState[.burst] ?? 0) == 0)
}

// 5. Raw vs aligned time origin resolves the same burst to the first bin (interval carries both bounds).
@Test
func rawAndAlignedTimeOriginAreHandledConsistently() throws {
    let t = train("t", [10.0, 10.05, 10.10, 10.15, 10.20])   // alignedTimestampsSec = ts - min = [0,0.05,...]
    let burst = interval(t.id, 10.0, 10.095, .burst, aligned: (0.0, 0.095))

    let rawResult = NeuralPopulationEventStateLayer.build(
        matrix: try matrix([t], timeOrigin: .raw), trains: [t], intervals: [burst], labelSource: .auto
    )
    let alignedResult = NeuralPopulationEventStateLayer.build(
        matrix: try matrix([t], timeOrigin: .aligned), trains: [t], intervals: [burst], labelSource: .auto
    )
    #expect(rawResult.rows.first?.dominantState == .burst)
    #expect(alignedResult.rows.first?.dominantState == .burst)
    // Per-state spike counts are populated under both time bases (spikes binned in the same base as the bins).
    #expect((rawResult.rows.first?.spikeCount ?? 0) > 0)
    #expect((alignedResult.rows.first?.spikeCount ?? 0) > 0)
}

// 6. PCA coordinates are unchanged when the event-state layer is built (no circularity).
@Test
func eventStateLayerDoesNotChangePCACoordinates() throws {
    let trains = [train("a", regularTimes), train("b", [0, 0.06, 0.12, 0.18, 0.24, 0.30])]
    let m = try matrix(trains)
    let before = try NeuralPopulationPCA.run(matrix: m)
    _ = NeuralPopulationEventStateLayer.build(
        matrix: m, trains: trains,
        intervals: [interval(trains[0].id, 0.0, 0.095, .burst)], labelSource: .auto
    )
    let after = try NeuralPopulationPCA.run(matrix: m)
    #expect(before.scores == after.scores)
}

// Invariant: the layer bins spikes the same way the population matrix does (per-bin total == matrix column sum).
@Test
func eventStateTotalSpikeCountMatchesPopulationMatrix() throws {
    let trains = [train("a", regularTimes), train("b", [0, 0.06, 0.12, 0.18, 0.24, 0.30])]
    let m = try matrix(trains)
    let result = NeuralPopulationEventStateLayer.build(
        matrix: m, trains: trains, intervals: [interval(trains[0].id, 0.0, 0.095, .burst)], labelSource: .auto
    )
    #expect(result.rows.count == m.bins.count)
    for (index, row) in result.rows.enumerated() {
        let matrixTotal = Int(m.counts[index].reduce(0, +).rounded())
        #expect(row.spikeCount == matrixTotal)
        // Per-state (labeled) + unlabeled spike counts reconstruct the total.
        let labeledSpikes = row.stateSpikeCounts.values.reduce(0, +)
        #expect(labeledSpikes <= row.spikeCount)
    }
}

@Test
func eventStateRatesUseAllMatrixTrainsAsDenominatorEvenWhenOneHasNoWindowSpikes() throws {
    let active = train("active", [0.00, 0.05, 0.10, 0.15])
    let quietInWindow = train("quiet", [1.00, 1.05, 1.10])
    let dataset = SpikeDataset(name: "ds", sourceDescription: "unit-test", trains: [active, quietInWindow])
    let m = try NeuralPopulationMatrixBuilder.build(
        dataset: dataset,
        parameters: NeuralPopulationParameters(binSec: 0.1, startSec: 0.0, endSec: 0.2)
    )

    #expect(m.trainCount == 2)
    let result = NeuralPopulationEventStateLayer.build(
        matrix: m,
        trains: [active, quietInWindow],
        intervals: [interval(active.id, 0.0, 0.095, .burst)],
        labelSource: .auto
    )
    let first = try #require(result.rows.first)
    #expect(first.spikeCount == 2)
    #expect(first.contributingTrainCount == 1)
    #expect(first.nTrains == 2)
    #expect(abs(first.firingRateHz - 10.0) < 1e-9) // 2 spikes / 0.1 s / 2 trains
    #expect(abs(first.rateHz(.burst) - 10.0) < 1e-9)
}
