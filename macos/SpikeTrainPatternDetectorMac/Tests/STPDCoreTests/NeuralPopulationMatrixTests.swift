import Foundation
import STPDCore
import Testing

// NM-1A — deterministic tests for the pure population activity matrix builder
// (`NeuralPopulationMatrixBuilder`), mirroring R `stpd_make_neural_population_matrix`.

private func makeTrain(_ name: String, _ times: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: times, duplicateTimestampPolicy: .errorKeep)
}

private func makeDataset(_ trains: [SpikeTrain]) -> SpikeDataset {
    SpikeDataset(name: "ds", sourceDescription: "test", trains: trains)
}

private func expectClose(_ actual: [Double], _ expected: [Double], tol: Double = 1e-9) {
    #expect(actual.count == expected.count)
    for (a, b) in zip(actual, expected) {
        #expect(abs(a - b) <= tol, "expected \(b), got \(a)")
    }
}

@Test
func binBoundariesClampFinalBinToWindowEnd() throws {
    // ceil(0.5 / 0.2) = 3 bins; the last bin [0.4, 0.5] is clamped (narrower).
    let ds = makeDataset([makeTrain("a", [0.0, 0.5]), makeTrain("b", [0.1, 0.45])])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: ds,
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.2, startSec: 0, endSec: 0.5,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 0, scaling: .none
        )
    )
    #expect(matrix.bins.count == 3)
    #expect(matrix.bins.map(\.binID) == [1, 2, 3])
    expectClose(matrix.bins.map(\.startSec), [0.0, 0.2, 0.4])
    expectClose(matrix.bins.map(\.endSec), [0.2, 0.4, 0.5])
    expectClose(matrix.bins.map(\.widthSec), [0.2, 0.2, 0.1])
    expectClose(matrix.bins.map(\.midSec), [0.1, 0.3, 0.45])
    #expect(matrix.windowStartSec == 0)
    #expect(matrix.windowEndSec == 0.5)
}

@Test
func spikeCountsAndRatesAcrossTrains() throws {
    let a = makeTrain("a", [0.05, 0.15, 0.30, 0.55, 0.95])
    let b = makeTrain("b", [0.10, 0.60, 0.61, 0.62])
    let ds = makeDataset([a, b])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: ds,
        selectedTrainIDs: ["a", "b"],
        parameters: NeuralPopulationParameters(
            binSec: 0.25, startSec: 0, endSec: 1.0,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 0, scaling: .none
        )
    )
    #expect(matrix.bins.count == 4)
    #expect(matrix.trainIDs == ["a", "b"])
    #expect(matrix.trainNames == ["a", "b"])
    // bins: [0,0.25) [0.25,0.5) [0.5,0.75) [0.75,1.0]
    #expect(matrix.counts[0] == [2, 1])
    #expect(matrix.counts[1] == [1, 0])
    #expect(matrix.counts[2] == [1, 3])
    #expect(matrix.counts[3] == [1, 0])
    // count transform + no scaling → signal and scaled both equal counts.
    #expect(matrix.signal == matrix.counts)
    #expect(matrix.scaled == matrix.counts)
    // rate = count / 0.25 = count * 4.
    expectClose(matrix.rates[0], [8.0, 4.0])
    expectClose(matrix.rates[2], [4.0, 12.0])
}

@Test
func lastBinInclusiveAndInteriorBoundariesLeftClosed() throws {
    // 0.5 sits on the interior boundary → next bin; 1.0 sits on the window end → last bin (inclusive).
    let ds = makeDataset([makeTrain("a", [0.5, 1.0])])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: ds,
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.5, startSec: 0, endSec: 1.0,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 0, scaling: .none
        )
    )
    #expect(matrix.bins.count == 2)          // [0,0.5) [0.5,1.0]
    #expect(matrix.trainCount == 1)          // single-train matrix is valid
    #expect(matrix.counts[0] == [0])         // 0.5 excluded from the first bin
    #expect(matrix.counts[1] == [2])         // 0.5 (>= 0.5) and 1.0 (<= 1.0, inclusive last)
}

@Test
func transformsCountRateSqrtAndLog1p() throws {
    let ds = makeDataset([makeTrain("a", [0.05, 0.15])])  // 2 spikes in one bin [0, 0.25]
    func build(_ transform: NeuralPopulationTransform) throws -> NeuralPopulationMatrix {
        try NeuralPopulationMatrixBuilder.build(
            dataset: ds,
            selectedTrainIDs: nil,
            parameters: NeuralPopulationParameters(
                binSec: 0.25, startSec: 0, endSec: 0.25,
                timeOrigin: .raw, transform: transform, smoothingSigmaBins: 0, scaling: .none
            )
        )
    }
    let counts = try build(.count)
    #expect(counts.bins.count == 1)
    #expect(counts.counts[0] == [2])
    #expect(counts.signal[0] == [2])

    let rate = try build(.rate)
    expectClose(rate.signal[0], [8.0])                        // 2 / 0.25

    let sqrtCount = try build(.sqrtCount)
    expectClose(sqrtCount.signal[0], [(2.0 + 0.375).squareRoot()])  // sqrt(count + 3/8)

    let log1pRate = try build(.log1pRate)
    expectClose(log1pRate.signal[0], [log(9.0)])              // log1p(8) = ln(9)
}

@Test
func gaussianSmoothingSpreadsImpulseSymmetrically() throws {
    // One in-window spike (bin 2) + one out-of-window spike (keeps the train valid with >= 2 spikes).
    let ds = makeDataset([makeTrain("a", [0.5, 2.0])])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: ds,
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.2, startSec: 0, endSec: 1.0,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 1, scaling: .none
        )
    )
    #expect(matrix.bins.count == 5)
    // Raw counts are an unsmoothed impulse; the out-of-window spike at 2.0 is not counted.
    #expect(matrix.counts.map { $0[0] } == [0, 0, 1, 0, 0])
    let column = matrix.signal.map { $0[0] }
    #expect(column.allSatisfy { $0 > 0 })          // kernel spreads mass to every bin
    #expect(column[2] > column[1])                 // peak stays at the impulse bin
    #expect(column[1] > column[0])                 // decays away from the center
    #expect(column[2] < 1.0)                        // peak is reduced by spreading
    expectClose([column[1]], [column[3]])           // symmetric about the center
    expectClose([column[0]], [column[4]])
}

@Test
func gaussianSmoothingLeavesConstantColumnUnchanged() throws {
    // Every bin holds exactly two spikes → constant count column; a normalized kernel is identity on it.
    let a = makeTrain("a", [0.05, 0.06, 0.25, 0.26, 0.45, 0.46, 0.65, 0.66, 0.85, 0.86])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: makeDataset([a]),
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.2, startSec: 0, endSec: 1.0,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 2, scaling: .none
        )
    )
    #expect(matrix.counts.map { $0[0] } == [2, 2, 2, 2, 2])
    expectClose(matrix.signal.map { $0[0] }, [2, 2, 2, 2, 2])
}

@Test
func alignedTimeOriginShiftsEachTrainToItsOwnStart() throws {
    // Raw timestamps offset by +100 s; aligned should behave like [0.0, 0.10, 0.25].
    let ds = makeDataset([makeTrain("a", [100.05, 100.15, 100.30])])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: ds,
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.25, startSec: nil, endSec: nil,
            timeOrigin: .aligned, transform: .count, smoothingSigmaBins: 0, scaling: .none
        )
    )
    #expect(matrix.windowStartSec == 0)            // aligned earliest spike is 0
    expectClose([matrix.windowEndSec], [0.25])     // aligned latest spike is 0.25
    #expect(matrix.bins.count == 1)                // ceil(0.25 / 0.25) = 1, [0, 0.25] inclusive
    #expect(matrix.counts[0] == [3])               // all three aligned spikes counted
}

@Test
func zscoreScalingReusesRCompatibleScalerAndNoneReturnsSignal() throws {
    let a = makeTrain("a", [0.05, 0.30, 0.31, 0.55, 0.80, 0.95])
    let b = makeTrain("b", [0.10, 0.12, 0.40, 0.70, 0.72, 0.99])
    let ds = makeDataset([a, b])
    let base = NeuralPopulationParameters(
        binSec: 0.2, startSec: 0, endSec: 1.0,
        timeOrigin: .raw, transform: .count, smoothingSigmaBins: 0, scaling: .none
    )

    let none = try NeuralPopulationMatrixBuilder.build(dataset: ds, selectedTrainIDs: nil, parameters: base)
    #expect(none.scaled == none.signal)            // none → median-filled signal (already finite)

    var zParams = base
    zParams.scaling = .zscore
    let zscore = try NeuralPopulationMatrixBuilder.build(dataset: ds, selectedTrainIDs: nil, parameters: zParams)

    // z-score (sample SD) centers each column mean to 0.
    for j in 0..<zscore.trainCount {
        let columnMean = (0..<zscore.binCount).reduce(0.0) { $0 + zscore.scaled[$1][j] } / Double(zscore.binCount)
        #expect(abs(columnMean) < 1e-9)
    }
    // Reuse check: scaled equals the shared R-compatible scaler applied directly to the signal.
    let direct = ISIStateSpacePCA.scaleMatrixForPCA(zscore.signal, scaling: .zscore)
    for bin in 0..<zscore.binCount { expectClose(zscore.scaled[bin], direct[bin]) }
}

@Test
func robustScalingMatchesSharedScaler() throws {
    let a = makeTrain("a", [0.05, 0.30, 0.31, 0.55, 0.80, 0.95])
    let b = makeTrain("b", [0.10, 0.12, 0.40, 0.70, 0.72, 0.99])
    let ds = makeDataset([a, b])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: ds,
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.2, startSec: 0, endSec: 1.0,
            timeOrigin: .raw, transform: .rate, smoothingSigmaBins: 0, scaling: .robust
        )
    )
    let direct = ISIStateSpacePCA.scaleMatrixForPCA(matrix.signal, scaling: .robust)
    for bin in 0..<matrix.binCount { expectClose(matrix.scaled[bin], direct[bin]) }
}

@Test
func selectionOrderingDropsUnknownIDsAndPreservesRequestOrder() throws {
    let a = makeTrain("a", [0.1, 0.2, 0.3])
    let b = makeTrain("b", [0.15, 0.25, 0.35])
    let c = makeTrain("c", [0.05, 0.45, 0.5])
    let ds = makeDataset([a, b, c])
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: ds,
        selectedTrainIDs: ["b", "zzz", "a"],    // unknown "zzz" ignored, order preserved
        parameters: NeuralPopulationParameters(
            binSec: 0.5, startSec: 0, endSec: 0.5,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 0, scaling: .none
        )
    )
    #expect(matrix.trainIDs == ["b", "a"])
}

@Test
func guardsThrowForEmptySelectionAndNoValidTrains() throws {
    let ds = makeDataset([makeTrain("a", [0.1, 0.2, 0.3]), makeTrain("b", [0.15, 0.25, 0.35])])
    #expect(throws: NeuralPopulationMatrixError.noTrainsSelected) {
        _ = try NeuralPopulationMatrixBuilder.build(dataset: ds, selectedTrainIDs: [])
    }
    // Every selected train has only a single spike → dropped → no valid trains.
    let thin = makeDataset([makeTrain("x", [0.1]), makeTrain("y", [0.2])])
    #expect(throws: NeuralPopulationMatrixError.noValidTrains) {
        _ = try NeuralPopulationMatrixBuilder.build(dataset: thin, selectedTrainIDs: nil)
    }
}
