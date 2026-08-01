import Foundation
import STPDCore
import Testing

// NM-1B — deterministic tests for the pure population PCA embedding layer
// (`NeuralPopulationPCA.run(matrix:)`), mirroring the PCA branch of R
// `stpd_run_neural_manifold_embedding`. Fixtures are built through the NM-1A matrix builder.

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

/// Three varying, non-collinear count columns over five 0.2 s bins on [0, 1.0]:
///   a = [3,0,1,0,1], b = [0,2,0,3,0], c = [2,0,1,0,2].
private func threeTrainDataset() -> SpikeDataset {
    makeDataset([
        makeTrain("a", [0.05, 0.06, 0.07, 0.45, 0.85]),
        makeTrain("b", [0.25, 0.26, 0.65, 0.66, 0.67]),
        makeTrain("c", [0.10, 0.11, 0.50, 0.90, 0.91]),
    ])
}

private func fiveBinParameters(
    transform: NeuralPopulationTransform = .count,
    scaling: NeuralPopulationScaling
) -> NeuralPopulationParameters {
    NeuralPopulationParameters(
        binSec: 0.2, startSec: 0, endSec: 1.0,
        timeOrigin: .raw, transform: transform, smoothingSigmaBins: 0, scaling: scaling
    )
}

@Test
func pcaProducesOneScorePerBinPreservingOrderAndContext() throws {
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: threeTrainDataset(), selectedTrainIDs: nil,
        parameters: fiveBinParameters(scaling: .zscore)
    )
    let result = try NeuralPopulationPCA.run(matrix: matrix)

    #expect(result.scores.count == matrix.binCount)
    #expect(result.binCount == matrix.binCount)
    #expect(result.trainCount == matrix.trainCount)
    #expect(result.method == "pca")
    #expect(result.methodLabel == "PCA")
    #expect(result.note == "SVD of the scaled population activity matrix.")
    for (score, bin) in zip(result.scores, matrix.bins) {
        #expect(score.binID == bin.binID)
        #expect(score.startSec == bin.startSec)
        #expect(score.endSec == bin.endSec)
        #expect(score.midSec == bin.midSec)
        #expect(score.widthSec == bin.widthSec)
        #expect(score.nm1.isFinite)
        #expect(score.nm2.isFinite)
        #expect(score.nm3.isFinite)   // full-rank 3-train fixture → PC3 present
    }
}

@Test
func pcaUsesScaledMatrixNotRawCountsOrSignal() throws {
    // transform = count → signal == counts; scaling = zscore → scaled differs from both.
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: threeTrainDataset(), selectedTrainIDs: nil,
        parameters: fiveBinParameters(scaling: .zscore)
    )
    #expect(matrix.signal == matrix.counts)        // sanity: the fixture makes signal == counts
    #expect(matrix.scaled != matrix.counts)        // ...and scaled is genuinely different

    let result = try NeuralPopulationPCA.run(matrix: matrix)

    // Matches PCA run directly on the scaled matrix.
    let fromScaled = ISIStateSpacePCA.principalComponents(scaledMatrix: matrix.scaled)
    for i in 0..<matrix.binCount {
        expectClose([result.scores[i].nm1], [fromScaled.scores[i][0]])
        expectClose([result.scores[i].nm2], [fromScaled.scores[i][1]])
        expectClose([result.scores[i].nm3], [fromScaled.scores[i][2]])
    }

    // Differs from PCA run on the raw counts (proving the layer did not use counts/signal).
    let fromCounts = ISIStateSpacePCA.principalComponents(scaledMatrix: matrix.counts)
    var anyDifference = false
    for i in 0..<matrix.binCount where abs(result.scores[i].nm1 - fromCounts.scores[i][0]) > 1e-6 {
        anyDifference = true
    }
    #expect(anyDifference)
}

@Test
func pcaZeroFillsAbsentScoreComponentsAndNilFillsLoadings() throws {
    // Two varying trains → ncomp = min(nBins, 2) = 2 → PC3 is absent everywhere.
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: makeDataset([
            makeTrain("a", [0.05, 0.06, 0.07, 0.45, 0.85]),
            makeTrain("b", [0.25, 0.26, 0.65, 0.66, 0.67]),
        ]),
        selectedTrainIDs: nil,
        parameters: fiveBinParameters(scaling: .zscore)
    )
    let result = try NeuralPopulationPCA.run(matrix: matrix)

    #expect(result.trainCount == 2)
    for score in result.scores {
        #expect(score.nm1.isFinite)
        #expect(score.nm2.isFinite)
        #expect(score.nm3 == 0)            // absent score component → 0 (R stpd_neural_take3)
    }
    for loading in result.loadings {
        #expect(loading.pc1 != nil)
        #expect(loading.pc2 != nil)
        #expect(loading.pc3 == nil)        // absent loading PC → nil (R NA-fill)
    }
    #expect(result.variance.count == 2)    // only PC1, PC2 exist
    #expect(result.variance.map(\.component) == ["PC1", "PC2"])
}

@Test
func pcaLoadingsMapToTrainIDsAndNamesInColumnOrder() throws {
    // Request a specific column order; loadings must follow matrix column order 1:1.
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: threeTrainDataset(),
        selectedTrainIDs: ["c", "a", "b"],
        parameters: fiveBinParameters(scaling: .zscore)
    )
    #expect(matrix.trainIDs == ["c", "a", "b"])

    let result = try NeuralPopulationPCA.run(matrix: matrix)
    #expect(result.loadings.count == matrix.trainCount)
    #expect(result.loadings.map(\.trainID) == matrix.trainIDs)
    #expect(result.loadings.map(\.trainName) == matrix.trainNames)
}

@Test
func pcaVarianceRatiosSumToApproximatelyOne() throws {
    // Full-rank 3-train fixture → ncomp = 3 → the three ratios partition the total variance.
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: threeTrainDataset(), selectedTrainIDs: nil,
        parameters: fiveBinParameters(scaling: .zscore)
    )
    let result = try NeuralPopulationPCA.run(matrix: matrix)

    #expect(result.variance.count == 3)
    let ratios = result.variance.compactMap(\.varianceExplained)
    #expect(ratios.count == 3)
    #expect(abs(ratios.reduce(0, +) - 1.0) < 1e-9)
    for ratio in ratios {
        #expect(ratio >= -1e-12)
        #expect(ratio <= 1.0 + 1e-9)
    }
    // Cumulative is monotonic non-decreasing and reaches ~1 at PC3.
    let cumulative = result.variance.compactMap(\.cumulative)
    #expect(cumulative.count == 3)
    #expect(cumulative[0] <= cumulative[1] + 1e-12)
    #expect(cumulative[1] <= cumulative[2] + 1e-12)
    #expect(abs((cumulative.last ?? 0) - 1.0) < 1e-9)
}

@Test
func pcaGuardsTooFewBinsAndTooFewTrains() throws {
    // Two bins (ceil(1.0 / 0.5) = 2) → too few bins.
    let twoBinMatrix = try NeuralPopulationMatrixBuilder.build(
        dataset: makeDataset([makeTrain("a", [0.1, 0.2, 0.6]), makeTrain("b", [0.15, 0.65, 0.66])]),
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.5, startSec: 0, endSec: 1.0,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 0, scaling: .none
        )
    )
    #expect(twoBinMatrix.binCount == 2)
    #expect(throws: NeuralPopulationPCAError.tooFewBins) {
        _ = try NeuralPopulationPCA.run(matrix: twoBinMatrix)
    }

    // Single train → one column → too few trains (NM-1A still builds a one-train matrix).
    let oneTrainMatrix = try NeuralPopulationMatrixBuilder.build(
        dataset: makeDataset([makeTrain("a", [0.05, 0.30, 0.55, 0.80])]),
        selectedTrainIDs: nil,
        parameters: fiveBinParameters(scaling: .none)
    )
    #expect(oneTrainMatrix.trainCount == 1)
    #expect(throws: NeuralPopulationPCAError.tooFewTrains) {
        _ = try NeuralPopulationPCA.run(matrix: oneTrainMatrix)
    }
}

@Test
func pcaGuardsTooFewVaryingTrains() throws {
    // Four 0.25 s bins on [0, 1.0]: one constant column ([1,1,1,1]) + one varying ([2,1,0,0]).
    let matrix = try NeuralPopulationMatrixBuilder.build(
        dataset: makeDataset([
            makeTrain("const", [0.05, 0.30, 0.55, 0.80]),   // one spike per bin → constant counts
            makeTrain("vary", [0.05, 0.10, 0.30]),          // [2,1,0,0] → varying
        ]),
        selectedTrainIDs: nil,
        parameters: NeuralPopulationParameters(
            binSec: 0.25, startSec: 0, endSec: 1.0,
            timeOrigin: .raw, transform: .count, smoothingSigmaBins: 0, scaling: .none
        )
    )
    #expect(matrix.binCount == 4)
    #expect(matrix.trainCount == 2)            // passes bin + train-count guards
    #expect(matrix.counts.map { $0[0] } == [1, 1, 1, 1])   // constant column confirmed
    #expect(throws: NeuralPopulationPCAError.tooFewVaryingTrains) {
        _ = try NeuralPopulationPCA.run(matrix: matrix)
    }
}
