import Foundation
import STPDCore
import Testing

// NM-3B: R-compatible Neural Manifold Isomap on `matrix.scaled`. Coordinates come only from the binned
// population activity — never from detector/event labels — and reuse the ISIStateSpaceIsomap primitives.

private func matrix(scaled: [[Double]]) -> NeuralPopulationMatrix {
    let nBins = scaled.count
    let nNeurons = scaled.first?.count ?? 0
    let bins = (0..<nBins).map { i in
        NeuralPopulationBin(
            binID: i + 1, startSec: Double(i) * 0.1, endSec: Double(i + 1) * 0.1,
            midSec: (Double(i) + 0.5) * 0.1, widthSec: 0.1
        )
    }
    let ids = (0..<nNeurons).map { "n\($0)" }
    return NeuralPopulationMatrix(
        bins: bins, trainIDs: ids, trainNames: ids,
        counts: scaled, rates: scaled, signal: scaled, scaled: scaled,
        transform: .sqrtCount, scaling: .zscore, timeOrigin: .raw,
        binSec: 0.1, smoothingSigmaBins: 0, windowStartSec: 0, windowEndSec: Double(nBins) * 0.1
    )
}

/// A gentle 1-D manifold (sine curve in the x–y plane; z constant) parameterized by bin order.
private func curve1D(_ count: Int = 10) -> [[Double]] {
    (0..<count).map { i in
        let t = Double(i)
        return [t, 0.2 * Foundation.sin(t * 0.6), 0.0]
    }
}

private func cluster(_ offset: Double) -> [[Double]] {
    [[offset, 0, 0], [offset + 0.1, 0.03, 0], [offset + 0.2, 0, 0], [offset + 0.3, 0.03, 0], [offset + 0.4, 0, 0]]
}

// MARK: - Guards

@Test
func neuralIsomapRejectsTooFewBins() {
    let m = matrix(scaled: [[0, 0], [1, 0.1], [2, 0], [3, 0.1]])   // 4 bins < 5
    #expect(throws: NeuralPopulationIsomapError.tooFewBins) {
        _ = try NeuralPopulationIsomap.run(matrix: m)
    }
}

@Test
func neuralIsomapRejectsTooFewNeuronColumns() {
    let scaled = (0..<6).map { i in [Double(i)] }
    #expect(throws: NeuralPopulationIsomapError.tooFewNeurons) {
        _ = try NeuralPopulationIsomap.run(matrix: matrix(scaled: scaled))
    }
}

@Test
func neuralIsomapKeepsConstantNeuronColumnsLikeR() throws {
    // R neural-manifold Isomap checks ncol(X) >= 2 and does not drop constant columns before computing
    // distances. Constant columns contribute 0 to Euclidean distance, so this one-varying + one-constant
    // fixture must still embed instead of failing with an ISI-state-space-style varying-feature guard.
    let scaled = (0..<6).map { i in [Double(i), 1.0] }
    let result = try NeuralPopulationIsomap.run(matrix: matrix(scaled: scaled), neighborCount: 3)
    #expect(result.points.count == 6)
    #expect(result.diagnostics.featureNeuronCount == 2)
}

@Test
func neuralIsomapMaxPointsCapsEmbeddedBinsWithEvenSampling() throws {
    let scaled = curve1D(30)
    let result = try NeuralPopulationIsomap.run(
        matrix: matrix(scaled: scaled),
        neighborCount: 4,
        maxPoints: 20
    )

    #expect(result.diagnostics.inputBinCount == 30)
    #expect(result.diagnostics.sampled)
    #expect(result.diagnostics.sampledCount == 20)
    #expect(result.diagnostics.embeddedCount == 20)
    #expect(result.points.count == 20)
    #expect(result.points.map(\.bin.binID) == ISIStateSpaceIsomap.sampleIndices(count: 30, maxPoints: 20).map { $0 + 1 })
}

@Test
func neuralIsomapMaxPointsFloorsToTwentyLikeR() throws {
    let scaled = curve1D(30)
    let result = try NeuralPopulationIsomap.run(
        matrix: matrix(scaled: scaled),
        neighborCount: 4,
        maxPoints: 5
    )

    #expect(result.diagnostics.inputBinCount == 30)
    #expect(result.diagnostics.sampled)
    #expect(result.diagnostics.sampledCount == 20)
    #expect(result.diagnostics.embeddedCount == 20)
    #expect(result.points.count == 20)
}

// MARK: - Disconnected graph behavior

@Test
func neuralIsomapDisconnectedGraphErrorPolicyThrows() {
    let m = matrix(scaled: cluster(0) + cluster(1000))   // two far-apart 5-point clusters
    #expect(throws: NeuralPopulationIsomapError.disconnectedGraph) {
        _ = try NeuralPopulationIsomap.run(matrix: m, neighborCount: 2, componentPolicy: .error)
    }
}

@Test
func neuralIsomapDisconnectedGraphLargestKeepsBiggestComponent() throws {
    let m = matrix(scaled: cluster(0) + cluster(1000))
    let result = try NeuralPopulationIsomap.run(matrix: m, neighborCount: 2, componentPolicy: .largest)
    #expect(result.diagnostics.componentCount == 2)
    // Only the largest component is embedded; rows are 1:1 with the kept bins.
    #expect(result.diagnostics.embeddedCount == result.diagnostics.largestComponentCount)
    #expect(result.points.count == result.diagnostics.embeddedCount)
    #expect(result.points.count < m.bins.count)
    // The kept bins are a subset of the input bins (binIDs are preserved).
    let keptIDs = Set(result.points.map(\.bin.binID))
    #expect(keptIDs.isSubset(of: Set(m.bins.map(\.binID))))
}

// MARK: - Known geometry

@Test
func neuralIsomapRecoversMonotonic1DOrdering() throws {
    let m = matrix(scaled: curve1D(10))
    let result = try NeuralPopulationIsomap.run(matrix: m, neighborCount: 4)

    // Connected -> all bins embedded, 1:1 with the input bins in order.
    #expect(result.points.count == 10)
    #expect(result.diagnostics.componentCount == 1)
    #expect(result.points.map(\.bin.binID) == Array(1...10))

    // Isomap recovers the 1-D ordering: NM1 is monotonic in bin order (up to sign/reflection).
    let nm1 = result.points.map { $0.nm1 ?? .nan }
    let allFinite = nm1.allSatisfy { $0.isFinite }
    #expect(allFinite)
    let increasing = zip(nm1, nm1.dropFirst()).allSatisfy { $0 < $1 }
    let decreasing = zip(nm1, nm1.dropFirst()).allSatisfy { $0 > $1 }
    #expect(increasing || decreasing)

    // Diagnostics: a clean 1-D manifold embeds with low residual variance.
    #expect(result.diagnostics.neighborCount == 4)
    #expect(result.diagnostics.featureNeuronCount == 3)   // z column is constant but kept, matching R/61
    let residual = try #require(result.diagnostics.residualVariance)
    #expect(residual < 0.2)
}

@Test
func neuralIsomapIsDeterministic() throws {
    let m = matrix(scaled: curve1D(10))
    let a = try NeuralPopulationIsomap.run(matrix: m, neighborCount: 4)
    let b = try NeuralPopulationIsomap.run(matrix: m, neighborCount: 4)
    #expect(a.points.map { $0.nm1 ?? .nan } == b.points.map { $0.nm1 ?? .nan })
}

@Test
func neuralIsomapTwoDimensionsLeavesNM3Nil() throws {
    // A 2-D embedding (dimensions: 2) populates NM1/NM2 and leaves NM3 nil (R fills missing dims with NA).
    let result = try NeuralPopulationIsomap.run(matrix: matrix(scaled: curve1D(10)), neighborCount: 4, dimensions: 2)
    let first = try #require(result.points.first)
    #expect(first.nm1 != nil)
    #expect(first.nm2 != nil)
    #expect(first.nm3 == nil)
}

// MARK: - PCA invariance

@Test
func neuralIsomapDoesNotChangePCACoordinates() throws {
    let m = matrix(scaled: curve1D(10))
    let before = try NeuralPopulationPCA.run(matrix: m)
    _ = try NeuralPopulationIsomap.run(matrix: m, neighborCount: 4)
    let after = try NeuralPopulationPCA.run(matrix: m)
    #expect(before.scores == after.scores)
}
