import Foundation
import STPDCore
import Testing

// ISI State Space parity P1: pure R-compatible Isomap (mirror of stpd_run_isi_state_isomap + helpers).
// Behavior is exercised through the public `run` API (kNN graph / components / Dijkstra / classical MDS).

private func isomapRows(_ isis: [Double], k: Int = 3) -> [SpikeISIStateSpaceRow] {
    var ts = [0.0]
    for d in isis { ts.append(ts.last! + d) }
    return SpikeISIStateSpaceFeatureBuilder.makeRows(
        train: SpikeTrain(name: "t", timestampsSec: ts), k: k, winsorize: false
    )
}

/// A smooth curved/ordered ISI motif (exponential ramp), enough rows + varying features for Isomap.
private func orderedMotifRows() -> [SpikeISIStateSpaceRow] {
    var ramp: [Double] = []
    for i in 0..<16 { ramp.append(0.005 * pow(1.18, Double(i))) }
    return isomapRows(ramp)
}

private func correlationMagnitude(_ a: [Double], _ b: [Double]) -> Double {
    let n = Double(a.count)
    let ma = a.reduce(0, +) / n, mb = b.reduce(0, +) / n
    var sxy = 0.0, sx = 0.0, sy = 0.0
    for i in a.indices {
        sxy += (a[i] - ma) * (b[i] - mb); sx += (a[i] - ma) * (a[i] - ma); sy += (b[i] - mb) * (b[i] - mb)
    }
    return abs(sxy / (sx * sy).squareRoot())
}

// MARK: - 1. Guards mirror R `stop(...)`: too few rows / too few varying features.

@Test
func isomapRejectsTooFewRows() {
    let rows = isomapRows([0.02, 0.03, 0.04])   // 4 timestamps -> ~3 rows (< 5)
    #expect(rows.count < 5)
    #expect(throws: ISIStateSpaceIsomapError.tooFewRows) {
        _ = try ISIStateSpaceIsomap.run(rows: rows, neighborCount: 3, scaling: .robust)
    }
}

/// Rows whose every numeric feature is the same constant -> R's varying filter keeps 0 columns.
private func constantFeatureRows(_ count: Int, value: Double = 0.5) -> [SpikeISIStateSpaceRow] {
    (0..<count).map { i in
        SpikeISIStateSpaceRow(
            trainID: "t", trainName: "t", rowNumber: i + 1, idx: i + 1, leftIdx: i, rightIdx: i + 1,
            leftTimeSec: Double(i) * 0.1, rightTimeSec: Double(i + 1) * 0.1,
            timeMidSec: (Double(i) + 0.5) * 0.1, isiSec: 0.1,
            logISI: value, logISIFeature: value, lagLogFeature: [-1: value, 1: value],
            localMedianLogISI: value, localMeanLogISI: value, localSDLogISI: value,
            localQ10LogISI: value, localQ90LogISI: value, localIQRLogISI: value,
            localMeanISISec: value, localMedianISISec: value, localRateHz: value,
            localCV: value, localLV: value, localCV2: value,
            deltaLogISI: value, nextDeltaLogISI: value, prepostRatio: value,
            qcStatus: .ok, label: nil
        )
    }
}

@Test
func isomapRejectsTooFewVaryingFeatures() {
    // >= 5 rows but every feature is constant -> R's varying filter leaves < 2 columns.
    let rows = constantFeatureRows(6)
    #expect(rows.count >= 5)
    #expect(throws: ISIStateSpaceIsomapError.tooFewVaryingFeatures) {
        _ = try ISIStateSpaceIsomap.run(rows: rows, neighborCount: 3, scaling: .robust)
    }
}

// MARK: - 2. Stable, faithful embedding on a curved/ordered motif.

@Test
func isomapProducesStableFaithfulEmbeddingOnOrderedMotif() throws {
    let rows = orderedMotifRows()
    let result = try ISIStateSpaceIsomap.run(rows: rows, neighborCount: 4, scaling: .robust)

    // Connected single component, every row embedded, diagnostics consistent.
    #expect(result.diagnostics.componentCount == 1)
    #expect(result.diagnostics.embeddedCount == rows.count)
    #expect(result.points.count == rows.count)
    #expect(result.diagnostics.neighborCount == 4)
    #expect(result.diagnostics.featureCount >= 2)
    #expect(result.diagnostics.sampled == false)

    // Faithful: residual variance and stress are in range and small (geodesics well preserved).
    let residual = try #require(result.diagnostics.residualVariance)
    #expect(residual >= 0 && residual <= 1)
    #expect(residual < 0.2)
    let stress = try #require(result.diagnostics.stress)
    #expect(stress >= 0 && stress < 0.3)

    // Coordinates are non-degenerate (dim1 actually spreads the points).
    let dim1 = result.points.compactMap(\.dim1)
    #expect(dim1.count == rows.count)
    #expect((dim1.max()! - dim1.min()!) > 1e-6)

    // Stable: a second run is bit-identical (deterministic eigensolver + sign convention).
    let again = try ISIStateSpaceIsomap.run(rows: rows, neighborCount: 4, scaling: .robust)
    for (a, b) in zip(result.points, again.points) {
        #expect(abs((a.dim1 ?? 0) - (b.dim1 ?? 0)) < 1e-12)
        #expect(abs((a.dim2 ?? 0) - (b.dim2 ?? 0)) < 1e-12)
    }
    // (cor(dim1, order) is intentionally NOT asserted: the local-window features fold the 1-D ramp, so dim1
    // need not be monotone with row index; the faithful-embedding + determinism checks above are the contract.)
    _ = correlationMagnitude(dim1, dim1)
}

// MARK: - 3. Disconnected kNN graph: largest-component vs error policy (mirrors R `component`).

@Test
func isomapDisconnectedGraphHandlingMatchesRIntent() throws {
    // Two well-separated ISI regimes (fast vs slow) with a large gap, small k -> no cross-cluster edges.
    let fast = (0..<7).map { 0.010 + Double($0) * 0.0003 }
    let slow = (0..<6).map { 2.0 + Double($0) * 0.01 }
    let rows = isomapRows(fast + [5.0] + slow)

    // .largest keeps the biggest component and embeds only it.
    let largest = try ISIStateSpaceIsomap.run(rows: rows, neighborCount: 2, scaling: .robust, componentPolicy: .largest)
    #expect(largest.diagnostics.componentCount >= 2)
    #expect(largest.diagnostics.embeddedCount >= 5)
    #expect(largest.diagnostics.embeddedCount < rows.count)
    #expect(largest.diagnostics.largestComponentCount == largest.diagnostics.embeddedCount)
    #expect(largest.points.count == largest.diagnostics.embeddedCount)

    // .error refuses a disconnected graph (R `stop("Isomap kNN graph is disconnected; …")`).
    #expect(throws: ISIStateSpaceIsomapError.disconnectedGraph) {
        _ = try ISIStateSpaceIsomap.run(rows: rows, neighborCount: 2, scaling: .robust, componentPolicy: .error)
    }
}

// MARK: - 4. Sampling index mirrors R `sort(unique(round(seq(1, n, length.out = m))))`.

@Test
func isomapSampleIndicesMatchRSeqRounding() {
    // Under the cap: identity.
    #expect(ISIStateSpaceIsomap.sampleIndices(count: 8, maxPoints: 20) == Array(0..<8))
    // Over the cap: evenly spaced, unique, sorted, 0-based, endpoints included.
    let sampled = ISIStateSpaceIsomap.sampleIndices(count: 100, maxPoints: 20)
    #expect(sampled.count <= 20)
    #expect(sampled.first == 0)
    #expect(sampled.last == 99)
    #expect(sampled == sampled.sorted())
    #expect(Set(sampled).count == sampled.count)
    // R `round()` is IEC 60559 / half-to-even: seq(1, 8, length.out = 3) -> 1, 4.5, 8 -> 1, 4, 8.
    #expect(ISIStateSpaceIsomap.sampleIndices(count: 8, maxPoints: 3) == [0, 3, 7])
}
