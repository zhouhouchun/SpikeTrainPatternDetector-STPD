import Foundation

/// Errors mirroring the R neural-manifold Isomap guards (`R/61` `stpd_run_neural_manifold_embedding` /
/// `stpd_neural_generic_isomap`).
public enum NeuralPopulationIsomapError: Error, Equatable, Sendable {
    /// Fewer than five population time bins ("Need at least five time bins for Isomap.").
    case tooFewBins
    /// Fewer than two population-activity columns (R dispatch's `ncol(X) >= 2` guard).
    case tooFewNeurons
    /// The kNN graph is disconnected and the policy is `.error`, or the kept component's geodesic graph still
    /// has non-finite distances.
    case disconnectedGraph
    /// The largest connected component has fewer than five bins.
    case componentTooSmall
}

/// One embedded population bin: the bin (preserving `binID` + times) plus its Isomap coordinates NM1–NM3
/// (`nil` for dimensions beyond `ndim`, mirroring R's `NA_real_` fill). Coordinates come only from the binned
/// population activity (`matrix.scaled`) — never from detector/event labels.
public struct NeuralPopulationIsomapPoint: Hashable, Sendable {
    public let bin: NeuralPopulationBin
    public let nm1: Double?
    public let nm2: Double?
    public let nm3: Double?

    public init(bin: NeuralPopulationBin, nm1: Double?, nm2: Double?, nm3: Double?) {
        self.bin = bin
        self.nm1 = nm1
        self.nm2 = nm2
        self.nm3 = nm3
    }
}

/// Diagnostics mirroring the R neural Isomap `diagnostics` (n_neighbors, embedded_points, component_count,
/// residual_variance) plus the sampling / feature context.
public struct NeuralPopulationIsomapDiagnostics: Hashable, Sendable {
    public let inputBinCount: Int
    public let sampledCount: Int
    public let embeddedCount: Int          // R "embedded_points" (largest component when disconnected)
    public let sampled: Bool
    public let neighborCount: Int          // effective k (R "n_neighbors")
    public let componentCount: Int         // R "component_count"
    public let largestComponentCount: Int
    public let residualVariance: Double?   // R "residual_variance" = 1 - cor(geodesic, embedding)^2
    public let stress: Double?
    public let featureNeuronCount: Int     // population-activity columns used as Isomap features
    public let methodLabel: String         // "Isomap"

    public init(
        inputBinCount: Int, sampledCount: Int, embeddedCount: Int, sampled: Bool, neighborCount: Int,
        componentCount: Int, largestComponentCount: Int, residualVariance: Double?, stress: Double?,
        featureNeuronCount: Int, methodLabel: String
    ) {
        self.inputBinCount = inputBinCount
        self.sampledCount = sampledCount
        self.embeddedCount = embeddedCount
        self.sampled = sampled
        self.neighborCount = neighborCount
        self.componentCount = componentCount
        self.largestComponentCount = largestComponentCount
        self.residualVariance = residualVariance
        self.stress = stress
        self.featureNeuronCount = featureNeuronCount
        self.methodLabel = methodLabel
    }
}

/// R-compatible Neural Manifold Isomap result. `points` are bin-aligned and 1:1 with the embedded (kept) bins.
public struct NeuralPopulationIsomapResult: Sendable {
    public let points: [NeuralPopulationIsomapPoint]
    public let diagnostics: NeuralPopulationIsomapDiagnostics

    private let pointsByBinID: [Int: NeuralPopulationIsomapPoint]

    public init(points: [NeuralPopulationIsomapPoint], diagnostics: NeuralPopulationIsomapDiagnostics) {
        self.points = points
        self.diagnostics = diagnostics
        self.pointsByBinID = Dictionary(points.map { ($0.bin.binID, $0) }, uniquingKeysWith: { first, _ in first })
    }

    public func point(forBinID binID: Int) -> NeuralPopulationIsomapPoint? { pointsByBinID[binID] }
}

/// Pure, R-compatible Neural Manifold Isomap, mirroring `R/61` `stpd_neural_generic_isomap` driven by
/// `stpd_run_neural_manifold_embedding(method = "isomap")`. It embeds the **already-scaled** population activity
/// matrix (`matrix.scaled` = R `pop$X`; bins are points, neurons are features) — no re-scaling, only R's
/// per-column NA→median fill — and reuses the exact `stpd_isomap_*` primitives already ported in
/// `ISIStateSpaceIsomap` (sampling, Euclidean kNN graph, connected components, Dijkstra geodesics, classical
/// MDS, residual-variance diagnostics). Detector/event labels are never an input — this is a coordinate method.
public enum NeuralPopulationIsomap {
    public static func run(
        matrix: NeuralPopulationMatrix,
        neighborCount: Int = 15,
        dimensions: Int = 3,
        maxPoints: Int = 1200,
        componentPolicy: ISIStateSpaceIsomapComponentPolicy = .largest
    ) throws -> NeuralPopulationIsomapResult {
        let scaled = matrix.scaled
        let allBins = matrix.bins
        // R `stpd_neural_generic_isomap`: `if (n < 5L) stop("Need at least five time bins for Isomap.")`.
        guard scaled.count >= 5, scaled.count == allBins.count else { throw NeuralPopulationIsomapError.tooFewBins }

        // R dispatch: even-spaced sampling to `max_points` (default 1200). Reuse the shared sampler.
        let cap = Swift.max(20, maxPoints)
        let sampleIndex = ISIStateSpaceIsomap.sampleIndices(count: scaled.count, maxPoints: cap)
        let sampled = scaled.count > cap
        var bins0 = sampleIndex.map { allBins[$0] }

        // R `stpd_neural_fill_matrix`: replace non-finite per column with the column median (the scaled matrix
        // is already finite in practice, so this is normally a no-op). Unlike the ISI state-space Isomap path,
        // R's neural-manifold dispatch does not drop constant neuron columns before Isomap; constant columns
        // simply contribute 0 to Euclidean distances.
        let filled = fillNonFiniteWithColumnMedian(sampleIndex.map { scaled[$0] })
        let featureWidth = filled.first?.count ?? 0
        guard featureWidth >= 2 else { throw NeuralPopulationIsomapError.tooFewNeurons }
        var featurePoints = filled

        var n = featurePoints.count
        let k = Swift.max(2, Swift.min(n - 1, neighborCount))
        let distance = ISIStateSpaceIsomap.euclideanDistanceMatrix(featurePoints)
        var (neighbors, weights) = ISIStateSpaceIsomap.knnGraph(distance: distance, k: k)

        // R: connected components; keep the largest (or error when disconnected and policy == .error).
        let components = ISIStateSpaceIsomap.connectedComponents(neighbors: neighbors)
        let componentCount = components.sizes.count
        let largestID = components.sizes.isEmpty
            ? 1
            : (components.sizes.firstIndex(of: components.sizes.max()!)! + 1)
        var keep = (0..<n).filter { components.component[$0] == largestID }
        if componentCount > 1 {
            if componentPolicy == .error { throw NeuralPopulationIsomapError.disconnectedGraph }
            let sub = ISIStateSpaceIsomap.subsetGraph(neighbors: neighbors, weights: weights, keep: keep)
            neighbors = sub.neighbors
            weights = sub.weights
            bins0 = keep.map { bins0[$0] }
            featurePoints = keep.map { featurePoints[$0] }
            n = keep.count
        } else {
            keep = Array(0..<n)
        }
        // R: `if (nrow(features0) < 5) stop("Largest Isomap component has too few points; …")`.
        guard bins0.count >= 5 else { throw NeuralPopulationIsomapError.componentTooSmall }

        // R `stpd_neural_generic_isomap` (R/61): impute any non-finite geodesic with 2× the max finite geodesic,
        // and fail ONLY when there is no finite geodesic at all. (This differs from the ISI Isomap, which throws
        // on any non-finite geodesic; the neural path imputes. In practice the kept component is connected by
        // construction, so within-component geodesics are already finite and no imputation occurs.)
        var geo = ISIStateSpaceIsomap.allPairsShortestPaths(neighbors: neighbors, weights: weights)
        var maxFiniteGeo: Double?
        for i in 0..<geo.count {
            for j in (i + 1)..<geo.count where geo[i][j].isFinite {
                if maxFiniteGeo == nil || geo[i][j] > maxFiniteGeo! { maxFiniteGeo = geo[i][j] }
            }
        }
        guard let maxFinite = maxFiniteGeo else { throw NeuralPopulationIsomapError.disconnectedGraph }
        let imputedGeo = maxFinite * 2
        for i in 0..<geo.count {
            for j in 0..<geo.count where !geo[i][j].isFinite {
                geo[i][j] = imputedGeo
            }
        }

        // R: `ndim <- max(2L, min(3L, ndim, nrow(features0) - 1L))`; classical MDS.
        let ndim = Swift.max(2, Swift.min(3, dimensions, bins0.count - 1))
        let coordinates = ISIStateSpaceIsomap.classicalMDS(geodesic: geo, dimensions: ndim)
        let embeddedDistance = ISIStateSpaceIsomap.euclideanDistanceMatrix(coordinates)
        let (residualVariance, stress) = ISIStateSpaceIsomap.embeddingDiagnostics(
            geodesic: geo, embedded: embeddedDistance
        )

        let points = bins0.enumerated().map { index, bin -> NeuralPopulationIsomapPoint in
            let coordinate = coordinates[index]
            return NeuralPopulationIsomapPoint(
                bin: bin,
                nm1: coordinate.indices.contains(0) ? coordinate[0] : nil,
                nm2: coordinate.indices.contains(1) ? coordinate[1] : nil,
                nm3: coordinate.indices.contains(2) ? coordinate[2] : nil
            )
        }
        let diagnostics = NeuralPopulationIsomapDiagnostics(
            inputBinCount: scaled.count,
            sampledCount: sampleIndex.count,
            embeddedCount: bins0.count,
            sampled: sampled,
            neighborCount: k,
            componentCount: componentCount,
            largestComponentCount: keep.count,
            residualVariance: residualVariance,
            stress: stress,
            featureNeuronCount: featureWidth,
            methodLabel: "Isomap"
        )
        return NeuralPopulationIsomapResult(points: points, diagnostics: diagnostics)
    }

    // MARK: - Preprocessing (mirror of stpd_neural_fill_matrix)

    /// R `stpd_neural_fill_matrix`: per column, replace non-finite entries with the column's finite median
    /// (0 when a column has no finite value).
    private static func fillNonFiniteWithColumnMedian(_ rows: [[Double]]) -> [[Double]] {
        guard let width = rows.first?.count, width > 0 else { return rows }
        var out = rows
        for j in 0..<width {
            let finite = rows.compactMap { $0[j].isFinite ? $0[j] : nil }
            let fill = finite.isEmpty ? 0 : median(finite)
            for i in 0..<out.count where !out[i][j].isFinite {
                out[i][j] = fill
            }
        }
        return out
    }

    private static func median(_ values: [Double]) -> Double {
        let sorted = values.sorted()
        let count = sorted.count
        guard count > 0 else { return 0 }
        if count % 2 == 1 { return sorted[count / 2] }
        return (sorted[count / 2 - 1] + sorted[count / 2]) / 2
    }
}
