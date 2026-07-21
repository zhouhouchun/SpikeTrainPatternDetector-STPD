import Foundation

/// Errors mirroring `stpd_run_isi_state_isomap`'s `stop(...)` guards (`R/55_state_space_pca.R`).
public enum ISIStateSpaceIsomapError: Error, Equatable, Sendable {
    /// Fewer than five ISI feature rows ("Need at least five ISI feature rows for Isomap.").
    case tooFewRows
    /// Fewer than two varying numeric features ("Need at least two varying numeric ISI features for Isomap.").
    case tooFewVaryingFeatures
    /// The kNN graph is disconnected and `component == .error` ("Isomap kNN graph is disconnected; …"), or
    /// the geodesic graph of the kept component still has non-finite distances ("… remains disconnected; …").
    case disconnectedGraph
    /// The largest connected component has fewer than five points ("Largest Isomap component has too few points; …").
    case componentTooSmall
}

/// How a disconnected kNN graph is handled — mirrors R's `component = c("largest", "error")`.
public enum ISIStateSpaceIsomapComponentPolicy: String, Sendable, Hashable {
    /// Keep only the largest connected component (R default).
    case largest
    /// Throw `disconnectedGraph` if the graph is not fully connected.
    case error
}

/// One embedded point: the kept feature row plus its Isomap coordinates (mirrors R `scores = cbind(features0,
/// Isomap1..3)`). `dim1/dim2/dim3` are `nil` for dimensions beyond `ndim` (R fills missing with `NA_real_`).
public struct ISIStateSpaceIsomapPoint: Hashable, Sendable {
    public let row: SpikeISIStateSpaceRow
    public let dim1: Double?
    public let dim2: Double?
    public let dim3: Double?
    public init(row: SpikeISIStateSpaceRow, dim1: Double?, dim2: Double?, dim3: Double?) {
        self.row = row; self.dim1 = dim1; self.dim2 = dim2; self.dim3 = dim3
    }
}

/// Diagnostics mirroring R `stpd_run_isi_state_isomap`'s returned `diagnostics` data.frame.
public struct ISIStateSpaceIsomapDiagnostics: Hashable, Sendable {
    public let inputCount: Int            // R `n_input`
    public let sampledCount: Int          // R `n_sampled`
    public let embeddedCount: Int         // R `n_embedded`
    public let sampled: Bool              // R `sampled`
    public let neighborCount: Int         // R `n_neighbors` (effective k)
    public let componentCount: Int        // R `component_n`
    public let largestComponentCount: Int // R `largest_component_n`
    public let residualVariance: Double?  // R `residual_variance` = 1 - cor(geo, emb)^2
    public let stress: Double?            // R `stress`
    public let scaling: ISIStatePCAScaling
    public let featureCount: Int          // R `feature_n`
    public init(
        inputCount: Int, sampledCount: Int, embeddedCount: Int, sampled: Bool, neighborCount: Int,
        componentCount: Int, largestComponentCount: Int, residualVariance: Double?, stress: Double?,
        scaling: ISIStatePCAScaling, featureCount: Int
    ) {
        self.inputCount = inputCount; self.sampledCount = sampledCount; self.embeddedCount = embeddedCount
        self.sampled = sampled; self.neighborCount = neighborCount; self.componentCount = componentCount
        self.largestComponentCount = largestComponentCount; self.residualVariance = residualVariance
        self.stress = stress; self.scaling = scaling; self.featureCount = featureCount
    }
}

/// R-compatible Isomap result (mirrors `stpd_run_isi_state_isomap`'s returned list, minus the raw geodesic).
public struct ISIStateSpaceIsomapResult: Sendable {
    /// One point per embedded (kept) feature row, in the order they were embedded.
    public let points: [ISIStateSpaceIsomapPoint]
    public let diagnostics: ISIStateSpaceIsomapDiagnostics
    /// The varying numeric feature columns used (R `feature_cols`).
    public let featureColumns: [String]
    public let scaling: ISIStatePCAScaling
    /// 0-based positions into the original input that were sampled (R `sample_index`, converted to 0-based).
    public let sampleIndex: [Int]
    /// 0-based positions into the original input that survived component pruning (R `kept_sample_index`).
    public let keptSampleIndex: [Int]
    public init(
        points: [ISIStateSpaceIsomapPoint], diagnostics: ISIStateSpaceIsomapDiagnostics,
        featureColumns: [String], scaling: ISIStatePCAScaling, sampleIndex: [Int], keptSampleIndex: [Int]
    ) {
        self.points = points; self.diagnostics = diagnostics; self.featureColumns = featureColumns
        self.scaling = scaling; self.sampleIndex = sampleIndex; self.keptSampleIndex = keptSampleIndex
    }
}

/// Pure, R-compatible ISI state-space Isomap, mirroring `R/55_state_space_pca.R`
/// (`stpd_run_isi_state_isomap` + its `stpd_isomap_*` helpers). It reuses the PCA layer's feature-column
/// selection, varying filter, and robust/zscore scaling (so the input geometry matches `stpd_run_isi_state_pca`),
/// then builds a symmetric kNN graph, keeps the largest connected component, computes all-pairs geodesic
/// shortest paths (Dijkstra), and embeds via classical MDS (double-centred geodesic-squared eigendecomposition).
/// No UI, no detector coupling — the MDS eigendecomposition (the geodesic matrix is at most
/// `max_points × max_points`) is delegated to the shared `SymmetricEigensolver` (Accelerate LAPACK `dsyevd`,
/// Jacobi fallback), which is the main performance lever for large embeddings.
public enum ISIStateSpaceIsomap {

    public static func run(
        rows: [SpikeISIStateSpaceRow],
        neighborCount: Int = 15,
        dimensions: Int = 3,
        maxPoints: Int = 900,
        scaling: ISIStatePCAScaling,
        componentPolicy: ISIStateSpaceIsomapComponentPolicy = .largest
    ) throws -> ISIStateSpaceIsomapResult {
        // R: `if (nrow(features) < 5) stop("Need at least five ISI feature rows for Isomap.")`.
        guard rows.count >= 5 else { throw ISIStateSpaceIsomapError.tooFewRows }

        // R: `max_points <- max(20L, max_points)`; deterministic even-spaced sampling when over the cap.
        let cap = Swift.max(20, maxPoints)
        let sampleIndex = sampleIndices(count: rows.count, maxPoints: cap)
        let sampled = rows.count > cap
        var features0 = sampleIndex.map { rows[$0] }

        // R: feature columns -> varying numeric only; >= 2 required. Reuse the PCA selection/varying filter.
        let varying = ISIStateSpacePCA.featureColumns(features0).filter { ISIStateSpacePCA.isVaryingColumn($0.values) }
        guard varying.count >= 2 else { throw ISIStateSpaceIsomapError.tooFewVaryingFeatures }
        let featureNames = varying.map(\.name)

        // R: scale, then n, k, and the pairwise Euclidean distance matrix.
        let scaled = ISIStateSpacePCA.scaleMatrixForPCA(matrix(from: varying, rowCount: features0.count), scaling: scaling)
        var n = scaled.count
        let k = Swift.max(2, Swift.min(n - 1, neighborCount))
        var distance = euclideanDistanceMatrix(scaled)

        // R: kNN graph (symmetric OR-kNN via `pmin(adj, t(adj))`).
        var (neighbors, weights) = knnGraph(distance: distance, k: k)

        // R: connected components; keep the largest (or error when `component == "error"`).
        let components = connectedComponents(neighbors: neighbors)
        let componentCount = components.sizes.count
        let largestID = components.sizes.isEmpty ? 1 : (components.sizes.firstIndex(of: components.sizes.max()!)! + 1)
        var keep = (0..<n).filter { components.component[$0] == largestID }
        if componentCount > 1 {
            if componentPolicy == .error { throw ISIStateSpaceIsomapError.disconnectedGraph }
            let sub = subsetGraph(neighbors: neighbors, weights: weights, keep: keep)
            neighbors = sub.neighbors
            weights = sub.weights
            features0 = keep.map { features0[$0] }
            distance = keep.map { i in keep.map { j in distance[i][j] } }
            n = keep.count
        } else {
            keep = Array(0..<n)
        }
        // R: `if (nrow(features0) < 5) stop("Largest Isomap component has too few points; …")`.
        guard features0.count >= 5 else { throw ISIStateSpaceIsomapError.componentTooSmall }

        // R: all-pairs geodesic shortest paths; must be fully finite.
        let geo = allPairsShortestPaths(neighbors: neighbors, weights: weights)
        if geo.contains(where: { $0.contains { !$0.isFinite } }) {
            throw ISIStateSpaceIsomapError.disconnectedGraph
        }

        // R: `ndim <- max(2L, min(3L, ndim, nrow(features0) - 1L))`; classical MDS.
        let ndim = Swift.max(2, Swift.min(3, dimensions, features0.count - 1))
        let coordinates = classicalMDS(geodesic: geo, dimensions: ndim)

        // R diagnostics: residual variance (1 - cor^2) and stress over the upper-triangle distances.
        let embeddedDistance = euclideanDistanceMatrix(coordinates)
        let (residualVariance, stress) = embeddingDiagnostics(geodesic: geo, embedded: embeddedDistance)

        let points = features0.enumerated().map { index, row in
            let coordinate = coordinates[index]
            return ISIStateSpaceIsomapPoint(
                row: row,
                dim1: coordinate.indices.contains(0) ? coordinate[0] : nil,
                dim2: coordinate.indices.contains(1) ? coordinate[1] : nil,
                dim3: coordinate.indices.contains(2) ? coordinate[2] : nil
            )
        }

        let diagnostics = ISIStateSpaceIsomapDiagnostics(
            inputCount: rows.count,
            sampledCount: sampleIndex.count,
            embeddedCount: features0.count,
            sampled: sampled,
            neighborCount: k,
            componentCount: componentCount,
            largestComponentCount: keep.count,
            residualVariance: residualVariance,
            stress: stress,
            scaling: scaling,
            featureCount: featureNames.count
        )
        return ISIStateSpaceIsomapResult(
            points: points, diagnostics: diagnostics, featureColumns: featureNames, scaling: scaling,
            sampleIndex: sampleIndex, keptSampleIndex: keep.map { sampleIndex[$0] }
        )
    }

    // MARK: - Sampling (mirror of `sort(unique(round(seq(1, n, length.out = max_points))))`)

    /// 0-based positions selected when the input exceeds `maxPoints` (R even-spaced `seq` + `round` + unique).
    /// Returns the identity range when under the cap.
    public static func sampleIndices(count n: Int, maxPoints: Int) -> [Int] {
        guard n > maxPoints else { return Array(0..<n) }
        let m = maxPoints
        var oneBased = Set<Int>()
        for i in 0..<m {
            // R `seq(1, n, length.out = m)` then `round`.
            let value = 1.0 + Double(n - 1) * (m == 1 ? 0 : Double(i) / Double(m - 1))
            oneBased.insert(Int(value.rounded(.toNearestOrEven)))
        }
        return oneBased.sorted().map { $0 - 1 }   // 0-based
    }

    // MARK: - Matrix + distance helpers

    private static func matrix(from columns: [(name: String, values: [Double])], rowCount n: Int) -> [[Double]] {
        let p = columns.count
        var out = [[Double]](repeating: [Double](repeating: .nan, count: p), count: n)
        for j in 0..<p {
            let column = columns[j].values
            for i in 0..<n { out[i][j] = column[i] }
        }
        return out
    }

    /// Pairwise Euclidean distance (R `stats::dist`). Symmetric, zero diagonal.
    static func euclideanDistanceMatrix(_ matrix: [[Double]]) -> [[Double]] {
        let n = matrix.count
        var distance = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        guard n > 0 else { return distance }
        let p = matrix[0].count
        for i in 0..<n {
            for j in (i + 1)..<n {
                var sum = 0.0
                for d in 0..<p {
                    let delta = matrix[i][d] - matrix[j][d]
                    sum += delta * delta
                }
                let value = sum.squareRoot()
                distance[i][j] = value
                distance[j][i] = value
            }
        }
        return distance
    }

    // MARK: - kNN graph (mirror of stpd_isomap_knn_graph)

    static func knnGraph(distance: [[Double]], k: Int) -> (neighbors: [[Int]], weights: [[Double]]) {
        let n = distance.count
        var adjacency = [[Double]](repeating: [Double](repeating: .infinity, count: n), count: n)
        for i in 0..<n { adjacency[i][i] = 0 }
        for i in 0..<n {
            // R: `row[i] <- Inf; nn <- order(row); finite only; head(k)`.
            let candidates = (0..<n)
                .filter { $0 != i && distance[i][$0].isFinite }
                .sorted { lhs, rhs in
                    distance[i][lhs] != distance[i][rhs] ? distance[i][lhs] < distance[i][rhs] : lhs < rhs
                }
            for j in candidates.prefix(k) { adjacency[i][j] = distance[i][j] }
        }
        // R: `adj <- pmin(adj, t(adj))` — symmetric OR-kNN (an edge survives if either endpoint chose it).
        var symmetric = adjacency
        for i in 0..<n {
            for j in 0..<n { symmetric[i][j] = Swift.min(adjacency[i][j], adjacency[j][i]) }
        }
        var neighbors = [[Int]](repeating: [], count: n)
        var weights = [[Double]](repeating: [], count: n)
        for i in 0..<n {
            for j in 0..<n where j != i && symmetric[i][j].isFinite {
                neighbors[i].append(j)
                weights[i].append(symmetric[i][j])
            }
        }
        return (neighbors, weights)
    }

    // MARK: - Connected components (mirror of stpd_isomap_components, iterative DFS)

    static func connectedComponents(neighbors: [[Int]]) -> (component: [Int], sizes: [Int]) {
        let n = neighbors.count
        var component = [Int](repeating: 0, count: n)
        var sizes: [Int] = []
        var componentID = 0
        for start in 0..<n where component[start] == 0 {
            componentID += 1
            component[start] = componentID
            var stack = [start]
            var size = 0
            while let v = stack.popLast() {
                size += 1
                for u in neighbors[v] where component[u] == 0 {
                    component[u] = componentID
                    stack.append(u)
                }
            }
            sizes.append(size)
        }
        return (component, sizes)
    }

    // MARK: - Subset graph (mirror of stpd_isomap_subset_graph)

    static func subsetGraph(
        neighbors: [[Int]], weights: [[Double]], keep: [Int]
    ) -> (neighbors: [[Int]], weights: [[Double]]) {
        var map = [Int](repeating: -1, count: neighbors.count)
        for (newIndex, old) in keep.enumerated() { map[old] = newIndex }
        var newNeighbors = [[Int]](repeating: [], count: keep.count)
        var newWeights = [[Double]](repeating: [], count: keep.count)
        for (newIndex, old) in keep.enumerated() {
            for (offset, nb) in neighbors[old].enumerated() where map[nb] >= 0 {
                newNeighbors[newIndex].append(map[nb])
                newWeights[newIndex].append(weights[old][offset])
            }
        }
        return (newNeighbors, newWeights)
    }

    // MARK: - Dijkstra + all-pairs (mirror of stpd_isomap_dijkstra / _all_pairs_shortest_paths)

    static func dijkstra(neighbors: [[Int]], weights: [[Double]], source: Int) -> [Double] {
        let n = neighbors.count
        var dist = [Double](repeating: .infinity, count: n)
        var visited = [Bool](repeating: false, count: n)
        dist[source] = 0
        var heap = BinaryMinHeap()
        heap.push(node: source, value: 0)
        while let item = heap.pop() {
            let v = item.node
            if visited[v] { continue }
            visited[v] = true
            for (offset, u) in neighbors[v].enumerated() where !visited[u] {
                let alt = item.value + weights[v][offset]
                if alt < dist[u] {
                    dist[u] = alt
                    heap.push(node: u, value: alt)
                }
            }
        }
        return dist
    }

    static func allPairsShortestPaths(neighbors: [[Int]], weights: [[Double]]) -> [[Double]] {
        let n = neighbors.count
        var geo = (0..<n).map { dijkstra(neighbors: neighbors, weights: weights, source: $0) }
        for i in 0..<n { geo[i][i] = 0 }
        return geo
    }

    // MARK: - Classical MDS (mirror of stats::cmdscale on the geodesic distances)

    /// Double-centre `D²`, eigendecompose, and project onto the top `dimensions` positive eigenpairs:
    /// `points[:, c] = vector_c · sqrt(max(eigenvalue_c, 0))`. Sign is reflection-invariant (a deterministic
    /// convention is applied), matching R `cmdscale` up to axis sign/reflection.
    static func classicalMDS(geodesic: [[Double]], dimensions: Int) -> [[Double]] {
        let n = geodesic.count
        guard n > 0, dimensions > 0 else { return Array(repeating: [], count: n) }

        // B = -0.5 · J · D² · J  with  J = I - 11ᵀ/n  (double-centring of the squared distances).
        var squared = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n {
            for j in 0..<n { squared[i][j] = geodesic[i][j] * geodesic[i][j] }
        }
        let rowMeans = squared.map { $0.reduce(0, +) / Double(n) }
        let grandMean = rowMeans.reduce(0, +) / Double(n)
        var b = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n {
            for j in 0..<n {
                b[i][j] = -0.5 * (squared[i][j] - rowMeans[i] - rowMeans[j] + grandMean)
            }
        }

        let (values, vectors) = SymmetricEigensolver.decompose(b)   // descending, vectors as columns
        var points = [[Double]](repeating: [Double](repeating: 0, count: dimensions), count: n)
        for c in 0..<dimensions {
            let lambda = c < values.count ? values[c] : 0
            let scale = lambda > 0 ? lambda.squareRoot() : 0
            for i in 0..<n {
                points[i][c] = (c < values.count ? vectors[i][c] : 0) * scale
            }
        }
        return points
    }

    // MARK: - Embedding diagnostics (mirror of the residual-variance / stress block)

    static func embeddingDiagnostics(
        geodesic: [[Double]], embedded: [[Double]]
    ) -> (residualVariance: Double?, stress: Double?) {
        let n = geodesic.count
        var geoVec: [Double] = []
        var embVec: [Double] = []
        for i in 0..<n {
            for j in (i + 1)..<n {
                geoVec.append(geodesic[i][j])
                embVec.append(embedded[i][j])
            }
        }
        let residualVariance = correlation(geoVec, embVec).map { 1 - $0 * $0 }
        let geoSumSquares = geoVec.reduce(0) { $0 + $1 * $1 }
        let stress: Double?
        if geoSumSquares > 0 {
            let diffSumSquares = zip(geoVec, embVec).reduce(0.0) { acc, pair in
                let delta = pair.0 - pair.1
                return acc + delta * delta
            }
            stress = (diffSumSquares / geoSumSquares).squareRoot()
        } else {
            stress = nil
        }
        return (residualVariance, stress)
    }

    /// Pearson correlation over complete (finite) pairs; `nil` when undefined (R `cor(use="complete.obs")`).
    private static func correlation(_ x: [Double], _ y: [Double]) -> Double? {
        let pairs = zip(x, y).filter { $0.0.isFinite && $0.1.isFinite }
        guard pairs.count >= 2 else { return nil }
        let mx = pairs.reduce(0) { $0 + $1.0 } / Double(pairs.count)
        let my = pairs.reduce(0) { $0 + $1.1 } / Double(pairs.count)
        var sxy = 0.0, sxx = 0.0, syy = 0.0
        for (a, b) in pairs {
            sxy += (a - mx) * (b - my)
            sxx += (a - mx) * (a - mx)
            syy += (b - my) * (b - my)
        }
        guard sxx > 0, syy > 0 else { return nil }
        return sxy / (sxx * syy).squareRoot()
    }

}

/// Minimal binary min-heap on (node, value), mirroring the hand-rolled heap in `stpd_isomap_dijkstra`.
private struct BinaryMinHeap {
    private var nodes: [Int] = []
    private var values: [Double] = []

    mutating func push(node: Int, value: Double) {
        nodes.append(node)
        values.append(value)
        var pos = nodes.count - 1
        while pos > 0 {
            let parent = (pos - 1) / 2
            if values[parent] <= values[pos] { break }
            nodes.swapAt(parent, pos); values.swapAt(parent, pos)
            pos = parent
        }
    }

    mutating func pop() -> (node: Int, value: Double)? {
        guard !nodes.isEmpty else { return nil }
        let outNode = nodes[0]
        let outValue = values[0]
        let last = nodes.count - 1
        nodes[0] = nodes[last]; values[0] = values[last]
        nodes.removeLast(); values.removeLast()
        var pos = 0
        let count = nodes.count
        while true {
            let left = pos * 2 + 1
            let right = left + 1
            if left >= count { break }
            var smaller = left
            if right < count && values[right] < values[left] { smaller = right }
            if values[pos] <= values[smaller] { break }
            nodes.swapAt(pos, smaller); values.swapAt(pos, smaller)
            pos = smaller
        }
        return (outNode, outValue)
    }
}
