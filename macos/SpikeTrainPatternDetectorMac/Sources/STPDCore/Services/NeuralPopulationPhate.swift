import Foundation

/// Errors mirroring the R neural-manifold PHATE guards (`R/61` `stpd_run_neural_manifold_embedding` /
/// `stpd_neural_generic_phate`).
public enum NeuralPopulationPhateError: Error, Equatable, Sendable {
    /// Fewer than five population time bins ("Need at least five time bins for PHATE.").
    case tooFewBins
    /// Fewer than two population-activity columns (R dispatch's `ncol(X) >= 2` guard).
    case tooFewNeurons
    /// The diffusion potential / MDS produced no finite coordinates (degenerate input).
    case degenerate
}

/// One embedded population bin: the bin (preserving `binID` + times) plus its PHATE coordinates NM1–NM3.
/// Coordinates come only from the binned population activity (`matrix.scaled`) — never from detector/event
/// labels. Following R `stpd_neural_take3`, missing/non-positive dimensions are `0` (not `nil`).
public struct NeuralPopulationPhatePoint: Hashable, Sendable {
    public let bin: NeuralPopulationBin
    public let nm1: Double
    public let nm2: Double
    public let nm3: Double

    public init(bin: NeuralPopulationBin, nm1: Double, nm2: Double, nm3: Double) {
        self.bin = bin
        self.nm1 = nm1
        self.nm2 = nm2
        self.nm3 = nm3
    }
}

/// Diagnostics for the Neural Manifold PHATE (diffusion-potential + classical MDS) fallback. The R diagnostics
/// row is minimal (`backend = diffusion_potential_mds`); these Swift fields add the sampling / kernel context
/// the UI panel shows (parallel to the Isomap diagnostics).
public struct NeuralPopulationPhateDiagnostics: Hashable, Sendable {
    public let inputBinCount: Int
    public let sampledCount: Int
    public let embeddedCount: Int          // == sampledCount (PHATE embeds every sampled bin; no pruning)
    public let sampled: Bool
    public let neighborCount: Int          // effective local-kernel k (R "n_neighbors")
    public let diffusionTime: Int          // effective t (R "diffusion_time")
    public let kernelEpsilon: Double       // R diffusion-kernel bandwidth ε (median k-NN squared distance)
    public let featureNeuronCount: Int     // population-activity columns used as features
    public let backend: String             // R diagnostics "backend" value ("diffusion_potential_mds")
    public let methodLabel: String         // "PHATE"

    public init(
        inputBinCount: Int, sampledCount: Int, embeddedCount: Int, sampled: Bool, neighborCount: Int,
        diffusionTime: Int, kernelEpsilon: Double, featureNeuronCount: Int, backend: String, methodLabel: String
    ) {
        self.inputBinCount = inputBinCount
        self.sampledCount = sampledCount
        self.embeddedCount = embeddedCount
        self.sampled = sampled
        self.neighborCount = neighborCount
        self.diffusionTime = diffusionTime
        self.kernelEpsilon = kernelEpsilon
        self.featureNeuronCount = featureNeuronCount
        self.backend = backend
        self.methodLabel = methodLabel
    }
}

/// R-compatible Neural Manifold PHATE result. `points` are bin-aligned and 1:1 with the embedded (sampled) bins.
public struct NeuralPopulationPhateResult: Sendable {
    public let points: [NeuralPopulationPhatePoint]
    public let diagnostics: NeuralPopulationPhateDiagnostics

    private let pointsByBinID: [Int: NeuralPopulationPhatePoint]

    public init(points: [NeuralPopulationPhatePoint], diagnostics: NeuralPopulationPhateDiagnostics) {
        self.points = points
        self.diagnostics = diagnostics
        self.pointsByBinID = Dictionary(points.map { ($0.bin.binID, $0) }, uniquingKeysWith: { first, _ in first })
    }

    public func point(forBinID binID: Int) -> NeuralPopulationPhatePoint? { pointsByBinID[binID] }
}

/// Pure, R-compatible Neural Manifold PHATE **fallback**, mirroring `R/61` `stpd_neural_generic_phate`'s
/// `diffusion_potential_mds` branch (the deterministic path R uses when the external `phateR` package is absent),
/// driven by `stpd_run_neural_manifold_embedding(method = "phate")`. It embeds the **already-scaled** population
/// activity matrix (`matrix.scaled` = R `pop$X`; bins are points, neurons are features), with only R's per-column
/// NA→median fill, and reuses the shared sampler / Euclidean-distance / classical-MDS primitives from
/// `ISIStateSpaceIsomap`. It deliberately does NOT call the external Python/R `phateR` backend.
///
/// Pipeline (mirror of the R fallback):
/// 1. even-spaced sampling to `maxPoints` (R `stpd_state_trajectory_sample_index`);
/// 2. anisotropic diffusion affinity `P` (R `stpd_diffusion_probability`, `kernel_scale = "local"`, `alpha = 1`);
/// 3. `t`-step diffusion `Pᵗ`, floored at machine epsilon, potential `U = -log(Pᵗ)`;
/// 4. Euclidean distance between potential rows → classical MDS (`stats::cmdscale`).
///
/// Note on `cmdscale(..., add = TRUE)`: R requests the Cailliez additive constant, but `dist(U)` is by
/// construction a *Euclidean* distance matrix (the pairwise distances of the real rows of `U`), so its additive
/// constant is provably `0` (empirically `≈1e-16` in R). Classical MDS (`add = FALSE`) therefore yields the same
/// coordinates as R's `add = TRUE`, up to the usual axis sign/reflection — verified against R reference values in
/// `NeuralPopulationPhateTests`. This avoids the expensive non-symmetric `2n×2n` eigenproblem the additive
/// constant would otherwise require, with no change in the result.
public enum NeuralPopulationPhate {
    public static func run(
        matrix: NeuralPopulationMatrix,
        neighborCount: Int = 15,
        diffusionTime: Int = 3,
        dimensions: Int = 3,
        maxPoints: Int = 1200
    ) throws -> NeuralPopulationPhateResult {
        let scaled = matrix.scaled
        let allBins = matrix.bins
        // R `stpd_neural_generic_phate`: `if (n < 5L) stop("Need at least five time bins for PHATE.")`.
        guard scaled.count >= 5, scaled.count == allBins.count else { throw NeuralPopulationPhateError.tooFewBins }

        // R dispatch: even-spaced sampling to `max_points` (default 1200). Reuse the shared sampler.
        let cap = Swift.max(20, maxPoints)
        let sampleIndex = ISIStateSpaceIsomap.sampleIndices(count: scaled.count, maxPoints: cap)
        let sampled = scaled.count > cap
        let bins0 = sampleIndex.map { allBins[$0] }

        // R `stpd_neural_fill_matrix`: per-column non-finite → column median (normally a no-op for the scaled
        // matrix). R's neural dispatch does not drop constant columns; they contribute 0 to Euclidean distances.
        let filled = fillNonFiniteWithColumnMedian(sampleIndex.map { scaled[$0] })
        let featureWidth = filled.first?.count ?? 0
        guard featureWidth >= 2 else { throw NeuralPopulationPhateError.tooFewNeurons }
        guard filled.count >= 5 else { throw NeuralPopulationPhateError.tooFewBins }

        let n = filled.count
        // R `stpd_diffusion_probability(X, kernel_scale = "local", n_neighbors, alpha = 1)`.
        let kernel = diffusionProbability(filled, neighborCount: neighborCount)

        // R: `t_use <- max(1L, diffusion_time)`; `Pt <- P`; multiply (t_use - 1) times → Pᵗ.
        let tUse = Swift.max(1, diffusionTime)
        var pt = kernel.p
        for _ in 1..<Swift.max(1, tUse) {
            pt = multiply(pt, kernel.p)
        }

        // R: `Pt <- pmax(Pt, .Machine$double.eps)`; `potential <- -log(Pt)`.
        let floorValue = Double.ulpOfOne   // R `.Machine$double.eps` = 2^-52
        var potential = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n {
            for j in 0..<n {
                potential[i][j] = -Foundation.log(Swift.max(pt[i][j], floorValue))
            }
        }

        // R: `dpot <- stats::dist(potential)`; `cmdscale(dpot, k = max(2, min(3, ndim, n - 1)), add = TRUE)`.
        // `dist(potential)` is a Euclidean distance matrix, so the additive constant is 0 and classical MDS
        // reproduces R's coordinates (see the type comment). Reuse the shared distance + MDS primitives.
        let distance = ISIStateSpaceIsomap.euclideanDistanceMatrix(potential)
        let ndim = Swift.max(2, Swift.min(3, dimensions, n - 1))
        let coordinates = ISIStateSpaceIsomap.classicalMDS(geodesic: distance, dimensions: ndim)

        // R `stpd_neural_take3`: pad missing dimensions with 0 (classical MDS already pads non-positive eigen-
        // pairs to 0, so every embedded bin has finite NM1–NM3).
        let points = bins0.enumerated().map { index, bin -> NeuralPopulationPhatePoint in
            let coordinate = coordinates[index]
            return NeuralPopulationPhatePoint(
                bin: bin,
                nm1: coordinate.indices.contains(0) ? coordinate[0] : 0,
                nm2: coordinate.indices.contains(1) ? coordinate[1] : 0,
                nm3: coordinate.indices.contains(2) ? coordinate[2] : 0
            )
        }
        guard points.contains(where: { $0.nm1.isFinite && $0.nm2.isFinite }) else {
            throw NeuralPopulationPhateError.degenerate
        }

        let diagnostics = NeuralPopulationPhateDiagnostics(
            inputBinCount: scaled.count,
            sampledCount: sampleIndex.count,
            embeddedCount: bins0.count,
            sampled: sampled,
            neighborCount: kernel.effectiveK,
            diffusionTime: tUse,
            kernelEpsilon: kernel.epsilon,
            featureNeuronCount: featureWidth,
            backend: "diffusion_potential_mds",
            methodLabel: "PHATE"
        )
        return NeuralPopulationPhateResult(points: points, diagnostics: diagnostics)
    }

    // MARK: - Diffusion affinity (mirror of stpd_diffusion_probability, kernel_scale = "local", alpha = 1)

    struct DiffusionKernel {
        let p: [[Double]]        // row-stochastic anisotropic diffusion operator
        let epsilon: Double      // kernel bandwidth ε
        let effectiveK: Int      // local k used for ε
    }

    /// R `stpd_diffusion_probability(X, kernel_scale = "local", n_neighbors, alpha = 1)`: an anisotropic
    /// (`alpha = 1`) diffusion affinity over squared Euclidean distances, with a local bandwidth ε = median of
    /// each row's k-th nearest-neighbour squared distance, returning the row-normalized operator `P`.
    static func diffusionProbability(_ x: [[Double]], neighborCount: Int) -> DiffusionKernel {
        let n = x.count
        // d2 = as.matrix(dist(X))^2 — squared Euclidean (sqrt-then-square, matching R's `dist(...)^2`).
        let euclid = ISIStateSpaceIsomap.euclideanDistanceMatrix(x)
        var d2 = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n {
            for j in 0..<n { d2[i][j] = euclid[i][j] * euclid[i][j] }
        }

        // Local bandwidth: k = max(2, min(n-1, n_neighbors)); ε = median of each row's k-th NN squared distance.
        let k = Swift.max(2, Swift.min(n - 1, neighborCount))
        var kthValues: [Double] = []
        kthValues.reserveCapacity(n)
        for i in 0..<n {
            // R `sort(d2[i, ] + diag(Inf))[k]` — the k-th smallest *off-diagonal* squared distance.
            let offDiagonal = (0..<n).filter { $0 != i }.map { d2[i][$0] }.sorted()
            kthValues.append(offDiagonal[k - 1])
        }
        let finitePositive = kthValues.filter { $0.isFinite && $0 > 0 }
        var epsilon = median(finitePositive)
        if !epsilon.isFinite || epsilon <= 0 { epsilon = 1 }

        // K = exp(-d2/ε); anisotropic normalization K /= outer(q, q) with q = rowSums(K); P = K / rowSums(K).
        var kMatrix = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n {
            for j in 0..<n { kMatrix[i][j] = Foundation.exp(-d2[i][j] / epsilon) }
        }
        var q = kMatrix.map { $0.reduce(0, +) }
        for i in 0..<n where !(q[i].isFinite) || q[i] <= 0 { q[i] = 1 }
        for i in 0..<n {
            for j in 0..<n { kMatrix[i][j] /= (q[i] * q[j]) }   // alpha = 1 → q^alpha = q
        }
        var d = kMatrix.map { $0.reduce(0, +) }
        for i in 0..<n where !(d[i].isFinite) || d[i] <= 0 { d[i] = 1 }
        var p = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n {
            for j in 0..<n { p[i][j] = kMatrix[i][j] / d[i] }   // R `K / d` recycles down columns → divide row i by d[i]
        }
        return DiffusionKernel(p: p, epsilon: epsilon, effectiveK: k)
    }

    // MARK: - Helpers

    /// Dense matrix product `A · B` (both `n × n`), mirroring R `%*%`.
    private static func multiply(_ a: [[Double]], _ b: [[Double]]) -> [[Double]] {
        let n = a.count
        var out = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n {
            for kk in 0..<n {
                let aik = a[i][kk]
                if aik == 0 { continue }
                let bRow = b[kk]
                for j in 0..<n { out[i][j] += aik * bRow[j] }
            }
        }
        return out
    }

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

    /// R `stats::median` (average of the two central order statistics for an even count).
    static func median(_ values: [Double]) -> Double {
        let sorted = values.sorted()
        let count = sorted.count
        guard count > 0 else { return .nan }
        if count % 2 == 1 { return sorted[count / 2] }
        return (sorted[count / 2 - 1] + sorted[count / 2]) / 2
    }
}
