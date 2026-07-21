import Foundation

/// Feature scaling for ISI state-space PCA, mirroring R's `c("robust", "zscore")`
/// (`stpd_scale_matrix_for_pca`). Kept independent of the app-level `ISIStateSpaceScaling` so the pure
/// PCA layer is self-contained.
public enum ISIStatePCAScaling: String, CaseIterable, Sendable, Hashable {
    case robust
    case zscore
}

/// Errors mirroring R `stpd_run_isi_state_pca`'s `stop(...)` guards.
public enum ISIStatePCAError: Error, Equatable, Sendable {
    /// Fewer than two ISI feature rows ("Need at least two ISI feature rows for PCA.").
    case tooFewRows
    /// Fewer than two varying numeric features ("Need at least two varying numeric ISI features for PCA.").
    case tooFewVaryingFeatures
}

/// One PCA score row (mirrors R `scores` PC1..PC3), aligned 1:1 with the input feature rows. PCs beyond
/// the available component count are `nil` (R fills missing PCs with `NA_real_`).
public struct ISIStatePCAScore: Hashable, Sendable {
    public let pc1: Double?
    public let pc2: Double?
    public let pc3: Double?
    public init(pc1: Double?, pc2: Double?, pc3: Double?) {
        self.pc1 = pc1; self.pc2 = pc2; self.pc3 = pc3
    }
}

/// One loadings row (mirrors R `loadings`: `feature`, PC1..PC3 from `pca$rotation`).
public struct ISIStatePCALoading: Hashable, Sendable {
    public let feature: String
    public let pc1: Double?
    public let pc2: Double?
    public let pc3: Double?
    public init(feature: String, pc1: Double?, pc2: Double?, pc3: Double?) {
        self.feature = feature; self.pc1 = pc1; self.pc2 = pc2; self.pc3 = pc3
    }
}

/// One variance row (mirrors R `variance`: `PC`, `variance` = `sdev^2/sum(sdev^2)`, `cumulative` = cumsum).
/// `variance`/`cumulative` are `nil` when the total variance is not finite or `<= 0` (R sets `NA_real_`).
public struct ISIStatePCAVariance: Hashable, Sendable {
    public let component: String
    public let variance: Double?
    public let cumulative: Double?
    public init(component: String, variance: Double?, cumulative: Double?) {
        self.component = component; self.variance = variance; self.cumulative = cumulative
    }
}

/// R-compatible PCA result (mirrors `stpd_run_isi_state_pca`'s returned list, minus the raw `prcomp`).
public struct ISIStatePCAResult: Hashable, Sendable {
    /// One score per input feature row, in input order.
    public let scores: [ISIStatePCAScore]
    /// One loadings row per used feature column.
    public let loadings: [ISIStatePCALoading]
    /// One variance row per component (`min(n, p)`).
    public let variance: [ISIStatePCAVariance]
    /// The varying numeric feature columns actually used, in R selection order.
    public let featureColumns: [String]
    public let scaling: ISIStatePCAScaling
    public init(
        scores: [ISIStatePCAScore], loadings: [ISIStatePCALoading], variance: [ISIStatePCAVariance],
        featureColumns: [String], scaling: ISIStatePCAScaling
    ) {
        self.scores = scores; self.loadings = loadings; self.variance = variance
        self.featureColumns = featureColumns; self.scaling = scaling
    }
}

/// PCA core output on an already-centered/scaled matrix (mirrors `prcomp(center = FALSE, scale. = FALSE)`).
public struct ISIStatePCACore: Sendable {
    /// Scores `X · V`, `n × ncomp` (row-major), `ncomp = min(n, p)`.
    public let scores: [[Double]]
    /// Rotation / loadings `V`, `p × ncomp` (row-major).
    public let loadings: [[Double]]
    /// Eigenvalues of `XᵀX`, descending, clamped to `>= 0` (`= singularValue²`).
    public let eigenvalues: [Double]
    /// `eigenvalue / sum(eigenvalues)` per component; empty when the total is not finite or `<= 0`.
    public let varianceRatios: [Double]
    public init(scores: [[Double]], loadings: [[Double]], eigenvalues: [Double], varianceRatios: [Double]) {
        self.scores = scores; self.loadings = loadings
        self.eigenvalues = eigenvalues; self.varianceRatios = varianceRatios
    }
}

/// Pure R-compatible ISI state-space PCA preparation + core, mirroring `R/55_state_space_pca.R`
/// (`stpd_isi_pca_feature_columns`, `stpd_scale_matrix_for_pca`, `stpd_run_isi_state_pca`). No UI, no detector
/// coupling: the eigendecomposition of the `p × p` Gram matrix (`p` = number of feature columns, small) is
/// delegated to the shared `SymmetricEigensolver` (Accelerate LAPACK `dsyevd`, Jacobi fallback).
public enum ISIStateSpacePCA {
    /// `.Machine$double.eps` — the R scale-degeneracy threshold.
    public static let scaleEpsilon = Double.ulpOfOne

    // MARK: - Feature selection (mirror of stpd_isi_pca_feature_columns)

    /// The PCA feature columns extracted from the rows, in R selection order: `log_isi_feature`, then the
    /// lag columns (ascending offset, so `lag_0` duplicates `log_isi_feature` exactly as in R), then the
    /// `local_*` columns, then `delta_logisi` / `next_delta_logisi` / `prepost_ratio`. Each column is one
    /// value per row, with `nil` features mapped to `.nan` (R `NA`).
    public static func featureColumns(_ rows: [SpikeISIStateSpaceRow]) -> [(name: String, values: [Double])] {
        guard !rows.isEmpty else { return [] }
        var columns: [(name: String, values: [Double])] = []
        func add(_ name: String, _ extract: (SpikeISIStateSpaceRow) -> Double?) {
            columns.append((name, rows.map { extract($0) ?? .nan }))
        }
        add("log_isi_feature") { $0.logISIFeature }
        let offsets = Set(rows.flatMap { $0.lagLogFeature.keys }).sorted()
        for offset in offsets {
            add(SpikeISIStateSpaceFeatureBuilder.lagName(forOffset: offset)) { $0.lag(offset) }
        }
        add("local_median_logisi") { $0.localMedianLogISI }
        add("local_mean_logisi") { $0.localMeanLogISI }
        add("local_sd_logisi") { $0.localSDLogISI }
        add("local_q10_logisi") { $0.localQ10LogISI }
        add("local_q90_logisi") { $0.localQ90LogISI }
        add("local_iqr_logisi") { $0.localIQRLogISI }
        add("local_mean_isi_sec") { $0.localMeanISISec }
        add("local_median_isi_sec") { $0.localMedianISISec }
        add("local_rate_hz") { $0.localRateHz }
        add("local_cv") { $0.localCV }
        add("local_lv") { $0.localLV }
        add("local_cv2") { $0.localCV2 }
        add("delta_logisi") { $0.deltaLogISI }
        add("next_delta_logisi") { $0.nextDeltaLogISI }
        add("prepost_ratio") { $0.prepostRatio }
        return columns
    }

    /// R varying filter: a column qualifies when it has `>= 2` finite values AND `>= 2` distinct values
    /// at 12 significant figures (`length(unique(signif(x, 12))) >= 2`).
    public static func isVaryingColumn(_ values: [Double]) -> Bool {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2 else { return false }
        var distinct = Set<Double>()
        for value in finite {
            distinct.insert(signif(value, 12))
            if distinct.count >= 2 { return true }
        }
        return false
    }

    // MARK: - Scaling (mirror of stpd_scale_matrix_for_pca)

    /// Per-column median-impute non-finite cells (fallback `0`), then center/scale. Robust = median center
    /// + `mad(constant = 1.4826)` scale with sample-SD fallback when MAD is non-finite or `<= eps`; zscore
    /// = mean center + sample-SD scale. Any remaining non-finite / `<= eps` scale becomes `1`. Input is
    /// row-major `n × p`; non-finite entries are treated as `NA`.
    public static func scaleMatrixForPCA(_ matrix: [[Double]], scaling: ISIStatePCAScaling) -> [[Double]] {
        let n = matrix.count
        guard n > 0 else { return matrix }
        let p = matrix[0].count
        guard p > 0 else { return matrix }

        // Column-major working copy with non-finite cells imputed by the column median (fallback 0).
        var columns: [[Double]] = (0..<p).map { j in
            let raw = matrix.map { $0[j] }
            let med = median(raw.filter(\.isFinite))
            let fill = (med?.isFinite ?? false) ? med! : 0
            return raw.map { $0.isFinite ? $0 : fill }
        }

        var centers = [Double](repeating: 0, count: p)
        var scales = [Double](repeating: 1, count: p)
        for j in 0..<p {
            let col = columns[j]
            switch scaling {
            case .robust:
                centers[j] = median(col) ?? 0
                var scale = medianAbsoluteDeviation(col) ?? .nan
                if !scale.isFinite || scale <= scaleEpsilon {
                    scale = sampleStandardDeviation(col) ?? .nan   // R sd fallback
                }
                scales[j] = scale
            case .zscore:
                centers[j] = STPDStatistics.mean(col) ?? 0
                scales[j] = sampleStandardDeviation(col) ?? .nan
            }
        }
        for j in 0..<p where !scales[j].isFinite || scales[j] <= scaleEpsilon {
            scales[j] = 1
        }

        var output = matrix
        for i in 0..<n {
            for j in 0..<p {
                output[i][j] = (columns[j][i] - centers[j]) / scales[j]
            }
        }
        return output
    }

    // MARK: - PCA core (mirror of prcomp(center = FALSE, scale. = FALSE))

    /// SVD-equivalent PCA on an already-centered/scaled matrix via symmetric eigendecomposition of the
    /// Gram matrix `G = XᵀX`. Returns scores `X·V`, rotation `V`, eigenvalues (`= singularValue²`), and
    /// `eigenvalue / total` variance ratios. Eigenvectors use a deterministic sign convention (the
    /// largest-magnitude component is positive) so results are reproducible; cross-implementation
    /// comparison with R must still be sign/reflection invariant.
    public static func principalComponents(scaledMatrix: [[Double]]) -> ISIStatePCACore {
        let n = scaledMatrix.count
        guard n > 0 else { return ISIStatePCACore(scores: [], loadings: [], eigenvalues: [], varianceRatios: []) }
        let p = scaledMatrix[0].count
        guard p > 0 else { return ISIStatePCACore(scores: [], loadings: [], eigenvalues: [], varianceRatios: []) }

        // Gram matrix G = XᵀX (p × p, symmetric).
        var gram = [[Double]](repeating: [Double](repeating: 0, count: p), count: p)
        for a in 0..<p {
            for b in a..<p {
                var sum = 0.0
                for i in 0..<n { sum += scaledMatrix[i][a] * scaledMatrix[i][b] }
                gram[a][b] = sum
                gram[b][a] = sum
            }
        }

        let (values, vectors) = SymmetricEigensolver.decompose(gram)   // descending, vectors as columns
        let ncomp = min(n, p)
        let eigenvalues = Array(values.prefix(ncomp)).map { Swift.max(0, $0) }

        var loadings = [[Double]](repeating: [Double](repeating: 0, count: ncomp), count: p)
        for j in 0..<p {
            for c in 0..<ncomp { loadings[j][c] = vectors[j][c] }
        }

        var scores = [[Double]](repeating: [Double](repeating: 0, count: ncomp), count: n)
        for i in 0..<n {
            for c in 0..<ncomp {
                var sum = 0.0
                for j in 0..<p { sum += scaledMatrix[i][j] * loadings[j][c] }
                scores[i][c] = sum
            }
        }

        let total = eigenvalues.reduce(0, +)
        let ratios: [Double] = (total.isFinite && total > 0) ? eigenvalues.map { $0 / total } : []
        return ISIStatePCACore(scores: scores, loadings: loadings, eigenvalues: eigenvalues, varianceRatios: ratios)
    }

    // MARK: - Orchestrator (mirror of stpd_run_isi_state_pca)

    /// Build the R-compatible PCA result from ISI state-space feature rows: select PCA feature columns,
    /// drop non-varying ones, scale, and run the PCA core. Throws `tooFewRows` (`< 2` rows) or
    /// `tooFewVaryingFeatures` (`< 2` varying columns), mirroring R's `stop(...)` guards.
    public static func run(
        rows: [SpikeISIStateSpaceRow],
        scaling: ISIStatePCAScaling
    ) throws -> ISIStatePCAResult {
        guard rows.count >= 2 else { throw ISIStatePCAError.tooFewRows }
        let varying = featureColumns(rows).filter { isVaryingColumn($0.values) }
        guard varying.count >= 2 else { throw ISIStatePCAError.tooFewVaryingFeatures }

        let names = varying.map(\.name)
        let n = rows.count
        let p = varying.count
        var matrix = [[Double]](repeating: [Double](repeating: .nan, count: p), count: n)
        for j in 0..<p {
            let column = varying[j].values
            for i in 0..<n { matrix[i][j] = column[i] }
        }

        let scaled = scaleMatrixForPCA(matrix, scaling: scaling)
        let core = principalComponents(scaledMatrix: scaled)
        let ncomp = core.eigenvalues.count

        func componentValue(_ row: [Double], _ index: Int) -> Double? {
            index < ncomp ? row[index] : nil
        }

        let scores = core.scores.map { row in
            ISIStatePCAScore(
                pc1: componentValue(row, 0), pc2: componentValue(row, 1), pc3: componentValue(row, 2)
            )
        }
        let loadings = (0..<p).map { j in
            ISIStatePCALoading(
                feature: names[j],
                pc1: componentValue(core.loadings[j], 0),
                pc2: componentValue(core.loadings[j], 1),
                pc3: componentValue(core.loadings[j], 2)
            )
        }

        let total = core.eigenvalues.reduce(0, +)
        let totalValid = total.isFinite && total > 0
        var variance: [ISIStatePCAVariance] = []
        var cumulative = 0.0
        for index in 0..<ncomp {
            let ratio = totalValid ? core.eigenvalues[index] / total : .nan
            if totalValid { cumulative += ratio }
            variance.append(ISIStatePCAVariance(
                component: "PC\(index + 1)",
                variance: totalValid ? ratio : nil,
                cumulative: totalValid ? cumulative : nil
            ))
        }

        return ISIStatePCAResult(
            scores: scores, loadings: loadings, variance: variance,
            featureColumns: names, scaling: scaling
        )
    }

    // MARK: - Pure numeric helpers

    /// Round to `digits` significant figures (R `signif`), used only for the varying-column distinctness
    /// test, so the exact half-way rounding rule does not affect clearly distinct / clearly constant data.
    private static func signif(_ x: Double, _ digits: Int) -> Double {
        guard x != 0, x.isFinite else { return x }
        let magnitude = ceil(log10(abs(x)))
        let factor = pow(10.0, Double(digits) - magnitude)
        return (x * factor).rounded() / factor
    }

    /// Type-7 median over the finite values (R `stats::median`).
    private static func median(_ values: [Double]) -> Double? {
        SortedFiniteSample(values).quantile(0.5)
    }

    /// `constant * median(|x - median(x)|)` over finite values (R `stats::mad`, default constant 1.4826).
    private static func medianAbsoluteDeviation(_ values: [Double], constant: Double = 1.4826) -> Double? {
        let finite = values.filter(\.isFinite)
        guard let center = median(finite) else { return nil }
        let deviations = finite.map { abs($0 - center) }
        guard let medianDeviation = median(deviations) else { return nil }
        return constant * medianDeviation
    }

    /// Sample standard deviation (`n - 1`), matching R `stats::sd`; `nil` for `< 2` finite values.
    private static func sampleStandardDeviation(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2, let mean = STPDStatistics.mean(finite) else { return nil }
        let sumSquares = finite.reduce(0.0) { partial, value in
            let delta = value - mean
            return partial + delta * delta
        }
        return (sumSquares / Double(finite.count - 1)).squareRoot()
    }

}
