import Foundation

/// Errors mirroring the R neural-manifold FA / GPFA guards (`R/61` `stpd_run_neural_manifold_embedding` +
/// `stats::factanal`).
public enum NeuralPopulationFactorError: Error, Equatable, Sendable {
    /// Fewer than three population time bins (cannot form a correlation / standardize).
    case tooFewBins
    /// Fewer than three usable (post rank-drop) feature columns — `factanal` requires ≥ 3 variables, and the
    /// auto factor count would be 0.
    case tooFewFeatures
    /// The model is not identified for the available bins: `nrow < n_factors + 3` (R's
    /// `stop("Not enough bins/features…")`).
    case notIdentified
    /// Every maximum-likelihood fit attempt (from the max factor count down to one) failed to converge.
    case fitFailed
}

/// One embedded population bin: the bin (preserving `binID` + times) plus its factor scores NM1–NM3. Coordinates
/// come only from the binned population activity (`matrix.scaled`) — never from detector/event labels. Following
/// R `stpd_neural_take3`, dimensions beyond the fitted factor count are `0` (not `nil`).
public struct NeuralPopulationFactorPoint: Hashable, Sendable {
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

/// One loadings row per **kept** train/neuron column (mirrors R `fit$loadings` + `fit$uniquenesses`). `f1–f3` are
/// `nil` for factors beyond the fitted count (R pads with `NA`). `uniqueness` is the feature-specific private
/// variance Ψ (lower ⇒ more shared population structure).
public struct NeuralPopulationFactorLoading: Sendable, Hashable {
    public let trainID: String
    public let trainName: String
    public let f1: Double?
    public let f2: Double?
    public let f3: Double?
    public let uniqueness: Double

    public init(trainID: String, trainName: String, f1: Double?, f2: Double?, f3: Double?, uniqueness: Double) {
        self.trainID = trainID
        self.trainName = trainName
        self.f1 = f1
        self.f2 = f2
        self.f3 = f3
        self.uniqueness = uniqueness
    }
}

/// Diagnostics mirroring the R FA/GPFA `diagnostics` (`n_factors`, `mean_uniqueness`) plus the rank-drop /
/// smoothing context the UI panel shows.
public struct NeuralPopulationFactorDiagnostics: Sendable, Hashable {
    public let inputBinCount: Int
    public let inputFeatureCount: Int       // selected train columns before rank-drop
    public let keptFeatureCount: Int        // columns surviving the QR rank-drop (= loadings rows)
    public let droppedFeatureCount: Int
    public let factorCount: Int             // R "n_factors"
    public let meanUniqueness: Double        // R "mean_uniqueness"
    public let smoothed: Bool               // GPFA-style (columns Gaussian-smoothed) vs plain FA
    public let methodLabel: String          // "FA" or "GPFA-style smooth FA"

    public init(
        inputBinCount: Int, inputFeatureCount: Int, keptFeatureCount: Int, droppedFeatureCount: Int,
        factorCount: Int, meanUniqueness: Double, smoothed: Bool, methodLabel: String
    ) {
        self.inputBinCount = inputBinCount
        self.inputFeatureCount = inputFeatureCount
        self.keptFeatureCount = keptFeatureCount
        self.droppedFeatureCount = droppedFeatureCount
        self.factorCount = factorCount
        self.meanUniqueness = meanUniqueness
        self.smoothed = smoothed
        self.methodLabel = methodLabel
    }
}

/// R-compatible Neural Manifold FA / GPFA-style result. `points` are 1:1 with the input bins (FA/GPFA do NOT
/// subsample); `loadings` are 1:1 with the kept train columns.
public struct NeuralPopulationFactorResult: Sendable {
    public let points: [NeuralPopulationFactorPoint]
    public let loadings: [NeuralPopulationFactorLoading]
    public let diagnostics: NeuralPopulationFactorDiagnostics

    public init(
        points: [NeuralPopulationFactorPoint],
        loadings: [NeuralPopulationFactorLoading],
        diagnostics: NeuralPopulationFactorDiagnostics
    ) {
        self.points = points
        self.loadings = loadings
        self.diagnostics = diagnostics
    }
}

/// Pure, R-compatible Neural Manifold **Factor Analysis** (and the **GPFA-style** smoothed variant), mirroring
/// `R/61` `stpd_run_neural_manifold_embedding(method = "fa" | "gpfa")`, which delegates to
/// `stats::factanal(X, factors = …, scores = "regression", rotation = "none", nstart = 20)`.
///
/// It embeds the **full** scaled population matrix (`matrix.scaled` = R `pop$X`; bins are points, neurons are
/// features). FA/GPFA deliberately **do not subsample** (`max_points` is unused — R's dispatch passes the full
/// `X`, not the sampled `X0`).
///
/// Pipeline (mirror of the R FA/GPFA branch):
/// 1. (GPFA only) Gaussian-smooth each column with `σ = max(1, smoothingSigmaBins)` (R
///    `stpd_state_trajectory_gaussian_smooth`);
/// 2. QR rank-drop of collinear columns (R `qr()` pivot keep — see the deviation note on `rankDropColumns`);
/// 3. auto factor count `m ≤ maxFactors` from the identifiability rule
///    (`stpd_state_trajectory_max_fa_factors`);
/// 4. maximum-likelihood factor fit on the correlation matrix (`factanal`'s exact concentrated-likelihood
///    objective over the uniquenesses Ψ, reusing the shared `SymmetricEigensolver` for the inner eigenproblem),
///    trying `m, m−1, … 1` and keeping the first that converges;
/// 5. `sortLoadings` (factanal's deterministic factor order + sign convention);
/// 6. Thomson regression scores `Z_std · R⁻¹ · Λ`, padded to NM1–NM3.
///
/// **Determinism / parity:** the ML optimum is start-independent for well-conditioned data (empirically R's
/// `nstart = 20` and a single deterministic start agree to ~1e-4), so this uses factanal's **deterministic**
/// start `Ψ₀ = (1 − 0.5·m/p) / diag(R⁻¹)` and **does not** replicate R's `runif` restarts. Coordinates/loadings
/// match R up to per-factor **sign** (the `sortLoadings` convention fixes order + the column-sum sign) — verified
/// against R 4.6.0 reference fixtures in `NeuralPopulationFactorAnalysisTests`.
public enum NeuralPopulationFactorAnalysis {

    /// R `factanal` uniqueness lower bound (`control$lower`).
    private static let uniquenessLowerBound = 0.005

    /// - Parameters:
    ///   - matrix: the population matrix; `matrix.scaled` is embedded in full.
    ///   - smoothingSigmaBins: `nil` ⇒ plain FA; non-nil ⇒ GPFA-style (columns Gaussian-smoothed with
    ///     `max(1, value)`, mirroring R `sigma = max(1, pop$smoothing_sigma_bins)`).
    ///   - maxFactors: cap on the factor count (R hard-codes 3).
    public static func run(
        matrix: NeuralPopulationMatrix,
        smoothingSigmaBins: Double? = nil,
        maxFactors: Int = 3
    ) throws -> NeuralPopulationFactorResult {
        let scaled = matrix.scaled
        let bins = matrix.bins
        guard scaled.count >= 3, scaled.count == bins.count else { throw NeuralPopulationFactorError.tooFewBins }
        let n = scaled.count
        let inputFeatureCount = scaled.first?.count ?? 0

        // 1. GPFA: Gaussian-smooth each column (R only when nrow >= 3, already guaranteed).
        let smoothed = smoothingSigmaBins != nil
        var processed = scaled
        if let sigma = smoothingSigmaBins {
            let effectiveSigma = Swift.max(1, sigma)
            processed = smoothColumns(scaled, sigmaBins: effectiveSigma)
        }

        // 2. QR rank-drop of collinear columns (R `qr(X_fa)` pivot keep).
        let keptColumns = rankDropColumns(processed)
        let p = keptColumns.count
        guard p >= 3 else { throw NeuralPopulationFactorError.tooFewFeatures }

        // Kept feature matrix (n × p), still bins × trains.
        let kept = processed.map { row in keptColumns.map { row[$0] } }

        // 3. Auto factor count.
        let factorCount = maxFactorCount(featureCount: p, maxFactors: maxFactors)
        guard factorCount >= 1 else { throw NeuralPopulationFactorError.tooFewFeatures }
        guard n >= factorCount + 3 else { throw NeuralPopulationFactorError.notIdentified }

        // Correlation matrix (R `cv <- cov.wt(z)$cov; cv <- cv / (sds %o% sds)` = Pearson correlation).
        guard let correlation = correlationMatrix(kept) else { throw NeuralPopulationFactorError.fitFailed }
        guard let correlationInverse = invert(correlation) else { throw NeuralPopulationFactorError.fitFailed }

        // 4. Try m, m−1, … 1; keep the first ML fit that converges (R's `for (nf in rev(seq_len(n_fac)))`).
        var fitted: (loadings: [[Double]], uniqueness: [Double], factors: Int)?
        var nf = factorCount
        while nf >= 1 {
            if let result = fitMaximumLikelihood(
                correlation: correlation, correlationInverse: correlationInverse, factors: nf
            ) {
                fitted = (result.loadings, result.uniqueness, nf)
                break
            }
            nf -= 1
        }
        guard let fit = fitted else { throw NeuralPopulationFactorError.fitFailed }

        // 5. factanal `sortLoadings`: order factors by descending column SS, flip sign so each column sums ≥ 0.
        let loadings = sortLoadings(fit.loadings)
        let q = fit.factors

        // 6. Regression (Thomson) scores: `Z_std · R⁻¹ · Λ`.
        let standardized = standardizeColumns(kept)                      // n × p
        let inverseTimesLoadings = multiply(correlationInverse, loadings) // p × q
        let scores = multiply(standardized, inverseTimesLoadings)         // n × q

        // Map to bin-aligned points (take3 → pad missing dims with 0).
        let points = (0..<n).map { i -> NeuralPopulationFactorPoint in
            NeuralPopulationFactorPoint(
                bin: bins[i],
                nm1: q >= 1 ? scores[i][0] : 0,
                nm2: q >= 2 ? scores[i][1] : 0,
                nm3: q >= 3 ? scores[i][2] : 0
            )
        }

        // Loadings rows for the kept columns.
        let loadingRows = keptColumns.enumerated().map { keptIndex, originalColumn -> NeuralPopulationFactorLoading in
            NeuralPopulationFactorLoading(
                trainID: matrix.trainIDs[originalColumn],
                trainName: matrix.trainNames[originalColumn],
                f1: q >= 1 ? loadings[keptIndex][0] : nil,
                f2: q >= 2 ? loadings[keptIndex][1] : nil,
                f3: q >= 3 ? loadings[keptIndex][2] : nil,
                uniqueness: fit.uniqueness[keptIndex]
            )
        }

        let meanUniqueness = fit.uniqueness.reduce(0, +) / Double(p)
        let diagnostics = NeuralPopulationFactorDiagnostics(
            inputBinCount: n,
            inputFeatureCount: inputFeatureCount,
            keptFeatureCount: p,
            droppedFeatureCount: inputFeatureCount - p,
            factorCount: q,
            meanUniqueness: meanUniqueness,
            smoothed: smoothed,
            methodLabel: smoothed ? "GPFA-style smooth FA" : "FA"
        )
        return NeuralPopulationFactorResult(points: points, loadings: loadingRows, diagnostics: diagnostics)
    }

    // MARK: - Factor count (mirror of stpd_state_trajectory_max_fa_factors)

    /// R `stpd_state_trajectory_max_fa_factors(p, max_factors)`: the largest `m ∈ 1…min(maxFactors, p−1)` whose
    /// FA degrees of freedom `((p − m)² − p − m) / 2 ≥ 0`; `0` if none.
    static func maxFactorCount(featureCount p: Int, maxFactors: Int) -> Int {
        let cap = Swift.max(1, maxFactors)
        let upper = Swift.min(cap, Swift.max(0, p - 1))
        guard upper >= 1 else { return 0 }
        var best = 0
        for m in 1...upper where (p - m) * (p - m) - p - m >= 0 { best = m }
        return best
    }

    // MARK: - Gaussian smoothing (mirror of stpd_state_trajectory_gaussian_smooth)

    /// Per-column truncated-Gaussian smoothing (R `radius = max(1, ceil(3σ))`, weights `exp(-0.5·((j−i)/σ)²)`,
    /// normalized over finite samples). Columns are time series (bins down each column).
    private static func smoothColumns(_ matrix: [[Double]], sigmaBins: Double) -> [[Double]] {
        guard sigmaBins > 0, matrix.count >= 3, let width = matrix.first?.count, width > 0 else { return matrix }
        let n = matrix.count
        let radius = Swift.max(1, Int((3 * sigmaBins).rounded(.up)))
        var out = matrix
        for j in 0..<width {
            for i in 0..<n {
                let lo = Swift.max(0, i - radius)
                let hi = Swift.min(n - 1, i + radius)
                var weighted = 0.0
                var weightSum = 0.0
                for k in lo...hi {
                    let value = matrix[k][j]
                    guard value.isFinite else { continue }
                    let delta = Double(k - i) / sigmaBins
                    let w = Foundation.exp(-0.5 * delta * delta)
                    weighted += value * w
                    weightSum += w
                }
                out[i][j] = weightSum > 0 ? weighted / weightSum : .nan
            }
        }
        return out
    }

    // MARK: - Rank-drop of collinear columns (approximation of R qr() pivot keep)

    /// Drop collinear columns, returning the kept original column indices in ascending order (R `qr(X_fa)` →
    /// `sort(pivot[1:rank])`). Implemented as a left-to-right modified Gram–Schmidt: a column is kept iff its
    /// residual norm after projecting out the already-kept (orthonormalized) columns exceeds `tol` × its own
    /// norm (`tol = 1e-7`, matching R `qr`'s default relative tolerance).
    ///
    /// Deviation (documented): R's `qr()` LINPACK `dqrdc2` uses a specific limited-pivoting rule; on exact /
    /// near collinearity the dropped-column choice can differ. For full-rank input (the common z-scored case)
    /// every column is kept and the result is identical.
    static func rankDropColumns(_ matrix: [[Double]]) -> [Int] {
        guard let width = matrix.first?.count, width > 0 else { return [] }
        let n = matrix.count
        let tol = 1e-7
        var basis: [[Double]] = []   // orthonormal kept-column directions (each length n)
        var kept: [Int] = []
        for j in 0..<width {
            var column = (0..<n).map { matrix[$0][j] }
            let originalNorm = norm(column)
            guard originalNorm > 0 else { continue }
            for b in basis {
                let projection = dot(column, b)
                for i in 0..<n { column[i] -= projection * b[i] }
            }
            let residualNorm = norm(column)
            if residualNorm > tol * originalNorm {
                let inv = 1.0 / residualNorm
                basis.append(column.map { $0 * inv })
                kept.append(j)
            }
        }
        return kept
    }

    // MARK: - Correlation / standardization

    /// Pearson correlation matrix of the columns (R `cor`). `nil` if any column has zero variance.
    static func correlationMatrix(_ matrix: [[Double]]) -> [[Double]]? {
        let n = matrix.count
        guard n >= 2, let p = matrix.first?.count, p > 0 else { return nil }
        var centered = [[Double]](repeating: [Double](repeating: 0, count: p), count: n)
        var sd = [Double](repeating: 0, count: p)
        for j in 0..<p {
            var mean = 0.0
            for i in 0..<n { mean += matrix[i][j] }
            mean /= Double(n)
            var ss = 0.0
            for i in 0..<n {
                let c = matrix[i][j] - mean
                centered[i][j] = c
                ss += c * c
            }
            let variance = ss / Double(n - 1)
            guard variance > 0 else { return nil }
            sd[j] = variance.squareRoot()
        }
        var correlation = [[Double]](repeating: [Double](repeating: 0, count: p), count: p)
        for a in 0..<p {
            for b in a..<p {
                var cov = 0.0
                for i in 0..<n { cov += centered[i][a] * centered[i][b] }
                cov /= Double(n - 1)
                let value = cov / (sd[a] * sd[b])
                correlation[a][b] = value
                correlation[b][a] = value
            }
        }
        return correlation
    }

    /// R `scale(z, TRUE, TRUE)`: column-center then divide by the sample SD (divisor `n − 1`).
    static func standardizeColumns(_ matrix: [[Double]]) -> [[Double]] {
        let n = matrix.count
        guard n >= 2, let p = matrix.first?.count, p > 0 else { return matrix }
        var out = [[Double]](repeating: [Double](repeating: 0, count: p), count: n)
        for j in 0..<p {
            var mean = 0.0
            for i in 0..<n { mean += matrix[i][j] }
            mean /= Double(n)
            var ss = 0.0
            for i in 0..<n { let c = matrix[i][j] - mean; ss += c * c }
            let sd = (ss / Double(n - 1)).squareRoot()
            let inv = sd > 0 ? 1.0 / sd : 0.0
            for i in 0..<n { out[i][j] = (matrix[i][j] - mean) * inv }
        }
        return out
    }

    // MARK: - Maximum-likelihood factor fit (mirror of factanal.fit.mle)

    /// Fit the ML factor model for `factors` factors on the correlation matrix; returns the (unsorted) loadings
    /// and uniquenesses, or `nil` when the bounded optimizer fails to converge.
    private static func fitMaximumLikelihood(
        correlation: [[Double]], correlationInverse: [[Double]], factors q: Int
    ) -> (loadings: [[Double]], uniqueness: [Double])? {
        let p = correlation.count
        guard q >= 1, q < p else { return nil }

        // Deterministic start Ψ₀ = (1 − 0.5·q/p) / diag(R⁻¹), clamped to [lower, 1] (factanal default start).
        let base = 1 - 0.5 * Double(q) / Double(p)
        var psi = (0..<p).map { clamp(base / correlationInverse[$0][$0], uniquenessLowerBound, 1) }

        // Spectral projected gradient (box [lower, 1]) on factanal's concentrated objective FAfn, analytic FAgr,
        // with a Grippo–Lampariello–Lucidi non-monotone line search (the canonical SPG — lets the
        // Barzilai–Borwein step converge fast even on the near-collinear / Heywood-boundary landscape).
        var previousPsi: [Double]?
        var previousGrad: [Double]?
        var alpha = 1.0
        var lastProjectedGradientNorm = Double.infinity
        var recentObjectives: [Double] = []        // window for the non-monotone reference
        let memory = 10
        var stagnation = 0
        let maxIterations = 1000
        for _ in 0..<maxIterations {
            guard let (objective, gradient) = objectiveAndGradient(psi, correlation: correlation, factors: q) else {
                return nil
            }
            // Projected-gradient stopping criterion.
            lastProjectedGradientNorm = (0..<p)
                .map { abs(clamp(psi[$0] - gradient[$0], uniquenessLowerBound, 1) - psi[$0]) }
                .max() ?? 0
            if lastProjectedGradientNorm < 1e-7 { break }

            // Barzilai–Borwein step length.
            if let pp = previousPsi, let pg = previousGrad {
                var sy = 0.0, ss = 0.0
                for i in 0..<p {
                    let s = psi[i] - pp[i]
                    let y = gradient[i] - pg[i]
                    sy += s * y
                    ss += s * s
                }
                alpha = sy > 1e-30 ? clamp(ss / sy, 1e-10, 1e10) : 1.0
            }

            // Projected direction and non-monotone Armijo line search (reference = max of the last `memory` f).
            recentObjectives.append(objective)
            if recentObjectives.count > memory { recentObjectives.removeFirst() }
            let reference = recentObjectives.max() ?? objective
            let direction = (0..<p).map { clamp(psi[$0] - alpha * gradient[$0], uniquenessLowerBound, 1) - psi[$0] }
            let directionalDerivative = (0..<p).reduce(0.0) { $0 + gradient[$1] * direction[$1] }
            var lambda = 1.0
            var accepted: [Double]?
            var acceptedObjective = objective
            for _ in 0..<40 {
                let candidate = (0..<p).map { psi[$0] + lambda * direction[$0] }
                let candidateObjective = objectiveValue(candidate, correlation: correlation, factors: q)
                if candidateObjective <= reference + 1e-4 * lambda * directionalDerivative {
                    accepted = candidate
                    acceptedObjective = candidateObjective
                    break
                }
                lambda *= 0.5
            }
            guard let nextPsi = accepted else { break }   // no further decrease ⇒ at the optimum
            // Objective-stagnation stop (robust near a boundary-active optimum where the projected gradient plateaus).
            if abs(objective - acceptedObjective) <= 1e-12 * (1 + abs(objective)) {
                stagnation += 1
                if stagnation >= 3 { psi = nextPsi; break }
            } else {
                stagnation = 0
            }
            previousPsi = psi
            previousGrad = gradient
            psi = nextPsi
        }

        guard lastProjectedGradientNorm < 1e-5 else { return nil }
        guard let loadings = factorLoadings(psi, correlation: correlation, factors: q) else { return nil }
        return (loadings, psi)
    }

    /// `S* = Ψ^(−1/2) · R · Ψ^(−1/2)` (factanal's `Sstar`).
    private static func sStar(_ correlation: [[Double]], psi: [Double]) -> [[Double]] {
        let p = correlation.count
        let invSqrt = psi.map { 1.0 / $0.squareRoot() }
        var out = [[Double]](repeating: [Double](repeating: 0, count: p), count: p)
        for i in 0..<p {
            for j in 0..<p { out[i][j] = correlation[i][j] * invSqrt[i] * invSqrt[j] }
        }
        return out
    }

    /// factanal `FAfn`: `-(Σ_{tail}(log e − e) − q + p)` over the smallest `p − q` eigenvalues of `S*`. Returns
    /// `+∞` when an eigenvalue is non-positive (invalid).
    private static func objectiveValue(_ psi: [Double], correlation: [[Double]], factors q: Int) -> Double {
        let p = correlation.count
        let (values, _) = SymmetricEigensolver.decompose(sStar(correlation, psi: psi))
        var objective = Double(q) - Double(p)
        for c in q..<p {
            let e = values[c]
            if e <= 0 || !e.isFinite { return .infinity }
            objective += e - Foundation.log(e)
        }
        return objective
    }

    /// factanal `FAfn` + `FAgr` together (the gradient reuses the eigenvectors). `nil` on an invalid eigenvalue.
    private static func objectiveAndGradient(
        _ psi: [Double], correlation: [[Double]], factors q: Int
    ) -> (objective: Double, gradient: [Double])? {
        let p = correlation.count
        let (values, vectors) = SymmetricEigensolver.decompose(sStar(correlation, psi: psi))
        var objective = Double(q) - Double(p)
        for c in q..<p {
            let e = values[c]
            if e <= 0 || !e.isFinite { return nil }
            objective += e - Foundation.log(e)
        }
        // load[i][c] = sqrt(Ψ_i) · vector_c[i] · sqrt(max(e_c − 1, 0)); FAgr: diag(load·loadᵀ + Ψ − R) / Ψ².
        let sqrtPsi = psi.map { $0.squareRoot() }
        var gradient = [Double](repeating: 0, count: p)
        for i in 0..<p {
            var sumSquares = 0.0
            for c in 0..<q {
                let scale = values[c] > 1 ? (values[c] - 1).squareRoot() : 0
                let load = sqrtPsi[i] * vectors[i][c] * scale
                sumSquares += load * load
            }
            let g = sumSquares + psi[i] - correlation[i][i]
            gradient[i] = g / (psi[i] * psi[i])
        }
        return (objective, gradient)
    }

    /// factanal `FAout`: loadings `Λ = Ψ^(1/2) · V_{1..q} · diag(sqrt(max(e_{1..q} − 1, 0)))`.
    private static func factorLoadings(_ psi: [Double], correlation: [[Double]], factors q: Int) -> [[Double]]? {
        let p = correlation.count
        let (values, vectors) = SymmetricEigensolver.decompose(sStar(correlation, psi: psi))
        let sqrtPsi = psi.map { $0.squareRoot() }
        var loadings = [[Double]](repeating: [Double](repeating: 0, count: q), count: p)
        for c in 0..<q {
            let scale = values[c] > 1 ? (values[c] - 1).squareRoot() : 0
            for i in 0..<p {
                let value = sqrtPsi[i] * vectors[i][c] * scale
                if !value.isFinite { return nil }
                loadings[i][c] = value
            }
        }
        return loadings
    }

    /// factanal `sortLoadings` (rotation = "none"): order factors by descending column sum-of-squares, then flip
    /// the sign of any column whose loadings sum is negative.
    static func sortLoadings(_ loadings: [[Double]]) -> [[Double]] {
        let p = loadings.count
        guard p > 0 else { return loadings }
        let q = loadings[0].count
        var columnSumSquares = [Double](repeating: 0, count: q)
        var columnSums = [Double](repeating: 0, count: q)
        for c in 0..<q {
            for i in 0..<p {
                columnSumSquares[c] += loadings[i][c] * loadings[i][c]
                columnSums[c] += loadings[i][c]
            }
        }
        let order = (0..<q).sorted {
            columnSumSquares[$0] != columnSumSquares[$1] ? columnSumSquares[$0] > columnSumSquares[$1] : $0 < $1
        }
        var out = [[Double]](repeating: [Double](repeating: 0, count: q), count: p)
        for (newColumn, oldColumn) in order.enumerated() {
            let sign = columnSums[oldColumn] < 0 ? -1.0 : 1.0
            for i in 0..<p { out[i][newColumn] = loadings[i][oldColumn] * sign }
        }
        return out
    }

    // MARK: - Small dense linear algebra (p × p, p small)

    private static func clamp(_ value: Double, _ low: Double, _ high: Double) -> Double {
        Swift.min(Swift.max(value, low), high)
    }

    private static func dot(_ a: [Double], _ b: [Double]) -> Double {
        var sum = 0.0
        for i in 0..<a.count { sum += a[i] * b[i] }
        return sum
    }

    private static func norm(_ a: [Double]) -> Double { dot(a, a).squareRoot() }

    /// Dense matrix product `A (r × k) · B (k × c)`.
    private static func multiply(_ a: [[Double]], _ b: [[Double]]) -> [[Double]] {
        let r = a.count
        guard r > 0, let k = a.first?.count, k > 0, b.count == k, let c = b.first?.count else { return [] }
        var out = [[Double]](repeating: [Double](repeating: 0, count: c), count: r)
        for i in 0..<r {
            for t in 0..<k {
                let ait = a[i][t]
                if ait == 0 { continue }
                let bRow = b[t]
                for j in 0..<c { out[i][j] += ait * bRow[j] }
            }
        }
        return out
    }

    /// Inverse of a square matrix via Gauss–Jordan with partial pivoting; `nil` if singular.
    static func invert(_ matrix: [[Double]]) -> [[Double]]? {
        let n = matrix.count
        guard n > 0, matrix.allSatisfy({ $0.count == n }) else { return nil }
        var a = matrix
        var inverse = (0..<n).map { i in (0..<n).map { $0 == i ? 1.0 : 0.0 } }
        for col in 0..<n {
            var pivotRow = col
            var pivotValue = abs(a[col][col])
            for row in (col + 1)..<n where abs(a[row][col]) > pivotValue {
                pivotValue = abs(a[row][col])
                pivotRow = row
            }
            guard pivotValue > 1e-12 else { return nil }
            if pivotRow != col {
                a.swapAt(pivotRow, col)
                inverse.swapAt(pivotRow, col)
            }
            let pivot = a[col][col]
            for j in 0..<n {
                a[col][j] /= pivot
                inverse[col][j] /= pivot
            }
            for row in 0..<n where row != col {
                let factor = a[row][col]
                if factor == 0 { continue }
                for j in 0..<n {
                    a[row][j] -= factor * a[col][j]
                    inverse[row][j] -= factor * inverse[col][j]
                }
            }
        }
        return inverse
    }
}
