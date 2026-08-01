import Foundation
import STPDCore
import Testing

// P2A: pure R-compatible ISI state-space PCA preparation + core
// (mirror of stpd_isi_pca_feature_columns / stpd_scale_matrix_for_pca / stpd_run_isi_state_pca).

private func train(_ name: String, _ timestamps: [Double]) -> SpikeTrain {
    SpikeTrain(name: name, timestampsSec: timestamps)
}

private func featureRows(_ timestamps: [Double], k: Int = 3) -> [SpikeISIStateSpaceRow] {
    SpikeISIStateSpaceFeatureBuilder.makeRows(train: train("t", timestamps), k: k, winsorize: false)
}

// R test fixture: a mixed tonic/burst/pause-ish ISI motif so many features vary.
private let mixedISIs: [Double] = [
    0.030, 0.032, 0.031, 0.029, 0.006, 0.007, 0.008, 0.130,
    0.026, 0.028, 0.027, 0.012, 0.013, 0.014, 0.016
]
private func mixedTimestamps() -> [Double] {
    var ts = [0.0]
    for isi in mixedISIs { ts.append(ts.last! + isi) }
    return ts
}

// MARK: - 1. Scaling (stpd_scale_matrix_for_pca) — z-score and robust, hand-computed.

@Test
func scaleMatrixForPCAMatchesRZScoreAndRobust() {
    let m = [[1.0, 5.0], [2.0, 5.0], [3.0, 5.0]]   // col0 varies; col1 constant

    let z = ISIStateSpacePCA.scaleMatrixForPCA(m, scaling: .zscore)
    // col0: mean 2, sd 1 -> [-1, 0, 1]; col1: sd 0 -> scale 1, center 5 -> [0, 0, 0].
    #expect(abs(z[0][0] - (-1)) < 1e-12)
    #expect(abs(z[1][0]) < 1e-12)
    #expect(abs(z[2][0] - 1) < 1e-12)
    #expect(abs(z[0][1]) < 1e-12 && abs(z[1][1]) < 1e-12 && abs(z[2][1]) < 1e-12)

    let r = ISIStateSpacePCA.scaleMatrixForPCA(m, scaling: .robust)
    // col0: median 2, mad = 1.4826 * median(|−1|,0,|1|) = 1.4826 -> (x−2)/1.4826.
    #expect(abs(r[0][0] - (-1.0 / 1.4826)) < 1e-9)
    #expect(abs(r[1][0]) < 1e-12)
    #expect(abs(r[2][0] - (1.0 / 1.4826)) < 1e-9)
    // col1: MAD 0 -> sd fallback 0 -> final scale 1; center median 5 -> 0.
    #expect(abs(r[0][1]) < 1e-12 && abs(r[1][1]) < 1e-12 && abs(r[2][1]) < 1e-12)
}

// MARK: - 2. Non-finite cells imputed with the column median (R per-column impute).

@Test
func scaleMatrixForPCAImputesNonFiniteWithColumnMedian() {
    let nan = Double.nan, inf = Double.infinity
    let m = [[1.0, inf], [nan, 5.0], [3.0, 5.0]]
    // col0 finite [1,3] median 2 -> NaN row becomes 2 -> [1,2,3]; zscore -> [-1,0,1].
    // col1 finite [5,5] median 5 -> Inf becomes 5 -> [5,5,5]; constant -> [0,0,0].
    let z = ISIStateSpacePCA.scaleMatrixForPCA(m, scaling: .zscore)
    #expect(abs(z[0][0] - (-1)) < 1e-12)
    #expect(abs(z[1][0]) < 1e-12)            // imputed (median) row scales to 0
    #expect(abs(z[2][0] - 1) < 1e-12)
    #expect(abs(z[0][1]) < 1e-12 && abs(z[1][1]) < 1e-12 && abs(z[2][1]) < 1e-12)
    #expect(z.allSatisfy { $0.allSatisfy(\.isFinite) })   // no NaN/Inf leaks through
}

// MARK: - 3. Varying-column filter (R: >= 2 finite AND >= 2 distinct at signif 12).

@Test
func isVaryingColumnMatchesRFiniteAndDistinctRule() {
    #expect(ISIStateSpacePCA.isVaryingColumn([1, 2, 3]) == true)
    #expect(ISIStateSpacePCA.isVaryingColumn([5, 5, 5]) == false)               // constant -> 1 distinct
    #expect(ISIStateSpacePCA.isVaryingColumn([1, .nan]) == false)               // < 2 finite
    #expect(ISIStateSpacePCA.isVaryingColumn([.nan, .nan]) == false)
    #expect(ISIStateSpacePCA.isVaryingColumn([1, 1, .infinity]) == false)       // 2 finite but 1 distinct
}

// MARK: - 4. PCA core — perfectly correlated columns concentrate all variance in PC1.

@Test
func principalComponentsOnCorrelatedColumnsConcentratesVariance() {
    // Already-centered/scaled matrix with two identical columns: G = XᵀX = [[2,2],[2,2]] -> eigenvalues 4, 0.
    let x = [[-1.0, -1.0], [0.0, 0.0], [1.0, 1.0]]
    let core = ISIStateSpacePCA.principalComponents(scaledMatrix: x)
    #expect(core.eigenvalues.count == 2)
    #expect(core.varianceRatios.count == 2)
    #expect(abs(core.varianceRatios[0] - 1.0) < 1e-9)      // all variance in PC1
    #expect(abs(core.varianceRatios[1]) < 1e-9)
    // PC1 direction is [1,1]/√2, so scores = X·v = [-√2, 0, √2] (sign-invariant check).
    let pc1 = core.scores.map { $0[0] }
    #expect(abs(abs(pc1[0]) - 2.0.squareRoot()) < 1e-9)
    #expect(abs(pc1[1]) < 1e-9)
    #expect(abs(abs(pc1[2]) - 2.0.squareRoot()) < 1e-9)
}

// MARK: - 5. PCA core — eigenvalues descending, loadings orthonormal, variance ratios sum to 1.

@Test
func principalComponentsLoadingsAreOrthonormalAndEigenvaluesDescending() {
    let x = [
        [2.0, 0.0, 1.0],
        [-1.0, 1.0, 0.5],
        [0.0, -1.0, -2.0],
        [-1.0, 0.0, 0.5],
    ]
    let core = ISIStateSpacePCA.principalComponents(scaledMatrix: x)
    // Eigenvalues non-increasing.
    for i in 1..<core.eigenvalues.count {
        #expect(core.eigenvalues[i - 1] >= core.eigenvalues[i] - 1e-12)
    }
    // Variance ratios sum to 1.
    #expect(abs(core.varianceRatios.reduce(0, +) - 1.0) < 1e-9)
    // Loadings columns are unit-norm and mutually orthogonal.
    let ncomp = core.eigenvalues.count
    let p = x[0].count
    func dot(_ a: Int, _ b: Int) -> Double { (0..<p).reduce(0.0) { $0 + core.loadings[$1][a] * core.loadings[$1][b] } }
    for c in 0..<ncomp { #expect(abs(dot(c, c) - 1.0) < 1e-9) }
    if ncomp >= 2 { #expect(abs(dot(0, 1)) < 1e-9) }
    // Reconstruction: scores · loadingsᵀ recovers the scaled matrix (sign-invariant by construction).
    for i in 0..<x.count {
        for j in 0..<p {
            let recon = (0..<ncomp).reduce(0.0) { $0 + core.scores[i][$1] * core.loadings[j][$1] }
            #expect(abs(recon - x[i][j]) < 1e-7)
        }
    }
}

// MARK: - 6. run guards (R stop() messages) — too few rows / too few varying features.

@Test
func runThrowsForTooFewRows() {
    // [0, 0.0005, 0.010]: first ISI (0.0005) excluded below the 0.001 floor -> only 1 feature row.
    let rows = featureRows([0, 0.0005, 0.010])
    #expect(rows.count == 1)
    #expect(throws: ISIStatePCAError.tooFewRows) {
        _ = try ISIStateSpacePCA.run(rows: rows, scaling: .robust)
    }
}

@Test
func runThrowsForTooFewVaryingFeatures() {
    // Perfectly regular train: integer-second timestamps give ISIs of exactly 1.0 (no FP drift), so
    // every PCA feature column is constant and < 2 columns vary.
    let regular = (0...10).map { Double($0) }
    let rows = featureRows(regular)
    #expect(rows.count >= 2)
    #expect(throws: ISIStatePCAError.tooFewVaryingFeatures) {
        _ = try ISIStateSpacePCA.run(rows: rows, scaling: .robust)
    }
}

// MARK: - 7. run end-to-end — R test_state_space_pca.R Test 1 structural parity.

@Test
func runProducesRCompatibleScoresLoadingsVarianceForMixedTrain() throws {
    let rows = featureRows(mixedTimestamps(), k: 3)
    #expect(rows.count > 5)
    let result = try ISIStateSpacePCA.run(rows: rows, scaling: .robust)

    // scores: one row per feature row, PC1 present + finite.
    #expect(result.scores.count == rows.count)
    #expect(result.scores.allSatisfy { $0.pc1 != nil && $0.pc1!.isFinite })
    #expect(result.scores.allSatisfy { $0.pc2 != nil && $0.pc3 != nil })   // >= 3 components here

    // loadings: more than one feature loaded; includes the R-named columns; PC1 finite.
    #expect(result.loadings.count >= 2)
    #expect(result.loadings.contains { $0.feature == "log_isi_feature" })
    #expect(result.loadings.contains { $0.feature == "local_cv2" })
    #expect(result.loadings.allSatisfy { $0.pc1 != nil && $0.pc1!.isFinite })

    // variance: ratios in [0,1], descending, sum to 1, cumulative ends at ~1.
    let ratios = result.variance.compactMap(\.variance)
    #expect(ratios.count == result.variance.count)
    #expect(ratios.allSatisfy { $0 >= -1e-12 && $0 <= 1 + 1e-9 })
    for i in 1..<ratios.count { #expect(ratios[i - 1] >= ratios[i] - 1e-12) }
    #expect(abs(ratios.reduce(0, +) - 1.0) < 1e-9)
    #expect(abs((result.variance.last?.cumulative ?? 0) - 1.0) < 1e-9)

    // featureColumns == loadings features, in selection order.
    #expect(result.featureColumns == result.loadings.map(\.feature))
}

// MARK: - 8. Feature-column selection order (R selector: log_isi_feature, lags ascending, locals, deltas).

@Test
func featureColumnsUseRSelectionOrderAndNames() {
    let rows = featureRows(mixedTimestamps(), k: 2)
    let names = ISIStateSpacePCA.featureColumns(rows).map(\.name)
    #expect(names.first == "log_isi_feature")
    // lag columns present in ascending-offset order, lag_0 included (duplicates log_isi_feature in R).
    #expect(names.contains("lag_m2") && names.contains("lag_0") && names.contains("lag_p2"))
    #expect(names.firstIndex(of: "lag_m2")! < names.firstIndex(of: "lag_0")!)
    #expect(names.firstIndex(of: "lag_0")! < names.firstIndex(of: "lag_p2")!)
    // local / delta / prepost columns follow.
    #expect(names.contains("local_cv") && names.contains("local_cv2") && names.contains("local_lv"))
    #expect(names.contains("delta_logisi") && names.contains("next_delta_logisi") && names.contains("prepost_ratio"))
    #expect(names.firstIndex(of: "lag_p2")! < names.firstIndex(of: "local_cv2")!)
}
