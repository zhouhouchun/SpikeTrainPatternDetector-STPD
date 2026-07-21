import Foundation

/// NM-1B: a pure, R-compatible PCA embedding layer on top of the NM-1A population matrix.
///
/// Mirrors the PCA branch of R `stpd_run_neural_manifold_embedding` (`R/61_neural_manifold.R`): it runs
/// `prcomp(center = FALSE, scale. = FALSE)` on the already-scaled population activity matrix
/// (`pop$X` → `NeuralPopulationMatrix.scaled`) and exposes the first three components as the NM1–NM3
/// manifold coordinates, plus per-train loadings and PC1–PC3 variance diagnostics.
///
/// The eigendecomposition is delegated to the existing R-compatible PCA core
/// `ISIStateSpacePCA.principalComponents(scaledMatrix:)` — no new eigensolver is introduced.
///
/// Scope is the PCA embedding only (Isomap is the separate `NeuralPopulationIsomap` service). No
/// FA/GPFA/PHATE/UMAP/t-SNE/CEBRA, no sliceTCA, no NF geometry, no event labels / validation / behavior
/// decoding / state dynamics, and no UI.

// MARK: - Result types

/// One PCA score row, aligned 1:1 with `NeuralPopulationMatrix.bins` (same order). `nm1`/`nm2`/`nm3` are
/// the first three principal-component coordinates; components beyond the available count are `0`
/// (mirroring R `stpd_neural_take3`, which zero-fills absent NM score columns).
public struct NeuralPopulationPCAScore: Sendable, Hashable {
    public let binID: Int
    public let startSec: Double
    public let endSec: Double
    public let midSec: Double
    public let widthSec: Double
    public let nm1: Double
    public let nm2: Double
    public let nm3: Double

    public init(
        binID: Int, startSec: Double, endSec: Double, midSec: Double, widthSec: Double,
        nm1: Double, nm2: Double, nm3: Double
    ) {
        self.binID = binID
        self.startSec = startSec
        self.endSec = endSec
        self.midSec = midSec
        self.widthSec = widthSec
        self.nm1 = nm1
        self.nm2 = nm2
        self.nm3 = nm3
    }
}

/// One loadings row per train/neuron column (mirrors R `fit$rotation` PC1–PC3, padded with `NA`/`nil`),
/// carrying the originating train id + name in `NeuralPopulationMatrix` column order.
public struct NeuralPopulationPCALoading: Sendable, Hashable {
    public let trainID: String
    public let trainName: String
    public let pc1: Double?
    public let pc2: Double?
    public let pc3: Double?

    public init(trainID: String, trainName: String, pc1: Double?, pc2: Double?, pc3: Double?) {
        self.trainID = trainID
        self.trainName = trainName
        self.pc1 = pc1
        self.pc2 = pc2
        self.pc3 = pc3
    }
}

/// One PC1–PC3 variance row: `varianceExplained = eigenvalue / sum(eigenvalues)` and the running
/// `cumulative`. Both are `nil` when total variance is not finite or `<= 0` (mirroring R's `NA`).
public struct NeuralPopulationPCAVariance: Sendable, Hashable {
    public let component: String
    public let varianceExplained: Double?
    public let cumulative: Double?

    public init(component: String, varianceExplained: Double?, cumulative: Double?) {
        self.component = component
        self.varianceExplained = varianceExplained
        self.cumulative = cumulative
    }
}

/// PCA embedding result. `scores` are 1:1 with the input bins; `loadings` are 1:1 with the input train
/// columns; `variance` has up to three rows (PC1–PC3, capped at the available component count).
public struct NeuralPopulationPCAResult: Sendable, Hashable {
    public let scores: [NeuralPopulationPCAScore]
    public let loadings: [NeuralPopulationPCALoading]
    public let variance: [NeuralPopulationPCAVariance]
    /// Method id, R `"pca"`.
    public let method: String
    /// Method label, R `"PCA"`.
    public let methodLabel: String
    /// Diagnostic note, R `"SVD of the scaled population activity matrix."`
    public let note: String
    public let binCount: Int
    public let trainCount: Int

    public init(
        scores: [NeuralPopulationPCAScore],
        loadings: [NeuralPopulationPCALoading],
        variance: [NeuralPopulationPCAVariance],
        method: String,
        methodLabel: String,
        note: String,
        binCount: Int,
        trainCount: Int
    ) {
        self.scores = scores
        self.loadings = loadings
        self.variance = variance
        self.method = method
        self.methodLabel = methodLabel
        self.note = note
        self.binCount = binCount
        self.trainCount = trainCount
    }
}

/// Guard failures, mirroring the R PCA-branch early return ("Need at least three bins and two varying
/// neurons.") split into precise, typed cases.
public enum NeuralPopulationPCAError: Error, Equatable, Sendable {
    /// Fewer than three population bins (R `nrow(X) < 3`).
    case tooFewBins
    /// Fewer than two train/neuron columns (R `ncol(X) < 2`).
    case tooFewTrains
    /// Fewer than two varying train/neuron columns (conservative reading of R's "two varying neurons").
    case tooFewVaryingTrains
}

// MARK: - Embedding

public enum NeuralPopulationPCA {
    public static let method = "pca"
    public static let methodLabel = "PCA"
    public static let note = "SVD of the scaled population activity matrix."

    /// Run PCA on `matrix.scaled` (R `pop$X`) and return NM1–NM3 scores, per-train loadings, and PC1–PC3
    /// variance diagnostics. Does not rebuild or rescale the population matrix.
    public static func run(matrix: NeuralPopulationMatrix) throws -> NeuralPopulationPCAResult {
        let scaled = matrix.scaled           // R pop$X (already centered/scaled by NM-1A)
        let nBins = matrix.binCount
        let nTrains = matrix.trainCount

        // Guards (R: nrow(X) < 3 || ncol(X) < 2). The varying check enforces the "varying neurons" wording.
        guard nBins >= 3 else { throw NeuralPopulationPCAError.tooFewBins }
        guard nTrains >= 2 else { throw NeuralPopulationPCAError.tooFewTrains }
        let varyingCount = (0..<nTrains).reduce(into: 0) { count, j in
            let column = (0..<nBins).map { scaled[$0][j] }
            if ISIStateSpacePCA.isVaryingColumn(column) { count += 1 }
        }
        guard varyingCount >= 2 else { throw NeuralPopulationPCAError.tooFewVaryingTrains }

        // Reuse the existing R-compatible PCA core on the scaled matrix.
        let core = ISIStateSpacePCA.principalComponents(scaledMatrix: scaled)
        let ncomp = core.eigenvalues.count

        // Scores zero-fill absent components (R `stpd_neural_take3`); loadings nil-fill them (R NA-fills
        // the missing PC columns of `fit$rotation`).
        func scoreComponent(_ row: [Double], _ index: Int) -> Double {
            index < ncomp ? row[index] : 0
        }
        func loadingComponent(_ row: [Double], _ index: Int) -> Double? {
            index < ncomp ? row[index] : nil
        }

        // Scores: 1:1 with bins, in bin order.
        let scores = zip(matrix.bins, core.scores).map { bin, row in
            NeuralPopulationPCAScore(
                binID: bin.binID,
                startSec: bin.startSec,
                endSec: bin.endSec,
                midSec: bin.midSec,
                widthSec: bin.widthSec,
                nm1: scoreComponent(row, 0),
                nm2: scoreComponent(row, 1),
                nm3: scoreComponent(row, 2)
            )
        }

        // Loadings: 1:1 with train columns, in matrix column order.
        let loadings = (0..<nTrains).map { j in
            NeuralPopulationPCALoading(
                trainID: matrix.trainIDs[j],
                trainName: matrix.trainNames[j],
                pc1: loadingComponent(core.loadings[j], 0),
                pc2: loadingComponent(core.loadings[j], 1),
                pc3: loadingComponent(core.loadings[j], 2)
            )
        }

        // Variance: PC1–PC3 (capped at ncomp). Ratio = eigenvalue / sum(all eigenvalues), cumulative is the
        // running sum (matching prcomp's `sdev^2 / sum(sdev^2)`; the `n-1` cancels in the ratio).
        let total = core.eigenvalues.reduce(0, +)
        let totalValid = total.isFinite && total > 0
        var variance: [NeuralPopulationPCAVariance] = []
        var cumulative = 0.0
        for index in 0..<Swift.min(3, ncomp) {
            let ratio = totalValid ? core.eigenvalues[index] / total : Double.nan
            if totalValid { cumulative += ratio }
            variance.append(NeuralPopulationPCAVariance(
                component: "PC\(index + 1)",
                varianceExplained: totalValid ? ratio : nil,
                cumulative: totalValid ? cumulative : nil
            ))
        }

        return NeuralPopulationPCAResult(
            scores: scores,
            loadings: loadings,
            variance: variance,
            method: method,
            methodLabel: methodLabel,
            note: note,
            binCount: nBins,
            trainCount: nTrains
        )
    }
}
