import Foundation

/// The complete set of inputs that determine a Neural Manifold **PHATE** embedding — and nothing else. It is the
/// cache key / signature for the background PHATE computation (parallel to `NeuralManifoldIsomapInputs`): it
/// changes only when an embedding-relevant input changes (dataset / train selection, population-matrix
/// parameters, diffusion neighbor count, diffusion time, max embedded bins), and it deliberately omits all
/// display-only state (color mode, X/Y/Z axes, 2-D vs 3-D, line/axes/legend toggles, hover/selection, language),
/// so those never invalidate a cached embedding.
///
/// Pure value type built from STPDCore types only, so the equality semantics are unit-testable.
public struct NeuralManifoldPhateInputs: Equatable, Sendable {
    public let datasetID: String
    /// Selected train IDs in dataset (column) order — the trains that enter the population matrix.
    public let selectedTrainIDs: [String]
    /// Population-matrix parameters (bin size, time origin, transform, scaling, smoothing) = R `pop` inputs.
    public let parameters: NeuralPopulationParameters
    /// Local-kernel neighbor count (R `n_neighbors`, shared with Isomap/UMAP in the R UI).
    public let neighborCount: Int
    /// Diffusion time `t` (R `neural_manifold_diffusion_time`).
    public let diffusionTime: Int
    public let maxPoints: Int

    public init(
        datasetID: String,
        selectedTrainIDs: [String],
        parameters: NeuralPopulationParameters,
        neighborCount: Int,
        diffusionTime: Int,
        maxPoints: Int
    ) {
        self.datasetID = datasetID
        self.selectedTrainIDs = selectedTrainIDs
        self.parameters = parameters
        self.neighborCount = neighborCount
        self.diffusionTime = diffusionTime
        self.maxPoints = maxPoints
    }
}
