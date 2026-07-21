import Foundation

/// The complete set of inputs that determine a Neural Manifold **Isomap** embedding — and nothing else. It is
/// the cache key / signature for the background Isomap computation: it changes only when an embedding-relevant
/// input changes (dataset / train selection, population-matrix parameters, Isomap neighbor count, disconnected
/// policy, max embedded bins), and it deliberately omits all display-only state (color mode, X/Y/Z axes, 2-D vs
/// 3-D, line/axes/legend toggles, hover/selection, language), so those never invalidate a cached embedding.
///
/// Pure value type built from STPDCore types only, so the equality semantics are unit-testable.
public struct NeuralManifoldIsomapInputs: Equatable, Sendable {
    public let datasetID: String
    /// Selected train IDs in dataset (column) order — the trains that enter the population matrix.
    public let selectedTrainIDs: [String]
    /// Population-matrix parameters (bin size, time origin, transform, scaling, smoothing) = R `pop` inputs.
    public let parameters: NeuralPopulationParameters
    public let neighborCount: Int
    public let componentPolicy: ISIStateSpaceIsomapComponentPolicy
    public let maxPoints: Int

    public init(
        datasetID: String,
        selectedTrainIDs: [String],
        parameters: NeuralPopulationParameters,
        neighborCount: Int,
        componentPolicy: ISIStateSpaceIsomapComponentPolicy,
        maxPoints: Int
    ) {
        self.datasetID = datasetID
        self.selectedTrainIDs = selectedTrainIDs
        self.parameters = parameters
        self.neighborCount = neighborCount
        self.componentPolicy = componentPolicy
        self.maxPoints = maxPoints
    }
}
