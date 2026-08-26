import STPDCore

/// The async state of the Neural Manifold **Isomap** background computation. Each non-idle case carries the
/// `NeuralManifoldIsomapInputs` it corresponds to, so the view can tell whether the cached result matches the
/// *current* inputs (and otherwise show the loading / error state). PCA never uses this — it stays synchronous.
enum NeuralManifoldIsomapState {
    case idle
    case computing(NeuralManifoldIsomapInputs)
    case ready(NeuralManifoldIsomapInputs, NeuralPopulationIsomapResult)
    case failed(NeuralManifoldIsomapInputs, NeuralManifoldIsomapFailure)

    /// The result iff the cache currently matches `inputs` (else `nil`: idle / computing / stale / failed).
    func result(for inputs: NeuralManifoldIsomapInputs) -> NeuralPopulationIsomapResult? {
        if case let .ready(cached, result) = self, cached == inputs { return result }
        return nil
    }

    /// The failure iff the cache currently matches `inputs` (else `nil`).
    func failure(for inputs: NeuralManifoldIsomapInputs) -> NeuralManifoldIsomapFailure? {
        if case let .failed(cached, failure) = self, cached == inputs { return failure }
        return nil
    }
}
/// Why an Isomap embedding could not be computed — carried raw (not localized) so the view formats the message.
enum NeuralManifoldIsomapFailure: Error {
    case matrix(NeuralPopulationMatrixError)
    case isomap(NeuralPopulationIsomapError)
}
