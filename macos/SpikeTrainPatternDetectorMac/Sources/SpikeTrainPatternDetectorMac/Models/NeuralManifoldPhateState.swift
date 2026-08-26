import STPDCore

/// The async state of the Neural Manifold **PHATE** background computation (parallel to
/// `NeuralManifoldIsomapState`). Each non-idle case carries the `NeuralManifoldPhateInputs` it corresponds to, so
/// the view can tell whether the cached result matches the *current* inputs (and otherwise show the loading /
/// error state). PCA never uses this — it stays synchronous.
enum NeuralManifoldPhateState {
    case idle
    case computing(NeuralManifoldPhateInputs)
    case ready(NeuralManifoldPhateInputs, NeuralPopulationPhateResult)
    case failed(NeuralManifoldPhateInputs, NeuralManifoldPhateFailure)

    /// The result iff the cache currently matches `inputs` (else `nil`: idle / computing / stale / failed).
    func result(for inputs: NeuralManifoldPhateInputs) -> NeuralPopulationPhateResult? {
        if case let .ready(cached, result) = self, cached == inputs { return result }
        return nil
    }

    /// The failure iff the cache currently matches `inputs` (else `nil`).
    func failure(for inputs: NeuralManifoldPhateInputs) -> NeuralManifoldPhateFailure? {
        if case let .failed(cached, failure) = self, cached == inputs { return failure }
        return nil
    }
}
/// Why a PHATE embedding could not be computed — carried raw (not localized) so the view formats the message.
enum NeuralManifoldPhateFailure: Error {
    case matrix(NeuralPopulationMatrixError)
    case phate(NeuralPopulationPhateError)
}
