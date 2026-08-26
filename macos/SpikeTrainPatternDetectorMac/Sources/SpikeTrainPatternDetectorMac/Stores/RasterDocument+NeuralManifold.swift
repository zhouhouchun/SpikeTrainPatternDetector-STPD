import Foundation
import STPDCore

extension RasterDocument {
    var neuralManifoldParameters: NeuralPopulationParameters {
        NeuralPopulationParameters(
            binSec: neuralManifoldBinMs / 1_000,
            startSec: nil,
            endSec: nil,
            timeOrigin: neuralManifoldTimeOrigin,
            transform: neuralManifoldTransform,
            smoothingSigmaBins: neuralManifoldSmoothingSigmaBins,
            scaling: neuralManifoldScaling
        )
    }

    var neuralManifoldIsomapInputs: NeuralManifoldIsomapInputs? {
        guard neuralManifoldMethod == .isomap, let dataset else { return nil }
        let ids = dataset.trains.map(\.id).filter(neuralManifoldSelectedTrainIDs.contains)
        guard ids.count >= 2 else { return nil }
        return NeuralManifoldIsomapInputs(
            datasetID: dataset.id.uuidString,
            selectedTrainIDs: ids,
            parameters: neuralManifoldParameters,
            neighborCount: neuralManifoldIsomapNeighbors,
            componentPolicy: neuralManifoldIsomapComponentMode == .error ? .error : .largest,
            maxPoints: neuralManifoldMaxEmbeddedBins
        )
    }

    var neuralManifoldPhateInputs: NeuralManifoldPhateInputs? {
        guard neuralManifoldMethod == .phate, let dataset else { return nil }
        let ids = dataset.trains.map(\.id).filter(neuralManifoldSelectedTrainIDs.contains)
        guard ids.count >= 2 else { return nil }
        return NeuralManifoldPhateInputs(
            datasetID: dataset.id.uuidString,
            selectedTrainIDs: ids,
            parameters: neuralManifoldParameters,
            neighborCount: neuralManifoldIsomapNeighbors,
            diffusionTime: neuralManifoldDiffusionTime,
            maxPoints: neuralManifoldMaxEmbeddedBins
        )
    }

    func requestNeuralManifoldIsomap(_ inputs: NeuralManifoldIsomapInputs) {
        switch neuralManifoldIsomapState {
        case let .ready(cached, _) where cached == inputs: return
        case let .computing(cached) where cached == inputs: return
        case let .failed(cached, _) where cached == inputs: return
        default: break
        }
        guard let dataset, dataset.id.uuidString == inputs.datasetID else { return }
        neuralManifoldIsomapGeneration &+= 1
        let generation = neuralManifoldIsomapGeneration
        neuralManifoldIsomapState = .computing(inputs)

        Task.detached(priority: .userInitiated) {
            let outcome: Result<NeuralPopulationIsomapResult, NeuralManifoldIsomapFailure>
            do {
                let matrix = try NeuralPopulationMatrixBuilder.build(
                    dataset: dataset,
                    selectedTrainIDs: inputs.selectedTrainIDs,
                    parameters: inputs.parameters
                )
                outcome = .success(try NeuralPopulationIsomap.run(
                    matrix: matrix,
                    neighborCount: inputs.neighborCount,
                    maxPoints: inputs.maxPoints,
                    componentPolicy: inputs.componentPolicy
                ))
            } catch let error as NeuralPopulationMatrixError {
                outcome = .failure(.matrix(error))
            } catch let error as NeuralPopulationIsomapError {
                outcome = .failure(.isomap(error))
            } catch {
                outcome = .failure(.isomap(.tooFewBins))
            }
            await MainActor.run {
                guard generation == self.neuralManifoldIsomapGeneration else { return }
                switch outcome {
                case let .success(result): self.neuralManifoldIsomapState = .ready(inputs, result)
                case let .failure(failure): self.neuralManifoldIsomapState = .failed(inputs, failure)
                }
            }
        }
    }

    func requestNeuralManifoldPhate(_ inputs: NeuralManifoldPhateInputs) {
        switch neuralManifoldPhateState {
        case let .ready(cached, _) where cached == inputs: return
        case let .computing(cached) where cached == inputs: return
        case let .failed(cached, _) where cached == inputs: return
        default: break
        }
        guard let dataset, dataset.id.uuidString == inputs.datasetID else { return }
        neuralManifoldPhateGeneration &+= 1
        let generation = neuralManifoldPhateGeneration
        neuralManifoldPhateState = .computing(inputs)

        Task.detached(priority: .userInitiated) {
            let outcome: Result<NeuralPopulationPhateResult, NeuralManifoldPhateFailure>
            do {
                let matrix = try NeuralPopulationMatrixBuilder.build(
                    dataset: dataset,
                    selectedTrainIDs: inputs.selectedTrainIDs,
                    parameters: inputs.parameters
                )
                outcome = .success(try NeuralPopulationPhate.run(
                    matrix: matrix,
                    neighborCount: inputs.neighborCount,
                    diffusionTime: inputs.diffusionTime,
                    maxPoints: inputs.maxPoints
                ))
            } catch let error as NeuralPopulationMatrixError {
                outcome = .failure(.matrix(error))
            } catch let error as NeuralPopulationPhateError {
                outcome = .failure(.phate(error))
            } catch {
                outcome = .failure(.phate(.tooFewBins))
            }
            await MainActor.run {
                guard generation == self.neuralManifoldPhateGeneration else { return }
                switch outcome {
                case let .success(result): self.neuralManifoldPhateState = .ready(inputs, result)
                case let .failure(failure): self.neuralManifoldPhateState = .failed(inputs, failure)
                }
            }
        }
    }

    func resetNeuralManifoldEmbeddingCaches() {
        neuralManifoldIsomapGeneration &+= 1
        neuralManifoldIsomapState = .idle
        neuralManifoldPhateGeneration &+= 1
        neuralManifoldPhateState = .idle
    }
}
