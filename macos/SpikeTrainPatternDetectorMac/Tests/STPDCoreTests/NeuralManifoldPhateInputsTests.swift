import XCTest
@testable import STPDCore

/// Locks the cache-key contract for the background Neural Manifold PHATE (parallel to
/// `NeuralManifoldIsomapInputsTests`): the signature is equal when (and only when) every embedding-relevant
/// input matches, so display-only UI changes (not fields of this type) can never invalidate a cached embedding,
/// while any real embedding input change recomputes it.
final class NeuralManifoldPhateInputsTests: XCTestCase {

    private func makeInputs(
        datasetID: String = "dataset-A",
        selectedTrainIDs: [String] = ["t1", "t2", "t3"],
        parameters: NeuralPopulationParameters = NeuralPopulationParameters(
            binSec: 0.05, startSec: 1, endSec: 9, timeOrigin: .raw,
            transform: .sqrtCount, smoothingSigmaBins: 1, scaling: .zscore
        ),
        neighborCount: Int = 15,
        diffusionTime: Int = 3,
        maxPoints: Int = 1200
    ) -> NeuralManifoldPhateInputs {
        NeuralManifoldPhateInputs(
            datasetID: datasetID,
            selectedTrainIDs: selectedTrainIDs,
            parameters: parameters,
            neighborCount: neighborCount,
            diffusionTime: diffusionTime,
            maxPoints: maxPoints
        )
    }

    func testIdenticalFieldsAreEqual() {
        XCTAssertEqual(makeInputs(), makeInputs())
    }

    func testDatasetIDChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(datasetID: "dataset-B"))
    }

    func testSelectedTrainIDsMembershipChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(selectedTrainIDs: ["t1", "t2"]))
        XCTAssertNotEqual(makeInputs(), makeInputs(selectedTrainIDs: ["t1", "t2", "t4"]))
    }

    func testSelectedTrainIDsOrderChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(selectedTrainIDs: ["t2", "t1", "t3"]))
    }

    func testBinSizeChangeDiffers() {
        var p = makeInputs().parameters
        p.binSec = 0.1
        XCTAssertNotEqual(makeInputs(), makeInputs(parameters: p))
    }

    func testWindowStartEndChangeDiffers() {
        var start = makeInputs().parameters
        start.startSec = 2
        XCTAssertNotEqual(makeInputs(), makeInputs(parameters: start))

        var end = makeInputs().parameters
        end.endSec = 8
        XCTAssertNotEqual(makeInputs(), makeInputs(parameters: end))
    }

    func testTimeOriginChangeDiffers() {
        var p = makeInputs().parameters
        p.timeOrigin = .aligned
        XCTAssertNotEqual(makeInputs(), makeInputs(parameters: p))
    }

    func testTransformChangeDiffers() {
        var p = makeInputs().parameters
        p.transform = .log1pRate
        XCTAssertNotEqual(makeInputs(), makeInputs(parameters: p))
    }

    func testSmoothingSigmaChangeDiffers() {
        var p = makeInputs().parameters
        p.smoothingSigmaBins = 2
        XCTAssertNotEqual(makeInputs(), makeInputs(parameters: p))
    }

    func testScalingChangeDiffers() {
        var p = makeInputs().parameters
        p.scaling = .robust
        XCTAssertNotEqual(makeInputs(), makeInputs(parameters: p))
    }

    func testNeighborCountChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(neighborCount: 16))
    }

    func testDiffusionTimeChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(diffusionTime: 5))
    }

    func testMaxPointsChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(maxPoints: 600))
    }
}
