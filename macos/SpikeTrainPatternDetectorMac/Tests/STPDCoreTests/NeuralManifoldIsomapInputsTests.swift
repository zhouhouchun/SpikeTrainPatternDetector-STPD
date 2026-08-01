import XCTest
@testable import STPDCore

/// Locks the cache-key contract for the background Neural Manifold Isomap: the signature is equal when (and only
/// when) every embedding-relevant input matches, so display-only UI changes (which are *not* fields of this type)
/// can never invalidate a cached embedding, while any real embedding input change recomputes it.
final class NeuralManifoldIsomapInputsTests: XCTestCase {

    private func makeInputs(
        datasetID: String = "dataset-A",
        selectedTrainIDs: [String] = ["t1", "t2", "t3"],
        parameters: NeuralPopulationParameters = NeuralPopulationParameters(
            binSec: 0.05,
            startSec: 1,
            endSec: 9,
            timeOrigin: .raw,
            transform: .sqrtCount,
            smoothingSigmaBins: 1,
            scaling: .zscore
        ),
        neighborCount: Int = 15,
        componentPolicy: ISIStateSpaceIsomapComponentPolicy = .largest,
        maxPoints: Int = 1200
    ) -> NeuralManifoldIsomapInputs {
        NeuralManifoldIsomapInputs(
            datasetID: datasetID,
            selectedTrainIDs: selectedTrainIDs,
            parameters: parameters,
            neighborCount: neighborCount,
            componentPolicy: componentPolicy,
            maxPoints: maxPoints
        )
    }

    // MARK: - Equality (cache HIT)

    func testIdenticalFieldsAreEqual() {
        XCTAssertEqual(makeInputs(), makeInputs())
    }

    // MARK: - Each embedding input differs (cache MISS → recompute)

    func testDatasetIDChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(datasetID: "dataset-B"))
    }

    func testSelectedTrainIDsMembershipChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(selectedTrainIDs: ["t1", "t2"]))
        XCTAssertNotEqual(makeInputs(), makeInputs(selectedTrainIDs: ["t1", "t2", "t4"]))
    }

    func testSelectedTrainIDsOrderChangeDiffers() {
        // Order matters: it determines the population-matrix column order.
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

    func testComponentPolicyChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(componentPolicy: .error))
    }

    func testMaxPointsChangeDiffers() {
        XCTAssertNotEqual(makeInputs(), makeInputs(maxPoints: 600))
    }
}
