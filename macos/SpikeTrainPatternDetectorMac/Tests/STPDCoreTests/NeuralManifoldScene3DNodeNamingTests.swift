import Testing
@testable import STPDCore

@Test
func neuralManifoldNodeNameRoundTripsBinID() {
    for binID in [0, 1, 7, 42, 1499, 100_000] {
        let name = NeuralManifoldScene3DNodeNaming.nodeName(forBinID: binID)
        #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: name) == binID)
    }
}

@Test
func neuralManifoldNodeNameRejectsNonPointNames() {
    #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: nil) == nil)
    #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: "") == nil)
    #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: "NeuralManifoldMainCamera") == nil)
    #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: "selectionMarker") == nil)
    #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: "stpd.nm.point:") == nil)      // prefix only
    #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: "stpd.nm.point:abc") == nil)    // non-numeric
}

@Test
func neuralManifoldNodeNameNegativeBinIDRoundTrips() {
    // Bin IDs are non-negative in practice, but the encoding must not corrupt an unexpected value.
    let name = NeuralManifoldScene3DNodeNaming.nodeName(forBinID: -3)
    #expect(NeuralManifoldScene3DNodeNaming.binID(fromNodeName: name) == -3)
}
