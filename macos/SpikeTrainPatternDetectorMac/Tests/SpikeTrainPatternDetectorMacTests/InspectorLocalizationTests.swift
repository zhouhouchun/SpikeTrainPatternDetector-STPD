@testable import SpikeTrainPatternDetectorMac
import STPDCore
import Testing

@Suite("Inspector localization")
@MainActor
struct InspectorLocalizationTests {
    @Test("Russian idle inspector and core review surfaces do not fall back to English")
    func russianCoreSurfacesAreLocalized() {
        let sources = [
            "Select a structural candidate to inspect timestamps, detection metrics, and manual review state. Click an ISI on the raster to pin its diagnostic.",
            "Manual annotations",
            "None yet. Enable Annotate mode in the raster header and drag on a train, or create one from a focused candidate.",
            "Event Inspector",
            "Manual review",
            "Event metrics",
            "Pinned ISI",
            "Clear",
        ]

        for source in sources {
            let localized = InspectorView.localizedInspectorText(source, language: .ru)
            #expect(!localized.isEmpty)
            #expect(localized != source, "Russian inspector fallback for: \(source)")
        }
    }

    @Test("English inspector copy remains the canonical source copy")
    func englishRemainsSourceCopy() {
        let source = "Manual annotations"
        #expect(InspectorView.localizedInspectorText(source, language: .en) == source)
    }

    @Test("Unknown detector tokens remain unchanged")
    func unknownDetectorTokensPassThrough() {
        let token = "detector_token_v1"
        #expect(InspectorView.localizedInspectorText(token, language: .zh) == token)
        #expect(InspectorView.localizedInspectorText(token, language: .ru) == token)
    }
}
