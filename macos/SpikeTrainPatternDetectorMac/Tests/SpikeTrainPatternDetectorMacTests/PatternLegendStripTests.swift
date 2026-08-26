import Testing
@testable import STPDCore
@testable import SpikeTrainPatternDetectorMac

@Suite("Raster pattern legend")
struct PatternLegendStripTests {
    @Test("A visible manual mark creates its legend without detector output")
    func manualMarkCreatesLegendEntry() {
        let train = SpikeTrain(name: "unit_a", timestampsSec: [0, 0.1, 0.2])
        let entries = PatternLegendEntry.visibleEntries(
            annotations: [],
            manualAnnotations: [
                .init(trainID: train.id, label: .tonic, startSec: 0.05, endSec: 0.15)
            ],
            trains: [train],
            trainIDs: [train.id],
            timeRange: .init(lowerBound: 0, upperBound: 0.2),
            timeMode: .raw
        )

        #expect(entries.map(\.key) == ["tonic"])
        #expect(entries.first?.name == "Tonic")
    }

    @Test("Manual state and embedded event receive independent legend entries")
    func manualStateAndEventBothAppear() {
        let train = SpikeTrain(name: "unit_a", timestampsSec: [0, 0.1, 0.2])
        let entries = PatternLegendEntry.visibleEntries(
            annotations: [],
            manualAnnotations: [
                .init(trainID: train.id, label: .highFrequencySpiking, startSec: 0.04, endSec: 0.18),
                .init(trainID: train.id, label: .highFrequencyBurst, startSec: 0.08, endSec: 0.14)
            ],
            trains: [train],
            trainIDs: [train.id],
            timeRange: .init(lowerBound: 0, upperBound: 0.2),
            timeMode: .raw
        )

        #expect(entries.map(\.key) == ["high_frequency_burst", "high_frequency_spiking"])
    }

    @Test("A negative veto is review evidence, not a displayed biological mode")
    func vetoDoesNotCreateLegendEntry() {
        let train = SpikeTrain(name: "unit_a", timestampsSec: [0, 0.1, 0.2])
        let entries = PatternLegendEntry.visibleEntries(
            annotations: [],
            manualAnnotations: [
                .init(trainID: train.id, label: .notBurst, startSec: 0.05, endSec: 0.15)
            ],
            trains: [train],
            trainIDs: [train.id],
            timeRange: .init(lowerBound: 0, upperBound: 0.2),
            timeMode: .raw
        )

        #expect(entries.isEmpty)
    }
}
