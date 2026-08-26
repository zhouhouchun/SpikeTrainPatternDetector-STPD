import Testing
@testable import STPDCore
@testable import SpikeTrainPatternDetectorMac

@Suite("Spike-train pattern heatmap")
struct SpikeTrainModeHeatmapTests {
    @Test("State and embedded event remain on independent categorical tracks")
    func stateAndEventCoexistAtSameTime() {
        let train = SpikeTrain(name: "unit_a", timestampsSec: [0, 0.1, 0.2, 0.3])
        let heatmap = SpikeTrainModeHeatmapBuilder.make(
            trains: [train],
            automaticSegments: [
                .init(trainID: train.id, label: .highFrequencySpiking, rawStartSec: 0.05, rawEndSec: 0.25),
                .init(trainID: train.id, label: .highFrequencyBurst, rawStartSec: 0.10, rawEndSec: 0.20)
            ],
            manualSegments: [],
            source: .automatic,
            timeMode: .raw,
            columnCount: 64
        )

        #expect(heatmap.stateCells.contains(SpikeTrainModeHeatmap.code(for: .highFrequencySpiking)))
        #expect(heatmap.eventCells.contains(SpikeTrainModeHeatmap.code(for: .highFrequencyBurst)))
        #expect(zip(heatmap.stateCells, heatmap.eventCells).contains { state, event in
            state == SpikeTrainModeHeatmap.code(for: .highFrequencySpiking)
                && event == SpikeTrainModeHeatmap.code(for: .highFrequencyBurst)
        })
    }

    @Test("Manual mark overrides only its own categorical track in combined view")
    func manualPrecedenceDoesNotEraseOtherTrack() {
        let train = SpikeTrain(name: "unit_a", timestampsSec: [0, 0.1, 0.2, 0.3])
        let heatmap = SpikeTrainModeHeatmapBuilder.make(
            trains: [train],
            automaticSegments: [
                .init(trainID: train.id, label: .tonic, rawStartSec: 0.05, rawEndSec: 0.25),
                .init(trainID: train.id, label: .burst, rawStartSec: 0.10, rawEndSec: 0.20)
            ],
            manualSegments: [
                .init(trainID: train.id, label: .highFrequencyTonic, rawStartSec: 0.10, rawEndSec: 0.20)
            ],
            source: .combined,
            timeMode: .raw,
            columnCount: 64
        )

        #expect(heatmap.stateCells.contains(SpikeTrainModeHeatmap.code(for: .highFrequencyTonic)))
        #expect(heatmap.eventCells.contains(SpikeTrainModeHeatmap.code(for: .burst)))
    }

    @Test("Raw mode retains disjoint recording positions rather than aligning them")
    func rawModePreservesRealTrainTimes() throws {
        let early = SpikeTrain(name: "early", timestampsSec: [0, 0.1, 0.2])
        let late = SpikeTrain(name: "late", timestampsSec: [10, 10.1, 10.2])
        let heatmap = SpikeTrainModeHeatmapBuilder.make(
            trains: [early, late],
            automaticSegments: [
                .init(trainID: early.id, label: .pause, rawStartSec: 0.05, rawEndSec: 0.15),
                .init(trainID: late.id, label: .pause, rawStartSec: 10.05, rawEndSec: 10.15)
            ],
            manualSegments: [],
            source: .automatic,
            timeMode: .raw,
            columnCount: 128
        )

        let earlyCells = Array(heatmap.eventCells.prefix(heatmap.columnCount))
        let lateCells = Array(heatmap.eventCells.suffix(heatmap.columnCount))
        let pause = SpikeTrainModeHeatmap.code(for: .pause)
        let earlyFirst = try #require(earlyCells.firstIndex(of: pause))
        let lateFirst = try #require(lateCells.firstIndex(of: pause))
        #expect(earlyFirst < heatmap.columnCount / 4)
        #expect(lateFirst > heatmap.columnCount * 3 / 4)
        #expect(earlyCells.suffix(20).allSatisfy { $0 == 0 })
        #expect(lateCells.prefix(20).allSatisfy { $0 == 0 })
    }
}
