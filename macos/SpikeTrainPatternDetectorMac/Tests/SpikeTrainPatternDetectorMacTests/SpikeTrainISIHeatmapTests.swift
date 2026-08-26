import Testing
@testable import SpikeTrainPatternDetectorMac
@testable import STPDCore

@Suite("Spike-train ISI heatmap")
struct SpikeTrainISIHeatmapTests {
    @Test("Short ISIs are warmer than long ISIs within the same train")
    func shortIntervalsProduceHigherLocalIntensity() {
        let train = SpikeTrain(
            name: "unit_a",
            timestampsSec: [0, 0.01, 0.02, 0.12]
        )

        let heatmap = SpikeTrainISIHeatmapBuilder.make(
            trains: [train],
            timeMode: .aligned,
            columnCount: 100
        )

        #expect(heatmap.rowCount == 1)
        #expect(heatmap.columnCount == 100)
        // The two 10 ms intervals occupy the left edge; the 100 ms interval occupies 20–120 ms.
        #expect(heatmap.normalizedValues[1] > heatmap.normalizedValues[8])
        #expect(heatmap.upperLogHz! > heatmap.lowerLogHz!)
    }

    @Test("Zero-length intervals never create non-finite heat values")
    func duplicateTimestampsAreIgnoredForISIIntensity() {
        let train = SpikeTrain(
            name: "unit_a",
            timestampsSec: [0, 0, 0.1, 0.2]
        )

        let heatmap = SpikeTrainISIHeatmapBuilder.make(
            trains: [train],
            timeMode: .aligned,
            columnCount: 128
        )

        #expect(heatmap.normalizedValues.allSatisfy { $0.isFinite && $0 >= 0 && $0 <= 1 })
        #expect(heatmap.lowerLogHz != nil)
        #expect(heatmap.upperLogHz != nil)
    }

    @Test("Raw-time heatmap preserves a shorter train's real start and end")
    func rawTimeDoesNotStretchShorterTrainCoverage() {
        let earlyTrain = SpikeTrain(name: "early", timestampsSec: [0, 0.01, 0.02, 0.2])
        let lateTrain = SpikeTrain(name: "late", timestampsSec: [10, 10.2, 10.3])

        let heatmap = SpikeTrainISIHeatmapBuilder.make(
            trains: [earlyTrain, lateTrain],
            timeMode: .raw,
            columnCount: 32
        )

        #expect(heatmap.lowerTimeSec == 0)
        #expect(heatmap.upperTimeSec >= 10.2)

        let earlyRow = Array(heatmap.supportedCells.prefix(heatmap.columnCount))
        let lateRow = Array(heatmap.supportedCells.suffix(heatmap.columnCount))
        #expect(earlyRow.prefix(1).contains(true))
        #expect(earlyRow.suffix(20).allSatisfy { !$0 })
        #expect(lateRow.prefix(20).allSatisfy { !$0 })
        #expect(lateRow.suffix(1).contains(true))
        #expect((heatmap.normalizedValues.max() ?? 0) > 0.5)
    }

    @Test("Raw time retains the aligned time-bin density instead of compressing ISIs")
    func rawTimeAdaptsColumnCountToAbsoluteSpan() {
        let earlyTrain = SpikeTrain(name: "early", timestampsSec: [0, 0.01, 0.02, 0.2])
        let lateTrain = SpikeTrain(name: "late", timestampsSec: [10, 10.2, 10.3])

        let aligned = SpikeTrainISIHeatmapBuilder.make(
            trains: [earlyTrain, lateTrain],
            timeMode: .aligned,
            columnCount: 256
        )
        let raw = SpikeTrainISIHeatmapBuilder.make(
            trains: [earlyTrain, lateTrain],
            timeMode: .raw,
            columnCount: 256
        )

        #expect(raw.columnCount > aligned.columnCount)
        let alignedSecondsPerColumn = (aligned.upperTimeSec - aligned.lowerTimeSec) / Double(aligned.columnCount)
        let rawSecondsPerColumn = (raw.upperTimeSec - raw.lowerTimeSec) / Double(raw.columnCount)
        #expect(abs(rawSecondsPerColumn - alignedSecondsPerColumn) < 0.000_1)
    }

    @Test("Publication scale is fixed to the full dataset rather than the selected trains")
    func publicationScaleIsIndependentOfVisibleSelection() {
        let slowTrain = SpikeTrain(name: "slow", timestampsSec: [0, 0.2, 0.5])
        let fastTrain = SpikeTrain(name: "fast", timestampsSec: [0, 0.01, 0.03])
        let fullDataset = [slowTrain, fastTrain]

        let slowOnly = SpikeTrainISIHeatmapBuilder.make(
            trains: [slowTrain],
            timeMode: .aligned,
            columnCount: 128,
            scaleTrains: fullDataset,
            scaleMode: .publicationDatasetFixed
        )
        let fastOnly = SpikeTrainISIHeatmapBuilder.make(
            trains: [fastTrain],
            timeMode: .aligned,
            columnCount: 128,
            scaleTrains: fullDataset,
            scaleMode: .publicationDatasetFixed
        )

        #expect(slowOnly.scaleMode == .publicationDatasetFixed)
        #expect(slowOnly.lowerLogHz == fastOnly.lowerLogHz)
        #expect(slowOnly.upperLogHz == fastOnly.upperLogHz)
        #expect(slowOnly.scaleObservationCount == 4)
        #expect(slowOnly.lowClipCount == 0)
        #expect(slowOnly.highClipCount == 0)
    }
}
