import STPDCore
import Testing

@Test
func parsesWideSpikeMatrixAndAlignsEachTrain() throws {
    let csv = """
    train_a,train_b
    10.0,20.0
    10.5,20.1
    ,20.4
    11.0,
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "synthetic",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.count == 2)
    #expect(dataset.totalSpikeCount == 6)
    #expect(dataset.trains[0].alignedTimestampsSec == [0.0, 0.5, 1.0])
    #expect(abs(dataset.trains[1].alignedTimestampsSec[0] - 0.0) < 1e-12)
    #expect(abs(dataset.trains[1].alignedTimestampsSec[1] - 0.1) < 1e-12)
    #expect(abs(dataset.trains[1].alignedTimestampsSec[2] - 0.4) < 1e-12)
    #expect(abs(dataset.rawDurationSec - 10.4) < 1e-12)
}

@Test
func convertsMillisecondsToSeconds() throws {
    let csv = """
    train_a
    1000
    1250
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "milliseconds",
        sourceDescription: "inline",
        unit: .milliseconds
    )

    #expect(dataset.trains.first?.timestampsSec == [1.0, 1.25])
    #expect(dataset.trains.first?.alignedTimestampsSec == [0.0, 0.25])
}

@Test
func rejectsLikelyDerivedAnalysisCSVByHeader() throws {
    let csv = """
    Spike_train,Start_time,End_time,threshold,score
    train_a,0,1,0.1,0.9
    """

    #expect(throws: CSVSpikeMatrixParserError.self) {
        _ = try CSVSpikeMatrixParser.parse(
            contents: csv,
            datasetName: "derived",
            sourceDescription: "inline",
            unit: .seconds
        )
    }
}
