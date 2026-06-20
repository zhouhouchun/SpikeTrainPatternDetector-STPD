import STPDCore
import Testing
import Foundation

@Test
func buildsISITraceWithSpikeEndpointsAndQCFlags() throws {
    let train = SpikeTrain(
        name: "train_a",
        timestampsSec: [10.0, 10.0005, 10.0018, 10.0040]
    )

    let trace = SpikeISITrace.build(
        for: train,
        settings: SpikeQualitySettings(
            artifactThresholdSec: 0.0009,
            refractorySuspectThresholdSec: 0.0015
        )
    )

    #expect(trace.trainName == "train_a")
    #expect(trace.events.count == 3)

    let artifact = try #require(trace.events.first)
    #expect(artifact.leftSpikeIndex == 1)
    #expect(artifact.rightSpikeIndex == 2)
    #expect(abs(artifact.leftSpikeTimeSec - 10.0) < 1e-12)
    #expect(abs(artifact.rightSpikeTimeSec - 10.0005) < 1e-12)
    #expect(abs(artifact.alignedLeftSpikeTimeSec - 0.0) < 1e-12)
    #expect(abs(artifact.alignedRightSpikeTimeSec - 0.0005) < 1e-12)
    #expect(abs(artifact.isiSec - 0.0005) < 1e-12)
    #expect(artifact.isArtifact)
    #expect(!artifact.isRefractorySuspect)

    let refractory = trace.events[1]
    #expect(abs(refractory.isiSec - 0.0013) < 1e-12)
    #expect(!refractory.isArtifact)
    #expect(refractory.isRefractorySuspect)

    let valid = trace.events[2]
    #expect(abs(valid.isiSec - 0.0022) < 1e-12)
    #expect(!valid.isArtifact)
    #expect(!valid.isRefractorySuspect)
}

@Test
func buildsDatasetISITracesForAllTrains() throws {
    let dataset = SpikeDataset(
        name: "isi",
        sourceDescription: "inline",
        trains: [
            SpikeTrain(name: "a", timestampsSec: [0.0, 0.001, 0.003]),
            SpikeTrain(name: "b", timestampsSec: [1.0, 1.010])
        ]
    )

    let traces = SpikeISITrace.build(for: dataset)

    #expect(traces.map(\.trainName) == ["a", "b"])
    #expect(traces.map { $0.events.count } == [2, 1])
}

@Test
func sampleISITraceCountsMatchQCReport() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    try assertISITraceCountsMatchQCReport(csvURL: sampleURL, datasetName: "sample")
}

@Test
func optionalComplexISITraceCountsMatchQCReport() throws {
    guard let path = ProcessInfo.processInfo.environment["STPD_COMPLEX_CSV"], !path.isEmpty else {
        return
    }

    try assertISITraceCountsMatchQCReport(
        csvURL: URL(fileURLWithPath: path),
        datasetName: "complex"
    )
}

private func assertISITraceCountsMatchQCReport(csvURL: URL, datasetName: String) throws {
    let csv = try String(contentsOf: csvURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: datasetName,
        sourceDescription: csvURL.path,
        unit: .seconds
    )
    let settings = SpikeQualitySettings(
        artifactThresholdSec: 0.0009,
        refractorySuspectThresholdSec: 0.0010
    )

    let traces = SpikeISITrace.build(for: dataset, settings: settings)
    let report = SpikeQualityAnalyzer.analyze(dataset: dataset, settings: settings)

    let expectedEventCount = dataset.trains.reduce(0) { total, train in
        total + max(0, train.spikeCount - 1)
    }
    let eventCount = traces.reduce(0) { $0 + $1.events.count }
    let artifactEventCount = traces.reduce(0) { $0 + $1.events.filter(\.isArtifact).count }
    let refractoryEventCount = traces.reduce(0) { $0 + $1.events.filter(\.isRefractorySuspect).count }

    #expect(eventCount == expectedEventCount)
    #expect(artifactEventCount == report.artifactISICount)
    #expect(refractoryEventCount == report.refractorySuspectISICount)
}

private func repositoryRoot() -> URL {
    var url = URL(fileURLWithPath: #filePath)
    for _ in 0..<5 {
        url.deleteLastPathComponent()
    }
    return url
}
