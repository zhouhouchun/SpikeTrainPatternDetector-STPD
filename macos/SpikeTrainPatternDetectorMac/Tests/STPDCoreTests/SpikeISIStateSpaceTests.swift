import Foundation
import STPDCore
import Testing

@Test
func buildsISIStateSpacePointsWithQCPrecedence() throws {
    let train = SpikeTrain(
        name: "state_a",
        timestampsSec: [0.0, 0.0, 0.0005, 0.0016, 0.0040],
        duplicateTimestampPolicy: .errorKeep
    )
    let settings = SpikeQualitySettings(
        artifactThresholdSec: 0.0009,
        refractorySuspectThresholdSec: 0.0015
    )

    let trace = SpikeISIStateTrace.build(for: train, settings: settings)

    #expect(trace.trainName == "state_a")
    #expect(trace.points.count == 3)
    #expect(trace.points.map(\.qcStatus) == [.duplicate, .artifact, .refractory])

    let duplicate = try #require(trace.points.first)
    #expect(duplicate.leftISISec == 0)
    #expect(abs(duplicate.rightISISec - 0.0005) < 1e-12)
    #expect(duplicate.firstSpikeTimeSec == 0)
    #expect(duplicate.middleSpikeTimeSec == 0)
    #expect(abs(duplicate.lastSpikeTimeSec - 0.0005) < 1e-12)

    let artifact = trace.points[1]
    #expect(artifact.qcStatus == .artifact)
    #expect(abs(artifact.leftISISec - 0.0005) < 1e-12)
    #expect(abs(artifact.rightISISec - 0.0011) < 1e-12)
}

@Test
func buildsISIStateSpaceFeatureTableWithLocalScalingAndBreakFlags() throws {
    let train = SpikeTrain(
        name: "feature_a",
        timestampsSec: [0.0, 0.0, 0.0005, 0.0016, 0.0040],
        duplicateTimestampPolicy: .errorKeep
    )
    let settings = SpikeQualitySettings(
        artifactThresholdSec: 0.0009,
        refractorySuspectThresholdSec: 0.0015
    )
    let stateTrace = SpikeISIStateTrace.build(for: train, settings: settings)
    let featureTrace = SpikeISIStateFeatureTable.build(
        for: stateTrace,
        options: SpikeISIStateFeatureOptions(
            halfWindowK: 1,
            scaling: .robust,
            winsorizeExtremeLogISI: false,
            breakLongISI: true,
            breakThresholdSec: 0.002
        )
    )

    #expect(featureTrace.features.count == 3)
    let duplicate = try #require(featureTrace.features.first)
    #expect(duplicate.qcStatus == .duplicate)
    #expect(duplicate.leftLogWasFloored)
    #expect(!duplicate.rightLogWasFloored)
    #expect(duplicate.scaledLeft.isFinite)
    #expect(duplicate.scaledRight.isFinite)
    #expect(duplicate.localMADLog10ISI >= 0)

    let longISI = featureTrace.features[2]
    #expect(longISI.isLongISIBreak)
}

@Test
func zScoreStateSpaceScalingCentersFeatureCoordinates() throws {
    let train = SpikeTrain(
        name: "z",
        timestampsSec: [0.0, 0.001, 0.003, 0.006, 0.010, 0.016]
    )
    let stateTrace = SpikeISIStateTrace.build(for: train)
    let featureTrace = SpikeISIStateFeatureTable.build(
        for: stateTrace,
        options: SpikeISIStateFeatureOptions(
            halfWindowK: 2,
            scaling: .zScore,
            winsorizeExtremeLogISI: false,
            breakLongISI: false,
            breakThresholdSec: 0.150
        )
    )

    let scaledValues = featureTrace.features.flatMap { [$0.scaledLeft, $0.scaledRight] }
    let mean = scaledValues.reduce(0, +) / Double(scaledValues.count)
    let variance = scaledValues.reduce(0) { total, value in
        let diff = value - mean
        return total + diff * diff
    } / Double(scaledValues.count - 1)

    #expect(abs(mean) < 1e-12)
    #expect(abs(sqrt(variance) - 1) < 1e-12)
}

@Test
func sampleISIStateSpacePointCountsMatchDataset() throws {
    let sampleURL = repositoryRoot()
        .appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    try assertStateSpaceCounts(
        csvURL: sampleURL,
        datasetName: "sample",
        expectDuplicatePoints: false
    )
}

@Test
func pdSTNISIStateSpaceRegression() throws {
    let pdURL = URL(fileURLWithPath: "/Users/zark/Desktop/Code/PD/STN/PD_STN.csv")
    guard FileManager.default.fileExists(atPath: pdURL.path) else {
        return
    }

    try assertStateSpaceCounts(
        csvURL: pdURL,
        datasetName: "PD_STN",
        expectDuplicatePoints: true
    )
}

private func assertStateSpaceCounts(
    csvURL: URL,
    datasetName: String,
    expectDuplicatePoints: Bool
) throws {
    let csv = try String(contentsOf: csvURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: datasetName,
        sourceDescription: csvURL.path,
        unit: .seconds,
        duplicatePolicy: .errorKeep
    )
    let settings = SpikeQualitySettings(
        artifactThresholdSec: 0.0009,
        refractorySuspectThresholdSec: 0.0010
    )

    let traces = SpikeISIStateTrace.build(for: dataset, settings: settings)
    let featureTraces = SpikeISIStateFeatureTable.build(
        for: traces,
        options: SpikeISIStateFeatureOptions(
            halfWindowK: 3,
            scaling: .robust,
            winsorizeExtremeLogISI: true,
            breakLongISI: true,
            breakThresholdSec: 0.150
        )
    )
    let pointCount = traces.reduce(0) { $0 + $1.points.count }
    let featureCount = featureTraces.reduce(0) { $0 + $1.features.count }
    let expectedPointCount = dataset.trains.reduce(0) { total, train in
        total + max(0, train.spikeCount - 2)
    }
    let duplicatePointCount = traces.reduce(0) { total, trace in
        total + trace.points.filter { $0.qcStatus == .duplicate }.count
    }
    let nonFiniteFeatureCount = featureTraces.reduce(0) { total, trace in
        total + trace.features.filter {
            !$0.log10LeftISISec.isFinite ||
                !$0.log10RightISISec.isFinite ||
                !$0.scaledLeft.isFinite ||
                !$0.scaledRight.isFinite
        }.count
    }

    #expect(traces.count == dataset.trains.count)
    #expect(pointCount == expectedPointCount)
    #expect(featureCount == expectedPointCount)
    #expect(pointCount > 0)
    #expect(nonFiniteFeatureCount == 0)

    if expectDuplicatePoints {
        #expect(duplicatePointCount > 0)
    } else {
        #expect(duplicatePointCount == 0)
    }
}

private func repositoryRoot() -> URL {
    var url = URL(fileURLWithPath: #filePath)
    for _ in 0..<5 {
        url.deleteLastPathComponent()
    }
    return url
}
