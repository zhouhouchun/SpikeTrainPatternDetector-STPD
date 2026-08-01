import STPDCore
import Testing

@Test
func qualitySettingsConvertsMillisecondsToSeconds() {
    let settings = SpikeQualitySettings(
        artifactThresholdMilliseconds: 0.9,
        refractorySuspectThresholdMilliseconds: 1.0
    )

    #expect(abs(settings.artifactThresholdSec - 0.0009) < 1e-12)
    #expect(abs(settings.refractorySuspectThresholdSec - 0.0010) < 1e-12)
}

@Test
func qualitySettingsKeepsRefractoryAtLeastArtifactAfterMillisecondConversion() {
    let settings = SpikeQualitySettings(
        artifactThresholdMilliseconds: 1.2,
        refractorySuspectThresholdMilliseconds: 1.0
    )

    #expect(abs(settings.artifactThresholdSec - 0.0012) < 1e-12)
    #expect(abs(settings.refractorySuspectThresholdSec - 0.0012) < 1e-12)
}

@Test
func reportsArtifactAndRefractorySuspectISI() throws {
    let dataset = SpikeDataset(
        name: "qc",
        sourceDescription: "inline",
        trains: [
            SpikeTrain(name: "train_a", timestampsSec: [0.0, 0.0005, 0.0018, 0.0040])
        ]
    )

    let report = SpikeQualityAnalyzer.analyze(
        dataset: dataset,
        settings: SpikeQualitySettings(
            artifactThresholdSec: 0.0009,
            refractorySuspectThresholdSec: 0.0015,
            displayUnit: .milliseconds
        )
    )

    let row = try #require(report.rows.first)
    #expect(row.warningLevel == .warning)
    #expect(row.artifactISICount == 1)
    #expect(row.refractorySuspectISICount == 1)
    #expect(row.warningMessage.contains("Artifact ISI: 1 interval"))
    #expect(row.warningMessage.contains("Refractory-suspect ISI: 1 interval"))
    #expect(report.artifactDetails.count == 1)
}

@Test
func defaultDuplicateTimestampPolicyKeepsAndErrors() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: """
        train_a
        0
        0.001
        0.001
        0.003
        """,
        datasetName: "duplicates",
        sourceDescription: "inline",
        unit: .seconds,
        duplicatePolicy: .errorKeep
    )

    let report = SpikeQualityAnalyzer.analyze(
        dataset: dataset,
        settings: SpikeQualitySettings(
            artifactThresholdSec: 0.0009,
            refractorySuspectThresholdSec: 0.0015,
            displayUnit: .milliseconds
        )
    )

    let row = try #require(report.rows.first)
    #expect(dataset.trains.first?.spikeCount == 4)
    #expect(row.warningLevel == .error)
    #expect(row.duplicateTimestampCount == 1)
    #expect(row.artifactISICount == 1)
    #expect(row.warningMessage.contains("Duplicate timestamps retained: 1"))
    #expect(row.warningMessage.contains("Policy: Error, keep"))
    #expect(report.duplicateDetails.count == 1)
}

@Test
func warnKeepDuplicateTimestampPolicyKeepsAndWarns() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: """
        train_a
        0
        0.001
        0.001
        0.003
        """,
        datasetName: "duplicates",
        sourceDescription: "inline",
        unit: .seconds,
        duplicatePolicy: .warnKeep
    )

    let report = SpikeQualityAnalyzer.analyze(
        dataset: dataset,
        settings: SpikeQualitySettings(
            artifactThresholdSec: 0.0009,
            refractorySuspectThresholdSec: 0.0015,
            displayUnit: .milliseconds
        )
    )

    let row = try #require(report.rows.first)
    #expect(dataset.trains.first?.spikeCount == 4)
    #expect(row.warningLevel == .warning)
    #expect(row.duplicateTimestampCount == 1)
    #expect(row.warningMessage.contains("Duplicate timestamps retained: 1"))
    #expect(row.warningMessage.contains("Policy: Warn, keep"))
    #expect(report.duplicateDetails.count == 1)
}

@Test
func collapseExactDuplicatePolicyDropsRepeatedTimestampsAndWarns() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: """
        train_a
        0
        0.001
        0.001
        0.003
        """,
        datasetName: "collapse",
        sourceDescription: "inline",
        unit: .seconds,
        duplicatePolicy: .collapseExact
    )

    let report = SpikeQualityAnalyzer.analyze(
        dataset: dataset,
        settings: SpikeQualitySettings(
            artifactThresholdSec: 0.0009,
            refractorySuspectThresholdSec: 0.0015,
            displayUnit: .milliseconds
        )
    )

    let train = try #require(dataset.trains.first)
    let row = try #require(report.rows.first)
    #expect(train.spikeCount == 3)
    #expect(train.droppedDuplicateTimestampCount == 1)
    #expect(row.warningLevel == .warning)
    #expect(row.duplicateTimestampCount == 0)
    #expect(row.warningMessage.contains("Exact duplicate timestamps collapsed: 1"))
    #expect(report.duplicateDetails.isEmpty)
}

@Test
func datasetCanApplyCollapseExactPolicyAfterInitialImport() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: """
        train_a
        0
        0.001
        0.001
        0.003
        """,
        datasetName: "late-collapse",
        sourceDescription: "inline",
        unit: .seconds,
        duplicatePolicy: .errorKeep
    )

    let collapsed = dataset.applyingDuplicateTimestampPolicy(.collapseExact)
    let report = SpikeQualityAnalyzer.analyze(
        dataset: collapsed,
        settings: SpikeQualitySettings(
            artifactThresholdSec: 0.0009,
            refractorySuspectThresholdSec: 0.0015,
            displayUnit: .milliseconds
        )
    )

    let train = try #require(collapsed.trains.first)
    let row = try #require(report.rows.first)
    #expect(train.spikeCount == 3)
    #expect(row.duplicateTimestampCount == 0)
    #expect(row.droppedDuplicateTimestampCount == 1)
    #expect(report.droppedDuplicateTimestampCount == 1)
}

@Test
func parserSupportsHeaderlessMillisecondImport() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: """
        1000,2000
        1005,2010
        """,
        datasetName: "headerless",
        sourceDescription: "inline",
        unit: .milliseconds,
        hasHeader: false
    )

    #expect(dataset.trains.map(\.name) == ["Train 1", "Train 2"])
    #expect(abs(dataset.trains[0].timestampsSec[0] - 1.0) < 1e-12)
    #expect(abs(dataset.trains[0].timestampsSec[1] - 1.005) < 1e-12)
    #expect(abs(dataset.trains[1].timestampsSec[0] - 2.0) < 1e-12)
    #expect(abs(dataset.trains[1].timestampsSec[1] - 2.01) < 1e-12)
}
