import Foundation
@testable import STPDCore
import Testing

private func taskEventFixtureURL() -> URL {
    URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent()
        .appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
}

private func taskEventFixtureDataset() throws -> SpikeDataset {
    let url = taskEventFixtureURL()
    return try CSVSpikeMatrixParser.parse(
        contents: String(contentsOf: url, encoding: .utf8),
        datasetName: "tonic_baseline_5x5",
        sourceDescription: url.path,
        unit: .seconds
    )
}

private func exactDouble(_ value: Double?) -> String {
    value.map { String($0.bitPattern) } ?? "nil"
}

private let runtimeUUIDPattern = try! NSRegularExpression(
    pattern: #"(?i)\b[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}\b"#
)

/// Captures every reflected field while removing only per-run UUID entropy.
/// Candidate IDs can embed `SpikeDataset.id`, which is intentionally regenerated
/// for two otherwise-identical datasets. UUID suffixes and all non-UUID content
/// remain in the contract, so scientific and audit differences still fail closed.
private func scientificContract<T>(_ value: T) -> String {
    let reflected = String(reflecting: value)
    return runtimeUUIDPattern.stringByReplacingMatches(
        in: reflected,
        range: NSRange(reflected.startIndex..., in: reflected),
        withTemplate: "<RUNTIME_UUID>"
    )
}

private func expectScientificContractsEqual<T>(
    _ baseline: [T],
    _ annotated: [T],
    context: String
) {
    #expect(baseline.count == annotated.count)
    guard baseline.count == annotated.count else { return }

    for index in baseline.indices {
        if scientificContract(baseline[index]) != scientificContract(annotated[index]) {
            Issue.record("\(context)[\(index)] differs after runtime-UUID normalization")
        }
    }
}

private func resolutionContract(_ run: ClassicAnchorDetectionRun) -> [String] {
    run.resolutions.flatMap { resolution in
        var rows = [
            [
                "resolution",
                resolution.trainID,
                resolution.trainName,
                exactDouble(resolution.minValidISISec),
                exactDouble(resolution.histogramBinWidthSec),
                String(resolution.validISICount)
            ].joined(separator: "|")
        ]
        rows += AdaptiveBandPattern.allCases.map { pattern in
            guard let band = resolution.bands[pattern] else {
                return "band|\(pattern.rawValue)|nil"
            }
            return [
                "band",
                pattern.rawValue,
                exactDouble(band.seedLowerSec),
                exactDouble(band.seedUpperSec),
                exactDouble(band.bridgeUpperSec),
                exactDouble(band.contrastS),
                band.primarySource.rawValue
            ].joined(separator: "|")
        }
        rows += resolution.thresholdRows.sorted { $0.id < $1.id }.map { row in
            [
                "threshold",
                row.id,
                row.pattern.rawValue,
                row.field.rawValue,
                exactDouble(row.histogramSec),
                exactDouble(row.defaultSec),
                exactDouble(row.effectiveSec),
                row.source.rawValue
            ].joined(separator: "|")
        }
        return rows
    }
}

private func expectDerivedOutputRejected(_ csv: String, sourceDescription: String = "inline") {
    do {
        _ = try CSVSpikeMatrixParser.parse(
            contents: csv,
            datasetName: "derived",
            sourceDescription: sourceDescription,
            unit: .seconds
        )
        Issue.record("Expected derivedOutputLikely, but parsing succeeded")
    } catch CSVSpikeMatrixParserError.derivedOutputLikely {
        // Expected.
    } catch {
        Issue.record("Expected derivedOutputLikely, got \(error)")
    }
}

@Test
func csvTaskEvent_columnRuleMatchesRAdversarialMatrix() {
    let cases: [(String, Bool)] = [
        ("event", true),
        ("\u{FEFF}event", true),
        (" Event ", true),
        ("EVENT_A", true),
        ("event cue", true),
        ("event.cue", true),
        ("event:cue", true),
        ("event-cue", true),
        ("event_", true),
        ("events", false),
        ("eventA", false),
        ("event/cue", false),
        ("event+cue", false),
        ("pre_event", false),
        ("event\tcue", false),
        ("", false),
        ("burst_response_1_s", false)
    ]

    for (name, expected) in cases {
        #expect(CSVSpikeMatrixParser.isTaskEventColumn(name) == expected)
    }
    #expect(CSVSpikeMatrixParser.cleanTaskEventName("event") == "Event")
    #expect(CSVSpikeMatrixParser.cleanTaskEventName("\u{FEFF}event") == "Event")
    #expect(CSVSpikeMatrixParser.cleanTaskEventName(" Event_stim_1 ") == "stim 1")
    #expect(CSVSpikeMatrixParser.cleanTaskEventName("event: cue") == "cue")
}

@Test
func csvTaskEvent_bomPrefixedEventHeaderIsExcludedFromSpikeTrains() throws {
    let csv = "\u{FEFF}event,train_a\n1,0\n2,1"

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "bom_event_header",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.map(\.name) == ["train_a"])
    #expect(dataset.trains[0].timestampsSec == [0, 1])
    #expect(dataset.taskEvents.map(\.timeSec) == [1, 2])
    #expect(dataset.taskEvents.allSatisfy { $0.name == "Event" && $0.column == "event" })
}

@Test
func csvTaskEvent_trackedFixtureLoadsAsTenTrainsAndEightEvents() throws {
    let dataset = try taskEventFixtureDataset()

    #expect(dataset.trains.count == 10)
    #expect(dataset.totalSpikeCount == 736)
    #expect(!dataset.trains.contains { CSVSpikeMatrixParser.isTaskEventColumn($0.name) })
    #expect(dataset.taskEvents.count == 8)
    #expect(dataset.taskEvents.map(\.timeSec) == [5, 8, 11, 14, 17, 20, 23, 26].map(Double.init))
    #expect(dataset.taskEvents.map(\.eventIndex) == Array(1...8))
    #expect(dataset.taskEvents.allSatisfy { $0.name == "Event" && $0.column == "event" })
    #expect(dataset.taskEvents.allSatisfy { $0.source == taskEventFixtureURL().path })
    #expect(Set(dataset.taskEvents.map(\.id)).count == 8)
    #expect(Set(dataset.taskEvents.map(\.trialID)).count == 8)
}

@Test
func csvTaskEvent_fileWithoutEventColumnsKeepsSpikeParsingUnchanged() throws {
    let csv = """
    train_a,train_b
    10.0,20.0
    10.5,20.1
    ,20.4
    11.0,
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "plain",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.taskEvents.isEmpty)
    #expect(dataset.trains.map(\.name) == ["train_a", "train_b"])
    #expect(dataset.trains[0].timestampsSec == [10.0, 10.5, 11.0])
    #expect(dataset.trains[1].timestampsSec == [20.0, 20.1, 20.4])
}

@Test
func csvTaskEvent_multipleColumnsScaleIgnoreInvalidCellsAndMatchRIdentityRules() throws {
    let csv = """
    train_a,Event_A,event:A
    0,1000,1000
    1000,NA,2000
    2000,Inf,
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "multiple_events",
        sourceDescription: "inline",
        unit: .milliseconds
    )

    #expect(dataset.trains.count == 1)
    #expect(dataset.trains[0].timestampsSec == [0, 1, 2])
    #expect(dataset.taskEvents.map(\.timeSec) == [1, 1, 2])
    #expect(dataset.taskEvents.map(\.eventIndex) == [1, 1, 2])
    #expect(dataset.taskEvents.map(\.name) == ["A", "A", "A"])
    #expect(dataset.taskEvents.allSatisfy { $0.source == "inline" })
    #expect(dataset.taskEvents.map(\.id) == [
        "event_1_A_1.000000",
        "event_1_A_1.000000_1",
        "event_2_A_2.000000"
    ])
    #expect(dataset.taskEvents.map(\.trialID) == ["A_1", "A_1_1", "A_2"])
}

@Test
func csvTaskEvent_millisecondIdentityUsesRDivisionRounding() throws {
    let csv = """
    train_a,event
    0,9.0005
    1,
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "rounding",
        sourceDescription: "inline",
        unit: .milliseconds
    )

    let event = try #require(dataset.taskEvents.first)
    #expect(event.timeSec == 9.0005 / 1_000.0)
    #expect(event.id == "event_1_Event_0.009000")
}

@Test
func csvTaskEvent_duplicateHeadersPreservePhysicalColumnProvenance() throws {
    let csv = """
    train_a,event,event
    0,1,2
    1,,3
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "duplicate_event_headers",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.taskEvents.map(\.column) == ["event", "event_1", "event_1"])
    #expect(dataset.taskEvents.map(\.timeSec) == [1, 2, 3])
    #expect(Set(dataset.taskEvents.map(\.id)).count == 3)
}

@Test
func csvTaskEvent_validEventColumnsDoNotTripDerivedGuard() throws {
    let csv = """
    train_a,Event_A,event:stim,event-cue
    10,1,2,3
    11,,,
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "multi_event",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.count == 1)
    #expect(dataset.taskEvents.count == 3)
    #expect(Set(dataset.taskEvents.map(\.name)) == ["A", "stim", "cue"])
}

@Test
func csvTaskEvent_derivedTableWithEventColumnStillRejects() {
    let csv = """
    Spike_train,Start_time,event,threshold,score
    train_a,0,5,0.1,0.9
    """

    expectDerivedOutputRejected(csv)
}

@Test
func csvTaskEvent_minimalDerivedEventTablesReject() {
    expectDerivedOutputRejected("""
    event,start_time,end_time
    5,4,6
    """)
    expectDerivedOutputRejected("""
    event,score
    5,0.9
    """)
    expectDerivedOutputRejected("\u{FEFF}event,start_time\n5,4")
    expectDerivedOutputRejected("event,trial_id\n5,trial_1")
}

@Test
func csvTaskEvent_invalidEventLikeHeadersFailClosed() {
    for header in ["events", "eventA", "event/cue", "event+cue", "event\tcue"] {
        expectDerivedOutputRejected("""
        train_a,\(header)
        0,1
        1,2
        """)
    }
}

@Test
func csvTaskEvent_taskAnnotationsAreScientificallyIsolatedFromDetection() throws {
    let parsed = try taskEventFixtureDataset()
    let baselineDataset = SpikeDataset(
        name: parsed.name,
        sourceDescription: parsed.sourceDescription,
        trains: parsed.trains
    )
    let adversarialEvents = (0..<64).map { index in
        TaskEvent(
            id: "adversarial_\(index)",
            name: ["Burst", "Tonic", "Pause", "High frequency"][index % 4],
            timeSec: Double(index / 2 - 8) * 0.75,
            column: "event_\(index % 3)",
            eventIndex: index + 1,
            trialID: "trial_\(index)",
            source: "adversarial_task_annotations"
        )
    }
    let annotatedDataset = SpikeDataset(
        name: parsed.name,
        sourceDescription: parsed.sourceDescription,
        trains: parsed.trains,
        taskEvents: parsed.taskEvents + adversarialEvents
    )

    #expect(baselineDataset.trains == annotatedDataset.trains)
    let baselineRun = ClassicAnchorDetectionPipeline.run(dataset: baselineDataset)
    let annotatedRun = ClassicAnchorDetectionPipeline.run(dataset: annotatedDataset)

    #expect(Set(baselineRun.candidates.map(\.id)).count == baselineRun.candidates.count)
    #expect(Set(annotatedRun.candidates.map(\.id)).count == annotatedRun.candidates.count)
    #expect(
        Set(baselineRun.hfsBurstArbitrationAuditRows.map(\.id)).count ==
            baselineRun.hfsBurstArbitrationAuditRows.count
    )
    #expect(
        Set(annotatedRun.hfsBurstArbitrationAuditRows.map(\.id)).count ==
            annotatedRun.hfsBurstArbitrationAuditRows.count
    )

    #expect(baselineRun.results.map(\.trainID) == annotatedRun.results.map(\.trainID))
    #expect(baselineRun.results.map(\.trainName) == annotatedRun.results.map(\.trainName))

    for train in parsed.trains {
        let baselineResult = try #require(baselineRun.result(for: train.id))
        let annotatedResult = try #require(annotatedRun.result(for: train.id))
        #expect(baselineResult.trainID == annotatedResult.trainID)
        #expect(baselineResult.trainName == annotatedResult.trainName)
        #expect(Set(baselineResult.candidates.map(\.id)).count == baselineResult.candidates.count)
        #expect(Set(annotatedResult.candidates.map(\.id)).count == annotatedResult.candidates.count)
        expectScientificContractsEqual(
            baselineResult.candidates,
            annotatedResult.candidates,
            context: "candidate contract for \(train.id)"
        )
        expectScientificContractsEqual(
            baselineResult.hfsBurstArbitrationAuditRows,
            annotatedResult.hfsBurstArbitrationAuditRows,
            context: "audit contract for \(train.id)"
        )
    }

    expectScientificContractsEqual(
        baselineRun.candidates,
        annotatedRun.candidates,
        context: "flattened candidate contract"
    )
    expectScientificContractsEqual(
        baselineRun.hfsBurstArbitrationAuditRows,
        annotatedRun.hfsBurstArbitrationAuditRows,
        context: "flattened audit contract"
    )
    #expect(resolutionContract(baselineRun) == resolutionContract(annotatedRun))
    #expect(
        baselineRun.resolutions.map(\.seedBandProfile) ==
            annotatedRun.resolutions.map(\.seedBandProfile)
    )
    #expect(
        baselineRun.resolutions.map(\.structuralSeedSummary) ==
            annotatedRun.resolutions.map(\.structuralSeedSummary)
    )
    #expect(baselineRun.datasetStructuralSeedSummary == annotatedRun.datasetStructuralSeedSummary)
    #expect(baselineRun.datasetRerunProvenance == annotatedRun.datasetRerunProvenance)
    #expect(baselineRun.datasetISIDistribution == annotatedRun.datasetISIDistribution)
}
