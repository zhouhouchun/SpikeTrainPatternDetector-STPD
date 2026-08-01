import Foundation
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

@Test
func duplicateHeadersProduceUniqueTrainIDsWithoutCrashingPipelineOrExport() throws {
    let csv = """
    unit,unit,unit
    0.10,0.50,0.20
    0.20,1.00,0.40
    0.30,1.50,0.60
    0.40,2.00,0.80
    0.50,2.50,1.00
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "dup_headers",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.count == 3)
    #expect(dataset.trains.allSatisfy { $0.name == "unit" })
    // Stable, unique IDs even though the display headers collide.
    #expect(Set(dataset.trains.map(\.id)).count == 3)
    // The original crash site: Dictionary(uniqueKeysWithValues:) keyed on train.id.
    let byID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
    #expect(byID.count == 3)

    // Pipeline + annotation + export paths must not trap with duplicate headers.
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    #expect(run.results.count == 3)

    let annotations = run.eventAnnotations(
        in: dataset,
        selectedOnly: false,
        tracks: Set(ClassicAnchorSemanticTrack.allCases)
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-20T00:00:00Z"))
    let exportCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: annotations,
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )
    #expect(!exportCSV.isEmpty)
}

@Test
func malformedInternalCellThrowsInsteadOfFabricatingLongISI() {
    // Without the guard, the BAD_TOKEN row would be skipped, merging 0.10 and
    // 0.30 into a single artificially long 0.20 s interval.
    let csv = """
    train_a
    0.10
    BAD_TOKEN
    0.30
    """

    #expect(throws: CSVSpikeMatrixParserError.self) {
        _ = try CSVSpikeMatrixParser.parse(
            contents: csv,
            datasetName: "malformed",
            sourceDescription: "inline",
            unit: .seconds
        )
    }
}

@Test
func internalNonFiniteAndNullTokensThrow() {
    for token in ["NaN", "Inf", "null", "NA"] {
        let csv = "train_a\n0.10\n\(token)\n0.30\n"
        #expect(throws: CSVSpikeMatrixParserError.self) {
            _ = try CSVSpikeMatrixParser.parse(
                contents: csv,
                datasetName: "malformed",
                sourceDescription: "inline",
                unit: .seconds
            )
        }
    }
}

@Test
func trailingInvalidPaddingRemainsAllowed() throws {
    // Trailing NA / empty padding (as used by the bundled wide-matrix CSVs) is
    // allowed because it cannot collapse a real interval.
    let csv = """
    train_a,train_b
    0.10,0.50
    0.20,NA
    0.30,
    """

    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "trailing_padding",
        sourceDescription: "inline",
        unit: .seconds
    )

    #expect(dataset.trains.count == 2)
    #expect(dataset.trains[0].timestampsSec == [0.10, 0.20, 0.30])
    // train_b keeps only its single real value; the trailing NA and empty cell are padding.
    #expect(dataset.trains[1].timestampsSec == [0.50])
}

@Test
func trailingMissingPaddingTokensRemainAllowed() throws {
    for token in ["NA", "nan", "Null"] {
        let csv = "train_a,train_b\n0.10,0.50\n0.20,\(token)\n0.30,\(token)\n"
        let dataset = try CSVSpikeMatrixParser.parse(
            contents: csv,
            datasetName: "padding",
            sourceDescription: "inline",
            unit: .seconds
        )

        #expect(dataset.trains.count == 2)
        #expect(dataset.trains[0].timestampsSec == [0.10, 0.20, 0.30])
        #expect(dataset.trains[1].timestampsSec == [0.50])
    }
}

@Test
func trailingGarbageTokenThrows() {
    // BAD_TOKEN after the last finite value is corrupted content, not padding.
    expectMalformedCellThrown("train_a\n0.10\n0.20\nBAD_TOKEN\n")
}

@Test
func trailingInfinityTokenThrows() {
    for token in ["Inf", "-Inf", "Infinity"] {
        expectMalformedCellThrown("train_a\n0.10\n0.20\n\(token)\n")
    }
}

@Test
func allInvalidNonEmptyColumnThrows() {
    // A column with no finite spikes but with non-empty tokens must not be silently
    // dropped, whether the tokens are padding-like (NA/NaN) or garbage/Inf.
    for token in ["BAD_TOKEN", "NA", "nan", "Inf"] {
        expectMalformedCellThrown("train_a,train_b\n0.10,\(token)\n0.20,\(token)\n")
    }
}

@Test
func allEmptyColumnStillDropsOrNoNumericSpikes() throws {
    // A fully empty column is padding and is dropped, not an error.
    let droppedCSV = "train_a,train_b\n0.10,\n0.20,\n0.30,\n"
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: droppedCSV,
        datasetName: "empty_col",
        sourceDescription: "inline",
        unit: .seconds
    )
    #expect(dataset.trains.count == 1)
    #expect(dataset.trains[0].name == "train_a")
    #expect(dataset.trains[0].timestampsSec == [0.10, 0.20, 0.30])

    // When every column is empty, the existing no-numeric-spikes error stands.
    let allEmptyCSV = "train_a,train_b\n,\n,\n"
    #expect(throws: CSVSpikeMatrixParserError.self) {
        _ = try CSVSpikeMatrixParser.parse(
            contents: allEmptyCSV,
            datasetName: "all_empty",
            sourceDescription: "inline",
            unit: .seconds
        )
    }
}

private func expectMalformedCellThrown(_ csv: String, unit: SpikeTimeUnit = .seconds) {
    do {
        _ = try CSVSpikeMatrixParser.parse(
            contents: csv,
            datasetName: "malformed",
            sourceDescription: "inline",
            unit: unit
        )
        Issue.record("Expected CSVSpikeMatrixParserError.malformedCell to be thrown")
    } catch let error as CSVSpikeMatrixParserError {
        if case .malformedCell = error {
            return
        }
        Issue.record("Expected .malformedCell, got \(error)")
    } catch {
        Issue.record("Unexpected error type: \(error)")
    }
}

// MARK: - EV-1 Step A: task-event columns (R `^event($|[_ .:-])` parity).

/// Builds an example like `tonic_baseline_repeated_stimulus_burst_pause_5x5.csv`: 10 spike columns + one
/// `event` column whose finite cells are the 8 stimulus times [5, 8, ..., 26].
private func taskEventExampleCSV() -> String {
    let eventTimes = [5, 8, 11, 14, 17, 20, 23, 26]
    let header = ((1...10).map { "t\($0)" } + ["event"]).joined(separator: ",")
    var lines = [header]
    for (rowIndex, eventTime) in eventTimes.enumerated() {
        let spikes = (1...10).map { column in
            String(format: "%.3f", Double(rowIndex + 1) * 0.1 + Double(column) * 0.001)
        }
        lines.append((spikes + [String(eventTime)]).joined(separator: ","))
    }
    return lines.joined(separator: "\n")
}

@Test
func exampleFileLoadsAsTenTrainsAndEightTaskEvents() throws {
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: taskEventExampleCSV(),
        datasetName: "example",
        sourceDescription: "tonic_baseline_repeated_stimulus_burst_pause_5x5.csv",
        unit: .seconds
    )
    #expect(dataset.trains.count == 10)                         // 10 spike trains, NOT 11
    #expect(dataset.taskEvents.count == 8)                      // 8 task events
    // No spike train is the event column.
    #expect(!dataset.trains.contains { $0.name.lowercased() == "event" || $0.id.lowercased() == "event" })
    // Event times are exactly the stimulus seconds, sorted.
    #expect(dataset.taskEvents.map(\.timeSec) == [5, 8, 11, 14, 17, 20, 23, 26].map(Double.init))
    // "event" column cleans to "Event"; 1-based row indices are preserved.
    #expect(dataset.taskEvents.allSatisfy { $0.name == "Event" && $0.column == "event" })
    #expect(dataset.taskEvents.map(\.eventIndex) == Array(1...8))
}

@Test
func fileWithoutEventColumnsHasNoTaskEventsAndIsUnchanged() throws {
    let csv = """
    train_a,train_b
    10,20
    11,21
    """
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv, datasetName: "plain", sourceDescription: "inline", unit: .seconds
    )
    #expect(dataset.trains.count == 2)
    #expect(dataset.taskEvents.isEmpty)
    #expect(dataset.trains.map(\.name) == ["train_a", "train_b"])
}

@Test
func multipleEventColumnsExtractWithCleanedNames() throws {
    let csv = """
    train_a,Event_A,event:stim
    10,1.0,2.5
    11,,3.0
    """
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv, datasetName: "multi", sourceDescription: "inline", unit: .seconds
    )
    #expect(dataset.trains.count == 1)                          // only train_a is a spike train
    #expect(!dataset.trains.contains { $0.name.lowercased().hasPrefix("event") })
    #expect(Set(dataset.taskEvents.map(\.name)) == ["A", "stim"])   // "Event_A" -> "A", "event:stim" -> "stim"
    #expect(dataset.taskEvents.map(\.timeSec).sorted() == [1.0, 2.5, 3.0])
    #expect(dataset.taskEvents.filter { $0.name == "stim" }.count == 2)
}

@Test
func taskEventColumnRuleMatchesRPattern() {
    // `^event($|[_ .:-])`, case-insensitive.
    #expect(CSVSpikeMatrixParser.isTaskEventColumn("event"))
    #expect(CSVSpikeMatrixParser.isTaskEventColumn("Event_A"))
    #expect(CSVSpikeMatrixParser.isTaskEventColumn("event:stim"))
    #expect(CSVSpikeMatrixParser.isTaskEventColumn("EVENT 1"))
    #expect(CSVSpikeMatrixParser.isTaskEventColumn("event-2"))
    #expect(CSVSpikeMatrixParser.isTaskEventColumn("event.x"))
    #expect(!CSVSpikeMatrixParser.isTaskEventColumn("events"))      // no separator after "event"
    #expect(!CSVSpikeMatrixParser.isTaskEventColumn("eventA"))
    #expect(!CSVSpikeMatrixParser.isTaskEventColumn("burst_response_1_s"))
    #expect(CSVSpikeMatrixParser.cleanTaskEventName("event") == "Event")
    #expect(CSVSpikeMatrixParser.cleanTaskEventName("Event_A") == "A")
    #expect(CSVSpikeMatrixParser.cleanTaskEventName("event_stim_1") == "stim 1")
}

@Test
func eventColumnTokensAreToleratedAndScaledWithUnit() throws {
    // Blank and non-finite event cells are ignored (R suppressWarnings + finite filter); ms scales to s.
    let csv = """
    train_a,event
    1000,5000
    1250,
    1500,NA
    1750,8000
    """
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv, datasetName: "ms", sourceDescription: "inline", unit: .milliseconds
    )
    #expect(dataset.trains.count == 1)
    #expect(dataset.taskEvents.map(\.timeSec) == [5.0, 8.0])    // 5000 ms, 8000 ms -> seconds; blank/NA ignored
    #expect(dataset.taskEvents.map(\.eventIndex) == [1, 4])
}

// MARK: - EV-1A: task-event columns must not trip the derived-CSV guard.

@Test
func multipleTaskEventColumnsDoNotTripDerivedGuard() throws {
    // Three valid task-event columns previously contributed 3 `event_*` derived-header hits and got the
    // file wrongly rejected. They must now be ignored for the derived count.
    let csv = """
    train_a,Event_A,event:stim,event-cue
    10,1,2,3
    11,,,
    """
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv, datasetName: "multi-event", sourceDescription: "inline", unit: .seconds
    )
    #expect(dataset.trains.count == 1)
    #expect(dataset.trains.first?.name == "train_a")
    #expect(dataset.taskEvents.count == 3)
    #expect(Set(dataset.taskEvents.map(\.name)) == ["A", "stim", "cue"])
    #expect(dataset.taskEvents.map(\.timeSec).sorted() == [1.0, 2.0, 3.0])
}

@Test
func derivedCSVWithEventColumnIsStillRejected() {
    // Excluding task-event columns must NOT weaken protection: a genuine derived table that also has an
    // `event` column still trips the guard on its other derived headers.
    let csv = """
    Spike_train,Start_time,event,threshold,score
    train_a,0,5,0.1,0.9
    """
    do {
        _ = try CSVSpikeMatrixParser.parse(
            contents: csv, datasetName: "derived", sourceDescription: "inline", unit: .seconds
        )
        Issue.record("Expected .derivedOutputLikely, but parse succeeded")
    } catch CSVSpikeMatrixParserError.derivedOutputLikely {
        // expected
    } catch {
        Issue.record("Expected .derivedOutputLikely, got \(error)")
    }
}
