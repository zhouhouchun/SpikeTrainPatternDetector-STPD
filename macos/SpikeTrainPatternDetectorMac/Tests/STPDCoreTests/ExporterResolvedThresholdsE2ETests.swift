import Foundation
import STPDCore
import Testing

// End-to-end coverage of the exporter `resolved_thresholds` column, kept in a self-contained file. P10B: the manual
// hard-threshold SCOPE note (P10) is auditable in the exported CSV — present in BOTH the `decision_path` and the
// dedicated `resolved_thresholds` column for an in-scope hard-gated candidate, and absent on out-of-scope trains and
// for soft-only profiles.

private func p10bBurstDataset() -> SpikeDataset {
    var t = 0.0
    var times = [0.0]
    for block in 0..<5 {
        for _ in 0..<6 { t += 0.40; times.append(t) }
        if block < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } }
    }
    return SpikeDataset(name: "p10b", sourceDescription: "p10b", trains: [
        SpikeTrain(name: "a", timestampsSec: times),
        SpikeTrain(name: "b", timestampsSec: times),
    ])
}

private func p10bExportedRows(_ profile: ManualThresholdProfile, _ scope: ManualThresholdScope, _ ds: SpikeDataset)
    -> (header: [String], rows: [[String: String]]) {
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, manualThresholdProfile: profile, manualThresholdScope: scope)
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: ds, run: run, annotations: run.eventAnnotations(in: ds), reviewStatuses: [:],
        exportedAt: ISO8601DateFormatter().date(from: "2026-06-29T00:00:00Z")!)
    let parsed = parseResolvedThresholdsCSV(csv)
    let header = parsed.first ?? []
    let rows = parsed.dropFirst().map { Dictionary(uniqueKeysWithValues: zip(header, $0)) }
    return (header, rows)
}

@Test
func p10bScopeNoteAppearsInBothDecisionPathAndResolvedThresholdsForInScopeTrain() throws {
    let ds = p10bBurstDataset()
    let aID = ds.trains[0].id
    let hardSeedGate = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.020)))
    let (_, rows) = p10bExportedRows(hardSeedGate, ManualThresholdScope(kind: .selectedTrains, trainIDs: [aID]), ds)
    let token = "manual_threshold_scope=selected_trains(count=1,train_id=\(aID))"

    let aRows = rows.filter { $0["train_name"] == "a" }
    let bRows = rows.filter { $0["train_name"] == "b" }
    #expect(!aRows.isEmpty)
    #expect(!bRows.isEmpty)

    // In-scope train "a": at least one exported candidate carries the scope note in BOTH columns, and every row that
    // has it in decision_path also has it in resolved_thresholds (the projection mirrors the decision_path note).
    #expect(aRows.contains { ($0["decision_path"] ?? "").contains(token) })
    #expect(aRows.contains { ($0["resolved_thresholds"] ?? "").contains(token) })
    for row in aRows where (row["decision_path"] ?? "").contains("manual_threshold_scope=") {
        #expect((row["resolved_thresholds"] ?? "").contains(token))
    }

    // Out-of-scope train "b": no scope note in either column (its hard gate was dropped to automatic in P9).
    for row in bRows {
        #expect(!(row["decision_path"] ?? "").contains("manual_threshold_scope"))
        #expect(!(row["resolved_thresholds"] ?? "").contains("manual_threshold_scope"))
    }
}

@Test
func p10bSoftOnlyProfileHasNoScopeNoteInCSV() throws {
    let ds = p10bBurstDataset()
    let aID = ds.trains[0].id
    let soft = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .softAnchor, valueSec: 0.060)))
    let (_, rows) = p10bExportedRows(soft, ManualThresholdScope(kind: .selectedTrains, trainIDs: [aID]), ds)
    for row in rows {
        #expect(!(row["decision_path"] ?? "").contains("manual_threshold_scope"))
        #expect(!(row["resolved_thresholds"] ?? "").contains("manual_threshold_scope"))
    }
}

private func parseResolvedThresholdsCSV(_ csv: String) -> [[String]] {
    var rows: [[String]] = []
    var row: [String] = []
    var field = ""
    var isQuoted = false
    var index = csv.startIndex

    while index < csv.endIndex {
        let character = csv[index]

        if character == "\"" {
            let next = csv.index(after: index)
            if isQuoted, next < csv.endIndex, csv[next] == "\"" {
                field.append("\"")
                index = csv.index(after: next)
                continue
            }
            isQuoted.toggle()
        } else if character == ",", !isQuoted {
            row.append(field)
            field = ""
        } else if character == "\n", !isQuoted {
            row.append(field)
            if !row.allSatisfy(\.isEmpty) {
                rows.append(row)
            }
            row = []
            field = ""
        } else if character != "\r" {
            field.append(character)
        }

        index = csv.index(after: index)
    }

    if !field.isEmpty || !row.isEmpty {
        row.append(field)
        rows.append(row)
    }

    return rows
}
