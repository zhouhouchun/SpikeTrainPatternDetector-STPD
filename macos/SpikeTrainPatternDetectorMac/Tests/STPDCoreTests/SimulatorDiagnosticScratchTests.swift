import Foundation
@testable import STPDCore
import Testing

private struct SimulatorIntervalRow {
    let trainNumber: Int
    let startSec: Double
    let endSec: Double
    let isiSec: Double
    let label: String
    let isiIndex: Int
}

@Test
func simulatorCurrentPipelineDiagnosticScratch() throws {
    let rawPath = "/Users/zark/Desktop/SPIKE_TRAIN_SIMULATOR.csv"
    let labelPath = "/Users/zark/Desktop/SPIKE_TRAIN_SIMULATOR_V13_4_12_interval_table.csv"
    let rawURL = URL(fileURLWithPath: rawPath)
    let labelURL = URL(fileURLWithPath: labelPath)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: String(contentsOf: rawURL, encoding: .utf8),
        datasetName: "SPIKE_TRAIN_SIMULATOR",
        sourceDescription: rawPath,
        unit: .seconds
    )
    let rows = try parseIntervalRows(String(contentsOf: labelURL, encoding: .utf8))
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
        qualitySettings: SpikeQualitySettings()
    )
    let annotations = run.eventAnnotations(
        in: dataset,
        selectedOnly: true,
        tracks: [.event, .state, .gap],
        includeEvidenceOnly: false,
        exclusiveFinalProjection: true
    )

    let trainsByNumber = Dictionary(uniqueKeysWithValues: dataset.trains.enumerated().map { ($0.offset + 1, $0.element) })
    let candidatesByID = Dictionary(uniqueKeysWithValues: run.candidates.map { ($0.id, $0) })
    let candidatesByTrain = Dictionary(grouping: run.candidates, by: \.trainID)
    var winner: [String: ClassicAnchorEventAnnotation] = [:]
    for annotation in annotations {
        guard let train = trainsByNumber.first(where: { $0.value.id == annotation.trainID })?.value,
              let covered = annotation.coveredISIIndices(in: train) else {
            continue
        }
        for index in covered {
            winner["\(annotation.trainID)#\(index)"] = annotation
        }
    }

    var confusion: [String: [String: Int]] = [:]
    var falseBurstRows: [(SimulatorIntervalRow, ClassicAnchorEventAnnotation, ClassicAnchorCandidate?)] = []
    var missedBurstRows: [(SimulatorIntervalRow, String)] = []
    var tonicAsBurstRows: [(SimulatorIntervalRow, ClassicAnchorEventAnnotation, ClassicAnchorCandidate?)] = []
    var noisyAsTonicRows: [(SimulatorIntervalRow, ClassicAnchorEventAnnotation, ClassicAnchorCandidate?)] = []

    for row in rows {
        guard let train = trainsByNumber[row.trainNumber] else {
            continue
        }
        let annotation = winner["\(train.id)#\(row.isiIndex)"]
        let detected = detectionLabel(for: annotation)
        confusion[row.label, default: [:]][detected, default: 0] += 1
        if detected == "Tonic", row.label == "Noisy", let annotation {
            let candidate = candidatesByID[annotation.candidateID]
            noisyAsTonicRows.append((row, annotation, candidate))
        }
        if detected == "Burst", row.label != "Burst" {
            let candidate = annotation.flatMap { candidatesByID[$0.candidateID] }
            if let annotation {
                falseBurstRows.append((row, annotation, candidate))
                if row.label == "Tonic" {
                    tonicAsBurstRows.append((row, annotation, candidate))
                }
            }
        }
        if row.label == "Burst", detected != "Burst" {
            missedBurstRows.append((row, detected))
        }
    }

    var report: [String] = []
    report.append("SIMULATOR_CURRENT_PIPELINE_DIAGNOSTIC")
    report.append("dataset_trains=\(dataset.trains.count) spikes=\(dataset.totalSpikeCount) candidates=\(run.candidateCount) selected=\(run.selectedAutoCount)")
    if let perf = run.performanceReport {
        report.append(String(format: "pipeline_wall_ms=%.3f", locale: Locale(identifier: "en_US_POSIX"), perf.totalWallTimeMs))
    }
    report.append("")
    report.append("TRAIN_RESOLUTIONS")
    for (index, train) in dataset.trains.enumerated() {
        let resolution = run.resolution(for: train.id)
        let burst = resolution?.burstBand
        let structural = resolution?.structuralSeedSummary
        report.append([
            "Train \(index + 1)",
            train.name,
            "spikes=\(train.spikeCount)",
            "bandSeed=\(formatRange(burst?.seedLowerSec, burst?.seedUpperSec))",
            "bandBridge=\(formatSeconds(burst?.bridgeUpperSec))",
            "structSeedUpper=\(formatSeconds(structural?.burstSeedUpperSec))",
            "structBridge=\(formatSeconds(structural?.burstBridgeUpperSec))",
            "structPause=\(formatSeconds(structural?.pauseSeedLowerSec))"
        ].joined(separator: " | "))
    }
    report.append("")
    report.append("CONFUSION_GT_ROWS")
    let gtLabels = confusion.keys.sorted()
    let detLabels = Array(Set(confusion.values.flatMap(\.keys))).sorted()
    report.append((["GT\\DET"] + detLabels).joined(separator: "\t"))
    for gt in gtLabels {
        report.append(([gt] + detLabels.map { String(confusion[gt]?[$0] ?? 0) }).joined(separator: "\t"))
    }
    report.append("")
    report.append("SUMMARY")
    report.append("non_burst_detected_as_burst=\(falseBurstRows.count)")
    report.append("tonic_detected_as_burst=\(tonicAsBurstRows.count)")
    report.append("gt_burst_not_detected_as_burst=\(missedBurstRows.count)")
    report.append("noisy_detected_as_tonic=\(noisyAsTonicRows.count)")
    report.append("")
    report.append("FALSE_BURST_EXAMPLES")
    for entry in falseBurstRows.prefix(30) {
        report.append(describeFalseBurst(entry.0, entry.1, entry.2))
    }
    report.append("")
    report.append("NOISY_AS_TONIC_EXAMPLES")
    for entry in noisyAsTonicRows.prefix(30) {
        report.append(describeStateMismatch(entry.0, entry.1, entry.2))
    }
    report.append("")
    report.append("MISSED_BURST_EXAMPLES")
    for entry in missedBurstRows.prefix(30) {
        let train = trainsByNumber[entry.0.trainNumber]
            let overlaps = train.map { train in
                candidatesByTrain[train.id, default: []]
                    .filter { candidate in
                        min(candidate.startISIIndex, candidate.endISIIndex) <= entry.0.isiIndex &&
                        max(candidate.startISIIndex, candidate.endISIIndex) >= entry.0.isiIndex
                }
                .sorted { lhs, rhs in
                    if lhs.selectedForAuto != rhs.selectedForAuto {
                        return lhs.selectedForAuto && !rhs.selectedForAuto
                    }
                    return lhs.priority > rhs.priority
                    }
            } ?? []
            let localRows = rows.filter { $0.trainNumber == entry.0.trainNumber }
            let localWindow = localRows
                .filter { abs($0.isiIndex - entry.0.isiIndex) <= 4 }
                .map { row -> String in
                    let detected = train.flatMap { winner["\($0.id)#\(row.isiIndex)"] }.map(detectionLabel) ?? "None"
                    return String(
                        format: "%d:%@/%@/%.4f",
                        locale: Locale(identifier: "en_US_POSIX"),
                        row.isiIndex,
                        row.label,
                        detected,
                        row.isiSec
                    )
                }
                .joined(separator: " ")
            let overlapSummary = overlaps.prefix(3).map { candidate in
                [
                    subtypeName(candidate),
                    candidate.selectedForAuto ? "selected" : "not_selected",
                    "prio=\(candidate.priority)",
                    candidate.candidateLayer,
                    candidate.gateStatus,
                    "span=\(candidate.startISIIndex)-\(candidate.endISIIndex)",
                    "q90=\(formatSeconds(candidate.intraQ90Sec))",
                    "path=\(candidate.decisionPath.prefix(180))"
            ].joined(separator: ":")
        }.joined(separator: " || ")
        report.append(
            String(
                format: "Train %d ISI %d %.6f-%.6f isi=%.6f GT=Burst DET=%@ | overlaps=%d | %@",
                locale: Locale(identifier: "en_US_POSIX"),
                entry.0.trainNumber,
                entry.0.isiIndex,
                entry.0.startSec,
                entry.0.endSec,
                entry.0.isiSec,
                entry.1,
                overlaps.count,
                "\(overlapSummary) | local=\(localWindow)"
            )
        )
    }

    try report.joined(separator: "\n").write(
        to: URL(fileURLWithPath: "/tmp/stpd_simulator_diagnostic.txt"),
        atomically: true,
        encoding: .utf8
    )

    #expect(falseBurstRows.isEmpty)
    #expect(tonicAsBurstRows.isEmpty)
    #expect(missedBurstRows.isEmpty)
}

private func describeStateMismatch(
    _ row: SimulatorIntervalRow,
    _ annotation: ClassicAnchorEventAnnotation,
    _ candidate: ClassicAnchorCandidate?
) -> String {
    let path = candidate?.decisionPath.replacingOccurrences(of: "\n", with: " ") ?? "NA"
    return [
        String(format: "Train %d ISI %d %.6f-%.6f isi=%.6f GT=%@ DET=%@", locale: Locale(identifier: "en_US_POSIX"), row.trainNumber, row.isiIndex, row.startSec, row.endSec, row.isiSec, row.label, detectionLabel(for: annotation)),
        "candidate=\(annotation.candidateID)",
        "span=\(annotation.startISISecIndex)-\(annotation.endISISecIndex)",
        "layer=\(candidate?.candidateLayer ?? "NA")",
        "status=\(candidate?.selectionStatus ?? "NA")",
        "nISI=\(candidate?.nISI ?? -1)",
        "q50=\(formatSeconds(candidate?.intraQ50Sec))",
        "q90=\(formatSeconds(candidate?.intraQ90Sec))",
        "cv=\(formatSeconds(candidate?.cv))",
        "cv2=\(formatSeconds(candidate?.cv2))",
        "lv=\(formatSeconds(candidate?.lv))",
        "band=\(formatRange(candidate?.anchorBandLowerSec, candidate?.anchorBandUpperSec))",
        "path=\(path.prefix(220))"
    ].joined(separator: " | ")
}

private func parseIntervalRows(_ text: String) throws -> [SimulatorIntervalRow] {
    var nextIndexByTrain: [Int: Int] = [:]
    var rows: [SimulatorIntervalRow] = []
    for line in text.split(whereSeparator: \.isNewline).dropFirst() {
        let fields = line.split(separator: ",", omittingEmptySubsequences: false).map(String.init)
        guard fields.count >= 6,
              let trainNumber = Int(fields[0]),
              let start = Double(fields[1]),
              let end = Double(fields[2]),
              let isi = Double(fields[3]) else {
            continue
        }
        let index = nextIndexByTrain[trainNumber, default: 0] + 1
        nextIndexByTrain[trainNumber] = index
        rows.append(SimulatorIntervalRow(
            trainNumber: trainNumber,
            startSec: start,
            endSec: end,
            isiSec: isi,
            label: fields[5],
            isiIndex: index
        ))
    }
    return rows
}

private func detectionLabel(for annotation: ClassicAnchorEventAnnotation?) -> String {
    guard let annotation else {
        return "None"
    }
    switch annotation.visualLabel {
    case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
        return "Burst"
    case .tonic:
        return "Tonic"
    case .highFrequencyTonic:
        return "HFTonic"
    case .highFrequencySpiking:
        return "HFS"
    case .pause:
        return "Pause"
    case .reject:
        return "Reject"
    case .profile:
        return "Profile"
    }
}

private func subtypeName(_ candidate: ClassicAnchorCandidate) -> String {
    if candidate.auditRecommendedEventTrackClass.isEmpty == false {
        return candidate.auditRecommendedEventTrackClass
    }
    if candidate.auditRecommendedFinalClass.isEmpty == false {
        return candidate.auditRecommendedFinalClass
    }
    return candidate.finalLabel.rawValue
}

private func describeFalseBurst(
    _ row: SimulatorIntervalRow,
    _ annotation: ClassicAnchorEventAnnotation,
    _ candidate: ClassicAnchorCandidate?
) -> String {
    [
        String(format: "Train %d ISI %d %.6f-%.6f isi=%.6f GT=%@ DET=Burst", locale: Locale(identifier: "en_US_POSIX"), row.trainNumber, row.isiIndex, row.startSec, row.endSec, row.isiSec, row.label),
        "candidate=\(annotation.candidateID)",
        "subtype=\(annotation.displaySubtypeName)",
        "span=\(annotation.startISISecIndex)-\(annotation.endISISecIndex)",
        "layer=\(candidate?.candidateLayer ?? "NA")",
        "gate=\(candidate?.gateStatus ?? "NA")",
        "track=\(candidate?.auditRecommendedTrack.rawValue ?? "NA")",
        "q90=\(formatSeconds(candidate?.intraQ90Sec))",
        "max=\(formatSeconds(candidate?.maxIntraISISec))",
        "pre=\(formatSeconds(candidate?.preGapSec))",
        "post=\(formatSeconds(candidate?.postGapSec))",
        "edge=\(formatRatio(candidate?.edgeContrastMinQ90))",
        "selection=\(candidate?.selectionStatus ?? "NA")",
        "path=\(candidate?.decisionPath ?? "NA")"
    ].joined(separator: " | ")
}

private func formatSeconds(_ value: Double?) -> String {
    guard let value, value.isFinite else { return "NA" }
    return String(format: "%.6f", locale: Locale(identifier: "en_US_POSIX"), value)
}

private func formatRatio(_ value: Double?) -> String {
    guard let value, value.isFinite else { return "NA" }
    return String(format: "%.3f", locale: Locale(identifier: "en_US_POSIX"), value)
}

private func formatRange(_ lower: Double?, _ upper: Double?) -> String {
    "\(formatSeconds(lower))-\(formatSeconds(upper))"
}
