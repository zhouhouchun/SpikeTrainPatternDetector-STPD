import Foundation
import Testing
@testable import STPDCore

private enum BlindBenchmarkPattern: String, CaseIterable, Comparable {
    case burst
    case pause
    case tonic
    case highFrequencySpiking = "high_frequency_spiking"
    case other

    static func < (lhs: Self, rhs: Self) -> Bool {
        lhs.rawValue < rhs.rawValue
    }
}

private struct BlindBenchmarkIntervalKey: Hashable {
    let trainID: String
    let isiIndex: Int
}

private struct BlindBenchmarkTruthInterval {
    let classID: Int
    let key: BlindBenchmarkIntervalKey
    let pattern: BlindBenchmarkPattern
}

private struct BlindBenchmarkRun {
    let trainID: String
    let lower: Int
    let upper: Int

    var count: Int { upper - lower + 1 }

    func intersectionCount(with other: Self) -> Int {
        guard trainID == other.trainID else { return 0 }
        return max(0, min(upper, other.upper) - max(lower, other.lower) + 1)
    }

    func iou(with other: Self) -> Double {
        let intersection = intersectionCount(with: other)
        guard intersection > 0 else { return 0 }
        return Double(intersection) / Double(count + other.count - intersection)
    }
}

/// Optional owner benchmark. Detection consumes only the blinded timestamp table and runs each
/// class as a separate dataset. Ground truth is opened only after all three prediction maps have
/// been frozen, preventing class labels or pattern labels from influencing candidate generation.
@Test
func optionalThreeClassSimulatorBlindBenchmark() throws {
    guard ProcessInfo.processInfo.environment["STPD_RUN_SIMULATOR_BLIND_BENCHMARK"] == "1" else {
        return
    }
    let benchmarkRoot = URL(
        fileURLWithPath: ProcessInfo.processInfo.environment["STPD_SIMULATOR_BENCHMARK_DIR"]
            ?? "/Users/zark/Desktop/Reviewer/Simulator data",
        isDirectory: true
    )
    let inputURL = benchmarkRoot
        .appendingPathComponent("detector_inputs", isDirectory: true)
        .appendingPathComponent("spike_times_blinded.csv")
    let truthURL = benchmarkRoot
        .appendingPathComponent("ground_truth", isDirectory: true)
        .appendingPathComponent("interval_labels.csv")

    guard FileManager.default.fileExists(atPath: inputURL.path),
          FileManager.default.fileExists(atPath: truthURL.path) else {
        return
    }

    let inputRows = try readBlindTimestampRows(inputURL)
    let classIDs = Set(inputRows.map(\.classID))
    #expect(classIDs == Set([1, 2, 3]))

    // Freeze all predictions before the protected truth file is read.
    var frozenPredictionsByClass: [Int: [BlindBenchmarkIntervalKey: Set<BlindBenchmarkPattern>]] = [:]
    var possibleBurstSupportByClass: [Int: Int] = [:]
    for classID in classIDs.sorted() {
        let rows = inputRows.filter { $0.classID == classID }
        let grouped = Dictionary(grouping: rows, by: \.trainID)
        #expect(grouped.count == 20)
        let trains = grouped.keys.sorted().map { trainID in
            SpikeTrain(
                name: trainID,
                timestampsSec: grouped[trainID, default: []].map(\.timeSec),
                duplicateTimestampPolicy: .errorKeep
            )
        }
        #expect(trains.allSatisfy { $0.inputZeroOrNegativeStepCount == 0 })

        let dataset = SpikeDataset(
            name: "Simulator Class \(classID) blind",
            sourceDescription: inputURL.path,
            trains: trains
        )
        let run = ClassicAnchorDetectionPipeline.run(dataset: dataset)
        var predictions: [BlindBenchmarkIntervalKey: Set<BlindBenchmarkPattern>] = [:]
        var possibleBurstSupport = Set<BlindBenchmarkIntervalKey>()
        let spikeCounts = Dictionary(uniqueKeysWithValues: trains.map { ($0.id, $0.spikeCount) })

        for candidate in run.candidates where candidate.selectedForAuto && candidate.isEligibleForAutoSelection {
            let lower = max(1, min(candidate.startISIIndex, candidate.endISIIndex))
            let upper = min(
                max(candidate.startISIIndex, candidate.endISIIndex),
                max(0, (spikeCounts[candidate.trainID] ?? 0) - 1)
            )
            guard lower <= upper else { continue }

            if candidate.finalLabel == .possibleBurst {
                for isiIndex in lower...upper {
                    possibleBurstSupport.insert(
                        BlindBenchmarkIntervalKey(trainID: candidate.trainID, isiIndex: isiIndex)
                    )
                }
                continue
            }
            guard let pattern = benchmarkPattern(for: candidate.finalLabel) else { continue }
            for isiIndex in lower...upper {
                let key = BlindBenchmarkIntervalKey(trainID: candidate.trainID, isiIndex: isiIndex)
                predictions[key, default: []].insert(pattern)
            }
        }
        frozenPredictionsByClass[classID] = predictions
        possibleBurstSupportByClass[classID] = possibleBurstSupport.count
    }

    let truth = try readBlindBenchmarkTruth(truthURL)
    #expect(Set(truth.map(\.classID)) == classIDs)

    print("BLIND_BENCHMARK_BEGIN")
    for classID in classIDs.sorted() {
        let classTruth = truth.filter { $0.classID == classID }
        let predictions = frozenPredictionsByClass[classID, default: [:]]
        let truthByKey = Dictionary(uniqueKeysWithValues: classTruth.map { ($0.key, $0.pattern) })
        let allKeys = Set(truthByKey.keys)
        var exactCorrect = 0
        var multilabelCount = 0

        for key in allKeys {
            let predicted = predictions[key, default: []]
            if predicted.count > 1 { multilabelCount += 1 }
            if primaryPattern(from: predicted) == truthByKey[key] {
                exactCorrect += 1
            }
        }

        let accuracy = allKeys.isEmpty ? 0 : Double(exactCorrect) / Double(allKeys.count)
        print(
            "CLASS,\(classID),trains,20,intervals,\(allKeys.count),accuracy,\(decimal(accuracy))," +
                "multilabel_intervals,\(multilabelCount),possible_burst_support,\(possibleBurstSupportByClass[classID, default: 0])"
        )

        let predictedHFSEnvelopeKeys = allKeys.filter {
            predictions[$0, default: []].contains(.highFrequencySpiking)
        }
        let hfsTruthComposition = Dictionary(grouping: predictedHFSEnvelopeKeys) {
            truthByKey[$0] ?? .other
        }
        print(
            "HFS_TRUTH_COMPOSITION,\(classID)," +
                BlindBenchmarkPattern.allCases.map { pattern in
                    "\(pattern.rawValue),\(hfsTruthComposition[pattern, default: []].count)"
                }.joined(separator: ",")
        )

        for pattern in BlindBenchmarkPattern.allCases {
            let truthKeys = Set(classTruth.lazy.filter { $0.pattern == pattern }.map(\.key))
            let predictedKeys: Set<BlindBenchmarkIntervalKey>
            if pattern == .other {
                predictedKeys = Set(allKeys.filter {
                    primaryPattern(from: predictions[$0, default: []]) == .other
                })
            } else {
                predictedKeys = Set(allKeys.filter {
                    predictions[$0, default: []].contains(pattern)
                })
            }
            let truePositive = truthKeys.intersection(predictedKeys).count
            let precision = ratio(truePositive, predictedKeys.count)
            let recall = ratio(truePositive, truthKeys.count)
            let f1 = precision + recall > 0 ? 2 * precision * recall / (precision + recall) : 0
            let episode = episodeMetrics(
                truthKeys: truthKeys,
                predictedKeys: predictedKeys
            )
            print(
                "PATTERN,\(classID),\(pattern.rawValue),truth,\(truthKeys.count),predicted,\(predictedKeys.count)," +
                    "tp,\(truePositive),precision,\(decimal(precision)),recall,\(decimal(recall)),f1,\(decimal(f1))," +
                    "episodes,\(episode.truthCount),iou50_recall,\(decimal(episode.iou50Recall))," +
                    "mean_best_iou,\(decimal(episode.meanBestIoU)),mean_fragmentation,\(decimal(episode.meanFragmentation))"
            )
        }
    }
    print("BLIND_BENCHMARK_END")
}

private struct BlindTimestampRow {
    let classID: Int
    let trainID: String
    let timeSec: Double
}

private func readBlindTimestampRows(_ url: URL) throws -> [BlindTimestampRow] {
    let rows = try readCSV(url)
    guard let header = rows.first else { return [] }
    let indices = Dictionary(uniqueKeysWithValues: header.enumerated().map { ($0.element, $0.offset) })
    let classIndex = try #require(indices["Class"])
    let trainIndex = try #require(indices["Train_ID"])
    let timeIndex = try #require(indices["Time_s"])
    return try rows.dropFirst().map { row in
        BlindTimestampRow(
            classID: try #require(Int(row[classIndex])),
            trainID: row[trainIndex],
            timeSec: try #require(Double(row[timeIndex]))
        )
    }
}

private func readBlindBenchmarkTruth(_ url: URL) throws -> [BlindBenchmarkTruthInterval] {
    let rows = try readCSV(url)
    guard let header = rows.first else { return [] }
    let indices = Dictionary(uniqueKeysWithValues: header.enumerated().map { ($0.element, $0.offset) })
    let classIndex = try #require(indices["Class"])
    let trainIndex = try #require(indices["Train_ID"])
    let intervalIndex = try #require(indices["Interval_ID"])
    let patternIndex = try #require(indices["Pattern"])
    return try rows.dropFirst().map { row in
        BlindBenchmarkTruthInterval(
            classID: try #require(Int(row[classIndex])),
            key: BlindBenchmarkIntervalKey(
                trainID: row[trainIndex],
                isiIndex: try #require(Int(row[intervalIndex]))
            ),
            pattern: try #require(BlindBenchmarkPattern(rawValue: row[patternIndex]))
        )
    }
}

private func readCSV(_ url: URL) throws -> [[String]] {
    let contents = try String(contentsOf: url, encoding: .utf8)
    return contents.split(whereSeparator: \.isNewline).map { line in
        var fields: [String] = []
        var field = ""
        var quoted = false
        var index = line.startIndex
        while index < line.endIndex {
            let character = line[index]
            if character == "\"" {
                let next = line.index(after: index)
                if quoted, next < line.endIndex, line[next] == "\"" {
                    field.append("\"")
                    index = line.index(after: next)
                    continue
                }
                quoted.toggle()
            } else if character == ",", !quoted {
                fields.append(field)
                field = ""
            } else {
                field.append(character)
            }
            index = line.index(after: index)
        }
        fields.append(field)
        return fields
    }
}

private func benchmarkPattern(for label: ClassicAnchorLabel) -> BlindBenchmarkPattern? {
    switch label {
    case .burst, .highFrequencyBurst, .longBurst:
        return .burst
    case .pause:
        return .pause
    case .tonic, .highFrequencyTonic:
        return .tonic
    case .highFrequencySpiking:
        return .highFrequencySpiking
    case .possibleBurst, .reject, .profile:
        return nil
    }
}

private func primaryPattern(from patterns: Set<BlindBenchmarkPattern>) -> BlindBenchmarkPattern {
    if patterns.contains(.pause) { return .pause }
    if patterns.contains(.burst) { return .burst }
    if patterns.contains(.highFrequencySpiking) { return .highFrequencySpiking }
    if patterns.contains(.tonic) { return .tonic }
    return .other
}

private func runs(_ keys: Set<BlindBenchmarkIntervalKey>) -> [BlindBenchmarkRun] {
    var allRuns: [BlindBenchmarkRun] = []
    for (trainID, trainKeys) in Dictionary(grouping: keys, by: \.trainID) {
        let indices = trainKeys.map(\.isiIndex).sorted()
        guard var lower = indices.first else { continue }
        var upper = lower
        var result: [BlindBenchmarkRun] = []
        for index in indices.dropFirst() {
            if index == upper + 1 {
                upper = index
            } else {
                result.append(BlindBenchmarkRun(trainID: trainID, lower: lower, upper: upper))
                lower = index
                upper = index
            }
        }
        result.append(BlindBenchmarkRun(trainID: trainID, lower: lower, upper: upper))
        allRuns.append(contentsOf: result)
    }
    return allRuns
}

private func episodeMetrics(
    truthKeys: Set<BlindBenchmarkIntervalKey>,
    predictedKeys: Set<BlindBenchmarkIntervalKey>
) -> (truthCount: Int, iou50Recall: Double, meanBestIoU: Double, meanFragmentation: Double) {
    let truthRuns = runs(truthKeys)
    let predictedRuns = runs(predictedKeys)
    guard !truthRuns.isEmpty else { return (0, 0, 0, 0) }
    var detected = 0
    var iouSum = 0.0
    var fragmentationSum = 0.0
    for truth in truthRuns {
        let overlaps = predictedRuns.filter { truth.intersectionCount(with: $0) > 0 }
        let bestIoU = overlaps.map { truth.iou(with: $0) }.max() ?? 0
        if bestIoU >= 0.5 { detected += 1 }
        iouSum += bestIoU
        fragmentationSum += Double(overlaps.count)
    }
    return (
        truthRuns.count,
        Double(detected) / Double(truthRuns.count),
        iouSum / Double(truthRuns.count),
        fragmentationSum / Double(truthRuns.count)
    )
}

private func ratio(_ numerator: Int, _ denominator: Int) -> Double {
    denominator == 0 ? 0 : Double(numerator) / Double(denominator)
}

private func decimal(_ value: Double) -> String {
    String(format: "%.4f", value)
}
