import Foundation

public enum CSVSpikeMatrixParserError: Error, LocalizedError, Sendable {
    case emptyFile
    case noSpikeTrainColumns
    case noNumericSpikes
    case derivedOutputLikely(String)

    public var errorDescription: String? {
        switch self {
        case .emptyFile:
            return "The CSV file is empty."
        case .noSpikeTrainColumns:
            return "The CSV file has no spike train columns."
        case .noNumericSpikes:
            return "No numeric spike timestamps were found."
        case .derivedOutputLikely(let reason):
            return "This CSV looks like a derived analysis table, not raw spike timestamps. \(reason)"
        }
    }
}

public enum CSVSpikeMatrixParser {
    public static func parse(
        contents: String,
        datasetName: String,
        sourceDescription: String,
        unit: SpikeTimeUnit = .seconds,
        hasHeader: Bool = true,
        duplicatePolicy: DuplicateTimestampPolicy = .errorKeep,
        allowDerivedCSV: Bool = false
    ) throws -> SpikeDataset {
        let rows = contents
            .split(whereSeparator: \.isNewline)
            .map(parseCSVLine)

        guard let firstRow = rows.first, !firstRow.isEmpty else {
            throw CSVSpikeMatrixParserError.emptyFile
        }

        if !allowDerivedCSV {
            try rejectLikelyDerivedCSV(firstRow: firstRow, sourceDescription: sourceDescription)
        }

        let trainNames: [String]
        let dataRows: ArraySlice<[String]>
        if hasHeader {
            trainNames = firstRow.enumerated().map { index, value in
                cleanedHeader(value, fallbackIndex: index)
            }
            dataRows = rows.dropFirst()
        } else {
            let columnCount = max(1, rows.map(\.count).max() ?? firstRow.count)
            trainNames = (0..<columnCount).map { "Train \($0 + 1)" }
            dataRows = rows[...]
        }

        guard !trainNames.isEmpty else {
            throw CSVSpikeMatrixParserError.noSpikeTrainColumns
        }

        var columns = Array(repeating: [Double](), count: trainNames.count)
        let scale = unit.scaleToSeconds

        for row in dataRows {
            for columnIndex in trainNames.indices {
                guard columnIndex < row.count else {
                    continue
                }
                let value = row[columnIndex].trimmingCharacters(in: .whitespacesAndNewlines)
                guard !value.isEmpty else {
                    continue
                }
                let lowered = value.lowercased()
                guard lowered != "na", lowered != "nan", lowered != "null" else {
                    continue
                }
                guard let parsed = Double(value), parsed.isFinite else {
                    continue
                }
                columns[columnIndex].append(parsed * scale)
            }
        }

        let trains = trainNames.indices.compactMap { index -> SpikeTrain? in
            guard !columns[index].isEmpty else {
                return nil
            }
            return SpikeTrain(
                name: trainNames[index],
                timestampsSec: columns[index],
                duplicateTimestampPolicy: duplicatePolicy
            )
        }

        guard !trains.isEmpty else {
            throw CSVSpikeMatrixParserError.noNumericSpikes
        }

        return SpikeDataset(
            name: datasetName,
            sourceDescription: sourceDescription,
            trains: trains
        )
    }

    private static func cleanedHeader(_ value: String, fallbackIndex: Int) -> String {
        let trimmed = value.trimmingCharacters(in: .whitespacesAndNewlines)
        return trimmed.isEmpty ? "Train \(fallbackIndex + 1)" : trimmed
    }

    private static func rejectLikelyDerivedCSV(firstRow: [String], sourceDescription: String) throws {
        let fileName = URL(fileURLWithPath: sourceDescription).lastPathComponent.lowercased()
        let highConfidenceDerivedNames = [
            "sliding", "isi_base", "tonic_summary", "threshold", "thresholds",
            "candidate", "eventness", "audit", "output", "results", "metrics",
            "features", "parameters", "params", "qc", "validation", "near_miss"
        ]

        if highConfidenceDerivedNames.contains(where: { token in fileName.contains(token) }) {
            throw CSVSpikeMatrixParserError.derivedOutputLikely(
                "The filename contains '\(fileName)', which matches known derived-output naming patterns."
            )
        }

        let normalizedHeaders = firstRow.map { value in
            value
                .lowercased()
                .replacingOccurrences(of: #"[^a-z0-9]+"#, with: "_", options: .regularExpression)
                .trimmingCharacters(in: CharacterSet(charactersIn: "_"))
        }

        let derivedHeaderPatterns = [
            "spike_train", "fragment", "fragment_number", "start", "start_time",
            "end", "end_time", "max_isi", "min_isi", "median_isi", "mean_isi",
            "threshold", "candidate", "event", "score", "decision", "status",
            "pattern", "label", "final_label", "source", "family", "summary",
            "metric"
        ]

        let hitCount = normalizedHeaders.filter { header in
            derivedHeaderPatterns.contains { pattern in
                header == pattern || header.hasPrefix("\(pattern)_")
            }
        }.count

        if hitCount >= 3 || (hitCount >= 2 && normalizedHeaders.contains("spike_train")) {
            throw CSVSpikeMatrixParserError.derivedOutputLikely(
                "Headers such as Spike_train, Start/End, threshold, candidate, score, or metric usually indicate a derived table."
            )
        }
    }

    static func parseCSVLine(_ line: Substring) -> [String] {
        var fields: [String] = []
        var field = ""
        var inQuotes = false
        var iterator = line.makeIterator()

        while let character = iterator.next() {
            if character == "\"" {
                if inQuotes, let next = iterator.next() {
                    if next == "\"" {
                        field.append("\"")
                    } else {
                        inQuotes = false
                        if next == "," {
                            fields.append(field)
                            field.removeAll(keepingCapacity: true)
                        } else {
                            field.append(next)
                        }
                    }
                } else {
                    inQuotes.toggle()
                }
            } else if character == ",", !inQuotes {
                fields.append(field)
                field.removeAll(keepingCapacity: true)
            } else {
                field.append(character)
            }
        }

        fields.append(field)
        return fields
    }
}
