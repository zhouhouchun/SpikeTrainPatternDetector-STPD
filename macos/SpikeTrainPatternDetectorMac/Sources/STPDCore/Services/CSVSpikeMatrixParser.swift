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

        let scale = unit.scaleToSeconds
        let dataRowArray = Array(dataRows)
        // Match R `stpd_is_task_event_column`: task events are an annotation layer,
        // never spike trains and never detector input.
        let isEventColumn = trainNames.map(isTaskEventColumn)
        var columns = Array(repeating: [Double](), count: trainNames.count)

        for row in dataRowArray {
            for columnIndex in trainNames.indices where !isEventColumn[columnIndex] {
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
            guard !isEventColumn[index], !columns[index].isEmpty else {
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

        let taskEvents = extractTaskEvents(
            trainNames: trainNames,
            isEventColumn: isEventColumn,
            dataRows: dataRowArray,
            unit: unit,
            source: sourceDescription
        )

        return SpikeDataset(
            name: datasetName,
            sourceDescription: sourceDescription,
            trains: trains,
            taskEvents: taskEvents
        )
    }

    // MARK: - Task events (R `R/03_data_io.R` parity)

    /// CSV files exported by spreadsheet tools may prefix the first header with a UTF-8 BOM.
    /// Treat it as transport metadata, not as part of the scientific column name.
    private static func trimmedHeader(_ value: String) -> String {
        value.trimmingCharacters(
            in: .whitespacesAndNewlines.union(CharacterSet(charactersIn: "\u{FEFF}"))
        )
    }

    /// R `stpd_is_task_event_column`: trimmed, case-insensitive `^event($|[_ .:-])`.
    public static func isTaskEventColumn(_ name: String) -> Bool {
        let trimmed = trimmedHeader(name).lowercased()
        guard trimmed.hasPrefix("event") else { return false }
        if trimmed.count == 5 { return true }
        let separators: Set<Character> = ["_", " ", ".", ":", "-"]
        let next = trimmed[trimmed.index(trimmed.startIndex, offsetBy: 5)]
        return separators.contains(next)
    }

    /// R `stpd_clean_task_event_name`: remove the `event` prefix and separators,
    /// replace underscores with spaces, collapse whitespace, and use "Event" when empty.
    public static func cleanTaskEventName(_ name: String) -> String {
        var stripped = Substring(trimmedHeader(name))
        if stripped.lowercased().hasPrefix("event") {
            stripped = stripped.dropFirst(5)
            let separators: Set<Character> = ["_", " ", ".", ":", "-"]
            while let first = stripped.first, separators.contains(first) {
                stripped = stripped.dropFirst()
            }
        }
        let spaced = String(stripped).replacingOccurrences(of: "_", with: " ")
        let collapsed = spaced.split(whereSeparator: \.isWhitespace).joined(separator: " ")
        return collapsed.isEmpty ? "Event" : collapsed
    }

    /// R `stpd_extract_task_events_from_data_frame` followed by
    /// `stpd_normalize_task_events`: finite cells become sorted, uniquely identified
    /// external task events. Blank and non-finite event cells are ignored.
    private static func extractTaskEvents(
        trainNames: [String],
        isEventColumn: [Bool],
        dataRows: [[String]],
        unit: SpikeTimeUnit,
        source: String
    ) -> [TaskEvent] {
        struct RawEvent {
            let id: String
            let name: String
            let column: String
            let timeSec: Double
            let eventIndex: Int
            let trialID: String
            let sourceOrder: Int
        }

        var raw: [RawEvent] = []
        // Exact duplicate CSV headers are legal with `check.names = FALSE` in R,
        // but the raw name alone cannot identify their physical source column.
        // Preserve the first spelling and add deterministic `_N` suffixes thereafter.
        let uniqueColumnLabels = makeUnique(trainNames)
        for columnIndex in trainNames.indices where isEventColumn[columnIndex] {
            let rawColumn = trainNames[columnIndex]
            let column = uniqueColumnLabels[columnIndex]
            let name = cleanTaskEventName(rawColumn)
            let slug = alphanumericSlug(name)
            var eventNumber = 0
            for (rowIndex, row) in dataRows.enumerated() {
                let cell = columnIndex < row.count
                    ? row[columnIndex].trimmingCharacters(in: .whitespacesAndNewlines)
                    : ""
                guard !cell.isEmpty, let value = Double(cell), value.isFinite else { continue }
                eventNumber += 1
                // Match R `to_sec`: milliseconds are divided by 1000. Using the
                // algebraically equivalent `* 0.001` changes IEEE-754 rounding at
                // six-decimal ID boundaries (for example 9.0005 ms).
                let timeSec = taskEventTimeInSeconds(value, unit: unit)
                raw.append(RawEvent(
                    id: "event_\(eventNumber)_\(slug)_\(fixedSixDecimalString(timeSec))",
                    name: name,
                    column: column,
                    timeSec: timeSec,
                    eventIndex: rowIndex + 1,
                    trialID: "\(slug)_\(eventNumber)",
                    sourceOrder: raw.count
                ))
            }
        }
        guard !raw.isEmpty else { return [] }

        let sorted = raw.sorted {
            if $0.timeSec != $1.timeSec { return $0.timeSec < $1.timeSec }
            if $0.name != $1.name { return $0.name < $1.name }
            if $0.eventIndex != $1.eventIndex { return $0.eventIndex < $1.eventIndex }
            return $0.sourceOrder < $1.sourceOrder
        }
        let uniqueIDs = makeUnique(sorted.map(\.id))
        let uniqueTrialIDs = makeUnique(sorted.map(\.trialID))
        return sorted.indices.map { index in
            let event = sorted[index]
            return TaskEvent(
                id: uniqueIDs[index],
                name: event.name,
                timeSec: event.timeSec,
                column: event.column,
                eventIndex: event.eventIndex,
                trialID: uniqueTrialIDs[index],
                source: source
            )
        }
    }

    private static func taskEventTimeInSeconds(_ value: Double, unit: SpikeTimeUnit) -> Double {
        switch unit {
        case .seconds:
            return value
        case .milliseconds:
            return value / 1_000.0
        }
    }

    /// R `gsub("[^A-Za-z0-9]+", "_", x)`.
    private static func alphanumericSlug(_ name: String) -> String {
        let allowed = Set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        var result = ""
        var lastWasSeparator = false
        for character in name {
            if allowed.contains(character) {
                result.append(character)
                lastWasSeparator = false
            } else if !lastWasSeparator {
                result.append("_")
                lastWasSeparator = true
            }
        }
        return result
    }

    private static func fixedSixDecimalString(_ value: Double) -> String {
        String(format: "%.6f", locale: Locale(identifier: "en_US_POSIX"), value)
    }

    /// Mirror R `make.unique(..., sep = "_")`, including suffixes already present
    /// in the original vector (`a, a, a_1` becomes `a, a_2, a_1`).
    private static func makeUnique(_ values: [String]) -> [String] {
        let reserved = Set(values)
        var used = Set<String>()
        var result: [String] = []
        result.reserveCapacity(values.count)

        for value in values {
            if used.insert(value).inserted {
                result.append(value)
                continue
            }
            var suffix = 1
            var candidate = "\(value)_\(suffix)"
            while used.contains(candidate) || reserved.contains(candidate) {
                suffix += 1
                candidate = "\(value)_\(suffix)"
            }
            used.insert(candidate)
            result.append(candidate)
        }
        return result
    }

    private static func cleanedHeader(_ value: String, fallbackIndex: Int) -> String {
        let trimmed = trimmedHeader(value)
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
            trimmedHeader(value)
                .lowercased()
                .replacingOccurrences(of: #"[^a-z0-9]+"#, with: "_", options: .regularExpression)
                .trimmingCharacters(in: CharacterSet(charactersIn: "_"))
        }

        let derivedHeaderPatterns = [
            "spike_train", "fragment", "fragment_number", "start", "start_time",
            "end", "end_time", "max_isi", "min_isi", "median_isi", "mean_isi",
            "threshold", "candidate", "event", "score", "decision", "status",
            "pattern", "label", "final_label", "source", "family", "summary",
            "metric", "trial_id"
        ]

        let validEventFlags = firstRow.map(isTaskEventColumn)
        if let invalidEventLike = zip(firstRow, validEventFlags).first(where: { original, valid in
            !valid && trimmedHeader(original).lowercased().hasPrefix("event")
        })?.0 {
            throw CSVSpikeMatrixParserError.derivedOutputLikely(
                "Header '\(invalidEventLike)' begins with the reserved event prefix but does not match ^event($|[_ .:-])."
            )
        }

        // Valid R-compatible event columns are external task annotations and do
        // not count by themselves. A valid event column paired with any other
        // derived-analysis header is nevertheless a derived event table, not a
        // raw spike matrix, and is rejected below.
        let hitCount = zip(firstRow, normalizedHeaders).filter { original, header in
            guard !isTaskEventColumn(original) else { return false }
            return derivedHeaderPatterns.contains { pattern in
                header == pattern || header.hasPrefix("\(pattern)_")
            }
        }.count

        let hasValidEventColumn = validEventFlags.contains(true)
        if hitCount >= 3 ||
            (hitCount >= 2 && normalizedHeaders.contains("spike_train")) ||
            (hasValidEventColumn && hitCount >= 1) {
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
