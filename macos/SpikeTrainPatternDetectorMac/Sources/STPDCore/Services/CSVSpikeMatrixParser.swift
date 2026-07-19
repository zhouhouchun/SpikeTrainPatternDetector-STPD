import Foundation

private func csvDiagnosticLiteral(_ value: String) -> String {
    String(reflecting: value)
}

public enum CSVSpikeMatrixParserError: Error, LocalizedError, Sendable {
    case emptyFile
    case noSpikeTrainColumns
    case noNumericSpikes
    case derivedOutputLikely(String)
    case malformedCSV(
        line: Int,
        column: String?,
        columnIndex: Int?,
        token: String?,
        reason: String
    )
    case malformedCell(column: String, columnIndex: Int, line: Int, token: String)

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
        case .malformedCSV(let line, let column, let columnIndex, let token, let reason):
            var location = "line \(line)"
            if let columnIndex {
                location += ", column #\(columnIndex)"
            }
            if let column {
                location += " (\(csvDiagnosticLiteral(column)))"
            }
            let tokenContext = token.map { " near \(csvDiagnosticLiteral($0))" } ?? ""
            return "Malformed CSV syntax at \(location)\(tokenContext): \(reason)"
        case .malformedCell(let column, let columnIndex, let line, let token):
            return "Malformed value \(csvDiagnosticLiteral(token)) in column \(csvDiagnosticLiteral(column)) "
                + "(#\(columnIndex)) at line \(line). "
                + "Non-empty spike cells must be finite timestamps, except trailing NA/NaN/null padding. "
                + "Replace the token with the intended timestamp or correct the source column. Do not delete "
                + "or blank an internal value: skipping it could merge surrounding spikes into an artificially "
                + "long interval."
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
        var numberedRows: [(line: Int, cells: [String])] = []
        var diagnosticHeaders: [String]?
        for (offset, physicalLine) in contents
            .split(omittingEmptySubsequences: false, whereSeparator: \.isNewline)
            .enumerated() {
            guard !physicalLine.isEmpty else { continue }
            let lineNumber = offset + 1
            var lexicalLine = physicalLine
            if offset == 0, lexicalLine.first == "\u{FEFF}" {
                lexicalLine = lexicalLine.dropFirst()
            }
            do {
                let cells = try parseCSVLine(lexicalLine, lineNumber: lineNumber)
                numberedRows.append((line: lineNumber, cells: cells))
                if hasHeader, diagnosticHeaders == nil {
                    diagnosticHeaders = cells.enumerated().map { index, value in
                        cleanedHeader(value, fallbackIndex: index)
                    }
                }
            } catch let error as CSVSpikeMatrixParserError {
                throw enrichedCSVError(
                    error,
                    headers: diagnosticHeaders,
                    hasHeader: hasHeader
                )
            }
        }
        let rows = numberedRows.map { $0.cells }
        let rowLineNumbers = numberedRows.map { $0.line }

        guard let firstRow = rows.first, !firstRow.isEmpty else {
            throw CSVSpikeMatrixParserError.emptyFile
        }

        if !allowDerivedCSV {
            try rejectLikelyDerivedCSV(firstRow: firstRow, sourceDescription: sourceDescription)
        }

        let trainNames: [String]
        let dataRows: ArraySlice<[String]>
        let dataLineNumbers: ArraySlice<Int>
        if hasHeader {
            trainNames = firstRow.enumerated().map { index, value in
                cleanedHeader(value, fallbackIndex: index)
            }
            dataRows = rows.dropFirst()
            dataLineNumbers = rowLineNumbers.dropFirst()
        } else {
            let columnCount = max(1, rows.map(\.count).max() ?? firstRow.count)
            trainNames = (0..<columnCount).map { "Train \($0 + 1)" }
            dataRows = rows[...]
            dataLineNumbers = rowLineNumbers[...]
        }

        guard !trainNames.isEmpty else {
            throw CSVSpikeMatrixParserError.noSpikeTrainColumns
        }

        let scale = unit.scaleToSeconds
        let dataRowArray = Array(dataRows)
        let dataLineArray = Array(dataLineNumbers)

        if hasHeader,
           let overWidthRow = dataRowArray.indices.first(where: {
               dataRowArray[$0].count > trainNames.count
           }) {
            let extraColumnIndex = trainNames.count + 1
            throw CSVSpikeMatrixParserError.malformedCSV(
                line: dataLineArray[overWidthRow],
                column: "Column \(extraColumnIndex)",
                columnIndex: extraColumnIndex,
                token: dataRowArray[overWidthRow][extraColumnIndex - 1],
                reason: "row contains \(dataRowArray[overWidthRow].count) fields, but the header defines "
                    + "only \(trainNames.count) columns"
            )
        }

        // Match R `stpd_is_task_event_column`: task events are an annotation layer,
        // never spike trains and never detector input.
        let isEventColumn = trainNames.map(isTaskEventColumn)

        var columns = Array(repeating: [Double](), count: trainNames.count)
        for columnIndex in trainNames.indices where !isEventColumn[columnIndex] {
            let rawCell: (Int) -> String = { rowIndex in
                let row = dataRowArray[rowIndex]
                return columnIndex < row.count
                    ? row[columnIndex].trimmingCharacters(in: .whitespacesAndNewlines)
                    : ""
            }
            // Last row holding a finite value. Only after this row may explicit
            // missing-value padding tokens (NA/NaN/null) be skipped.
            let lastValueRow = dataRowArray.indices.last { rowIndex in
                let raw = rawCell(rowIndex)
                guard !raw.isEmpty, let parsed = Double(raw) else { return false }
                return parsed.isFinite
            } ?? -1

            if lastValueRow < 0 {
                // No finite values in this column. An all-empty column is treated as
                // "no spikes" (dropped below). A column that has no real timestamp but
                // does contain a non-empty token is corrupted content, not padding.
                if let offendingRow = dataRowArray.indices.first(where: {
                    !rawCell($0).isEmpty
                }) {
                    throw CSVSpikeMatrixParserError.malformedCell(
                        column: trainNames[columnIndex],
                        columnIndex: columnIndex + 1,
                        line: dataLineArray[offendingRow],
                        token: rawCell(offendingRow)
                    )
                }
                columns[columnIndex] = []
                continue
            }

            var values: [Double] = []
            for rowIndex in dataRowArray.indices {
                let raw = rawCell(rowIndex)
                if !raw.isEmpty, let value = Double(raw), value.isFinite {
                    values.append(value * scale)
                    continue
                }
                if raw.isEmpty {
                    // Empty cells (internal or trailing) remain allowed: a wide spike
                    // matrix encodes "no spike at this row" as a blank cell.
                    continue
                }
                // A non-empty, non-finite token is allowed only as trailing
                // missing-value padding (NA/NaN/null) after the last real timestamp.
                // Internal tokens would fabricate a long ISI; trailing garbage or
                // Inf/-Inf is corrupted content. All other cases are malformed.
                if rowIndex > lastValueRow, Self.isMissingValuePaddingToken(raw) {
                    continue
                }
                throw CSVSpikeMatrixParserError.malformedCell(
                    column: trainNames[columnIndex],
                    columnIndex: columnIndex + 1,
                    line: dataLineArray[rowIndex],
                    token: raw
                )
            }
            columns[columnIndex] = values
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

    /// Explicit missing-value padding tokens permitted only as trailing padding
    /// after the last finite value in a column. Infinity is intentionally excluded:
    /// `Inf`/`-Inf` are treated as malformed, non-finite content.
    private static func isMissingValuePaddingToken(_ raw: String) -> Bool {
        switch raw.lowercased() {
        case "na", "nan", "null":
            return true
        default:
            return false
        }
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

    private static func enrichedCSVError(
        _ error: CSVSpikeMatrixParserError,
        headers: [String]?,
        hasHeader: Bool
    ) -> CSVSpikeMatrixParserError {
        guard case .malformedCSV(
            let line,
            let existingColumn,
            let columnIndex,
            let token,
            let reason
        ) = error else {
            return error
        }
        let inferredColumn = columnIndex.map { physicalIndex -> String in
            if let headers, headers.indices.contains(physicalIndex - 1) {
                return headers[physicalIndex - 1]
            }
            return hasHeader ? "Column \(physicalIndex)" : "Train \(physicalIndex)"
        }
        return .malformedCSV(
            line: line,
            column: existingColumn ?? inferredColumn,
            columnIndex: columnIndex,
            token: token,
            reason: reason
        )
    }

    private static func rawCSVField(
        in line: Substring,
        from fieldStart: Substring.Index,
        searchDelimiterFrom searchStart: Substring.Index
    ) -> String {
        var end = searchStart
        while end < line.endIndex, line[end] != "," {
            end = line.index(after: end)
        }
        return String(line[fieldStart..<end])
    }

    static func parseCSVLine(_ line: Substring, lineNumber: Int) throws -> [String] {
        var fields: [String] = []
        var field = ""
        var index = line.startIndex

        while true {
            let fieldStart = index
            let physicalColumnIndex = fields.count + 1
            if index < line.endIndex, line[index] == "\"" {
                index = line.index(after: index)
                var closed = false
                while index < line.endIndex {
                    let character = line[index]
                    if character == "\"" {
                        let next = line.index(after: index)
                        if next < line.endIndex, line[next] == "\"" {
                            field.append("\"")
                            index = line.index(after: next)
                            continue
                        }
                        index = next
                        closed = true
                        break
                    }
                    field.append(character)
                    index = line.index(after: index)
                }
                guard closed else {
                    throw CSVSpikeMatrixParserError.malformedCSV(
                        line: lineNumber,
                        column: nil,
                        columnIndex: physicalColumnIndex,
                        token: String(line[fieldStart..<line.endIndex]),
                        reason: "unterminated quoted field"
                    )
                }
                guard index == line.endIndex || line[index] == "," else {
                    throw CSVSpikeMatrixParserError.malformedCSV(
                        line: lineNumber,
                        column: nil,
                        columnIndex: physicalColumnIndex,
                        token: rawCSVField(
                            in: line,
                            from: fieldStart,
                            searchDelimiterFrom: index
                        ),
                        reason: "unexpected character after a closing quote"
                    )
                }
            } else {
                while index < line.endIndex, line[index] != "," {
                    guard line[index] != "\"" else {
                        throw CSVSpikeMatrixParserError.malformedCSV(
                            line: lineNumber,
                            column: nil,
                            columnIndex: physicalColumnIndex,
                            token: rawCSVField(
                                in: line,
                                from: fieldStart,
                                searchDelimiterFrom: index
                            ),
                            reason: "quote must begin a CSV field"
                        )
                    }
                    field.append(line[index])
                    index = line.index(after: index)
                }
            }

            fields.append(field)
            field.removeAll(keepingCapacity: true)
            guard index < line.endIndex else { break }
            index = line.index(after: index)
            if index == line.endIndex {
                fields.append("")
                break
            }
        }
        return fields
    }
}
