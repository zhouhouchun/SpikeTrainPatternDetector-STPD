import Foundation
import STPDCore
import ZIPFoundation

public enum ManualISIResultXLSXExporterError: Error, Sendable, LocalizedError {
    case archiveCreationFailed
    case missingTrainIDColumn

    public var errorDescription: String? {
        switch self {
        case .archiveCreationFailed:
            return "无法创建结果归档。"
        case .missingTrainIDColumn:
            return "逐 ISI 结果缺少 train_id 列，无法按 spike train 分页。"
        }
    }
}

/// Writes one worksheet per spike train. The table columns and row values are unchanged; only the
/// physical workbook partition changes. Cells remain text so long integer microsecond timestamps
/// and identity digests are never rounded by spreadsheet floating-point conversion.
public enum ManualISIResultXLSXExporter {
    public static func data(table: ManualISIExportTable) throws -> Data {
        let partitions = try ManualISIResultTablePartitioner.partitions(table: table)
        let sheetNames = ManualISIResultTablePartitioner.uniqueWorksheetNames(
            partitions.map(\.displayName)
        )
        var entries: [(String, Data)] = [
            ("[Content_Types].xml", Data(contentTypes(sheetCount: partitions.count).utf8)),
            ("_rels/.rels", Data(rootRelationships.utf8)),
            ("xl/workbook.xml", Data(workbook(sheetNames: sheetNames).utf8)),
            ("xl/_rels/workbook.xml.rels", Data(workbookRelationships(sheetCount: partitions.count).utf8)),
        ]
        entries.reserveCapacity(entries.count + partitions.count)
        for (offset, partition) in partitions.enumerated() {
            entries.append((
                "xl/worksheets/sheet\(offset + 1).xml",
                Data(worksheet(table: partition.table).utf8)
            ))
        }
        return try makeArchive(entries: entries)
    }

    private static func worksheet(table: ManualISIExportTable) -> String {
        let values = [table.headers] + table.rows
        var rows = ""
        rows.reserveCapacity(values.count * table.headers.count * 36)
        for (rowOffset, fields) in values.enumerated() {
            let rowNumber = rowOffset + 1
            rows += "<row r=\"\(rowNumber)\">"
            for (columnOffset, field) in fields.enumerated() {
                let reference = columnName(columnOffset + 1) + String(rowNumber)
                rows += "<c r=\"\(reference)\" t=\"inlineStr\"><is><t xml:space=\"preserve\">"
                rows += xml(field)
                rows += "</t></is></c>"
            }
            rows += "</row>"
        }
        let lastColumn = columnName(table.headers.count)
        let lastRow = values.count
        return """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
          <sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/></sheetView></sheetViews>
          <sheetData>\(rows)</sheetData>
          <autoFilter ref="A1:\(lastColumn)\(lastRow)"/>
        </worksheet>
        """
    }

    private static func columnName(_ oneBasedIndex: Int) -> String {
        var value = oneBasedIndex
        var result = ""
        while value > 0 {
            value -= 1
            result.insert(Character(UnicodeScalar(65 + value % 26)!), at: result.startIndex)
            value /= 26
        }
        return result
    }

    private static func contentTypes(sheetCount: Int) -> String {
        let sheets = (1...sheetCount).map {
            "<Override PartName=\"/xl/worksheets/sheet\($0).xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
        }.joined()
        return """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
          <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
          <Default Extension="xml" ContentType="application/xml"/>
          <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
          \(sheets)
        </Types>
        """
    }

    private static func workbook(sheetNames: [String]) -> String {
        let sheets = sheetNames.enumerated().map { offset, name in
            "<sheet name=\"\(xml(name))\" sheetId=\"\(offset + 1)\" r:id=\"rId\(offset + 1)\"/>"
        }.joined()
        return """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
          <sheets>\(sheets)</sheets>
        </workbook>
        """
    }

    private static func workbookRelationships(sheetCount: Int) -> String {
        let relationships = (1...sheetCount).map {
            "<Relationship Id=\"rId\($0)\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet\($0).xml\"/>"
        }.joined()
        return """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
          \(relationships)
        </Relationships>
        """
    }

    private static let rootRelationships = """
    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
      <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
    </Relationships>
    """
}

/// CSV has no worksheet concept. To preserve the same one-train-per-tab intent without inventing a
/// non-standard CSV dialect, this writer creates one ordinary CSV file per spike train in one ZIP.
public enum ManualISIResultCSVBundleExporter {
    public static func data(table: ManualISIExportTable) throws -> Data {
        let partitions = try ManualISIResultTablePartitioner.partitions(table: table)
        let names = ManualISIResultTablePartitioner.uniqueWorksheetNames(
            partitions.map(\.displayName)
        )
        let width = max(3, String(partitions.count).count)
        let entries = partitions.enumerated().map { offset, partition in
            let ordinal = String(format: "%0*d", width, offset + 1)
            let name = ManualISIResultTablePartitioner.safeFileComponent(names[offset])
            return (
                "\(ordinal)_\(name).csv",
                Data(partition.table.csv(lineEnding: "\r\n").utf8)
            )
        }
        return try makeArchive(entries: entries)
    }
}

private struct ManualISIResultPartition {
    let displayName: String
    let table: ManualISIExportTable
}

private enum ManualISIResultTablePartitioner {
    static func partitions(table: ManualISIExportTable) throws -> [ManualISIResultPartition] {
        guard let trainIDColumn = table.headers.firstIndex(of: "train_id") else {
            throw ManualISIResultXLSXExporterError.missingTrainIDColumn
        }
        guard !table.rows.isEmpty else {
            return [ManualISIResultPartition(displayName: "ISI labels", table: table)]
        }
        let trainNameColumn = table.headers.firstIndex(of: "train_name")
        var order: [String] = []
        var rowsByTrain: [String: [[String]]] = [:]
        var displayNameByTrain: [String: String] = [:]
        for row in table.rows {
            let trainID = row[trainIDColumn]
            if rowsByTrain[trainID] == nil {
                order.append(trainID)
                let name = trainNameColumn.map {
                    row[$0].trimmingCharacters(in: .whitespacesAndNewlines)
                }
                displayNameByTrain[trainID] = name.flatMap { $0.isEmpty ? nil : $0 }
                    ?? (trainID.isEmpty ? "Spike train \(order.count)" : trainID)
            }
            rowsByTrain[trainID, default: []].append(row)
        }
        return order.map { trainID in
            ManualISIResultPartition(
                displayName: displayNameByTrain[trainID] ?? trainID,
                table: ManualISIExportTable(
                    headers: table.headers,
                    rows: rowsByTrain[trainID] ?? []
                )
            )
        }
    }

    static func uniqueWorksheetNames(_ proposedNames: [String]) -> [String] {
        var used = Set<String>()
        return proposedNames.enumerated().map { offset, proposed in
            var base = sanitizedWorksheetName(proposed)
            if base.isEmpty { base = "Spike train \(offset + 1)" }
            var candidate = truncatedToUTF16Limit(base, maximum: 31)
            var collisionIndex = 2
            while !used.insert(candidate.lowercased()).inserted {
                let suffix = " (\(collisionIndex))"
                candidate = truncatedToUTF16Limit(
                    base,
                    maximum: 31 - suffix.utf16.count
                ) + suffix
                collisionIndex += 1
            }
            return candidate
        }
    }

    static func safeFileComponent(_ value: String) -> String {
        let invalid = CharacterSet(charactersIn: "/:\\")
            .union(.controlCharacters)
        let mapped = value.unicodeScalars.map { invalid.contains($0) ? "_" : String($0) }
            .joined()
        let trimmed = mapped.trimmingCharacters(in: CharacterSet(charactersIn: ". "))
        return trimmed.isEmpty ? "spike_train" : trimmed
    }

    private static func sanitizedWorksheetName(_ value: String) -> String {
        let invalid = CharacterSet(charactersIn: "[]:*?/\\")
            .union(.controlCharacters)
        var result = value.unicodeScalars.map { invalid.contains($0) ? "_" : String($0) }
            .joined()
            .trimmingCharacters(in: .whitespacesAndNewlines)
        while result.hasPrefix("'") { result.removeFirst() }
        while result.hasSuffix("'") { result.removeLast() }
        return result
    }

    private static func truncatedToUTF16Limit(_ value: String, maximum: Int) -> String {
        guard maximum > 0 else { return "" }
        var result = ""
        for character in value {
            let proposed = result + String(character)
            guard proposed.utf16.count <= maximum else { break }
            result = proposed
        }
        return result
    }
}

private func makeArchive(entries: [(String, Data)]) throws -> Data {
    let archive = try Archive(accessMode: .create)
    for (path, data) in entries {
        try archive.addEntry(
            with: path,
            type: .file,
            uncompressedSize: Int64(data.count),
            compressionMethod: .deflate,
            bufferSize: 64 * 1_024
        ) { position, size in
            let lower = Int(position)
            let upper = Swift.min(lower + size, data.count)
            return data.subdata(in: lower..<upper)
        }
    }
    guard let result = archive.data else {
        throw ManualISIResultXLSXExporterError.archiveCreationFailed
    }
    return result
}

private func xml(_ value: String) -> String {
    value
        .replacingOccurrences(of: "&", with: "&amp;")
        .replacingOccurrences(of: "<", with: "&lt;")
        .replacingOccurrences(of: ">", with: "&gt;")
        .replacingOccurrences(of: "\"", with: "&quot;")
        .replacingOccurrences(of: "'", with: "&apos;")
}
