import Foundation
import STPDCore
import ZIPFoundation

public enum RectangularTableXLSXExporterError: Error, Sendable, LocalizedError {
    case archiveCreationFailed

    public var errorDescription: String? {
        switch self {
        case .archiveCreationFailed:
            return "Could not create the XLSX report."
        }
    }
}

/// A single-sheet XLSX writer for ordinary rectangular reports. All values remain inline text so
/// identity digests and exact integer fields are never converted through spreadsheet floating point.
public enum RectangularTableXLSXExporter {
    public static func data(
        table: ManualISIExportTable,
        worksheetName: String
    ) throws -> Data {
        let safeName = sanitizedWorksheetName(worksheetName)
        let entries: [(String, Data)] = [
            ("[Content_Types].xml", Data(contentTypes.utf8)),
            ("_rels/.rels", Data(rootRelationships.utf8)),
            ("xl/workbook.xml", Data(workbook(sheetName: safeName).utf8)),
            ("xl/_rels/workbook.xml.rels", Data(workbookRelationships.utf8)),
            ("xl/worksheets/sheet1.xml", Data(worksheet(table: table).utf8)),
        ]
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
            throw RectangularTableXLSXExporterError.archiveCreationFailed
        }
        return result
    }

    private static func worksheet(table: ManualISIExportTable) -> String {
        let values = [table.headers] + table.rows
        let rows = values.enumerated().map { rowOffset, fields in
            let rowNumber = rowOffset + 1
            let cells = fields.enumerated().map { columnOffset, field in
                let reference = columnName(columnOffset + 1) + String(rowNumber)
                return "<c r=\"\(reference)\" t=\"inlineStr\"><is><t xml:space=\"preserve\">\(xml(field))</t></is></c>"
            }.joined()
            return "<row r=\"\(rowNumber)\">\(cells)</row>"
        }.joined()
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

    private static func workbook(sheetName: String) -> String {
        """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
          <sheets><sheet name="\(xml(sheetName))" sheetId="1" r:id="rId1"/></sheets>
        </workbook>
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

    private static func sanitizedWorksheetName(_ value: String) -> String {
        let invalid = CharacterSet(charactersIn: "[]:*?/\\").union(.controlCharacters)
        var result = value.unicodeScalars.map { invalid.contains($0) ? "_" : String($0) }
            .joined().trimmingCharacters(in: .whitespacesAndNewlines)
        while result.hasPrefix("'") { result.removeFirst() }
        while result.hasSuffix("'") { result.removeLast() }
        if result.isEmpty { result = "Report" }
        var limited = ""
        for character in result {
            let proposed = limited + String(character)
            guard proposed.utf16.count <= 31 else { break }
            limited = proposed
        }
        return limited
    }

    private static func xml(_ value: String) -> String {
        value
            .replacingOccurrences(of: "&", with: "&amp;")
            .replacingOccurrences(of: "<", with: "&lt;")
            .replacingOccurrences(of: ">", with: "&gt;")
            .replacingOccurrences(of: "\"", with: "&quot;")
            .replacingOccurrences(of: "'", with: "&apos;")
    }

    private static let contentTypes = """
    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
      <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
      <Default Extension="xml" ContentType="application/xml"/>
      <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
      <Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
    </Types>
    """

    private static let rootRelationships = """
    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
      <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
    </Relationships>
    """

    private static let workbookRelationships = """
    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
      <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>
    </Relationships>
    """
}
