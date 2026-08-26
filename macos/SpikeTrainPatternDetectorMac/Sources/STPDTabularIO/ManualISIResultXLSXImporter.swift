import Foundation
import STPDCore

/// Reads only the ordinary XLSX tables produced by `ManualISIResultXLSXExporter` and returns an
/// unsealed, format-neutral draft. The Core decoder subsequently binds every row to the currently
/// persisted canonical import before any workbench state can change.
public enum ManualISIResultXLSXImporter {
    public static func `import`(
        data: Data,
        inspectionLimits: CanonicalXLSXInspectionLimits = .supportedDatasetEnvelope,
        worksheetLimits: CanonicalXLSXWorksheetStagingLimits = .supportedDatasetEnvelope
    ) throws -> CanonicalManualISIDraftImport {
        let inspection = try CanonicalXLSXWorkbookInspector.inspect(
            data: data,
            limits: inspectionLimits
        )
        let visibleSheets = inspection.worksheets.filter { $0.visibility == .visible }
        guard !visibleSheets.isEmpty else {
            throw ManualISIResultXLSXImporterError.noVisibleWorksheets
        }
        let tables = try visibleSheets.map { worksheet in
            let staged = try CanonicalXLSXWorksheetStagingReader.read(
                inspection: inspection,
                worksheet: worksheet,
                headerDecision: .firstRecordIsHeader,
                limits: worksheetLimits
            )
            return try table(from: staged, worksheetName: worksheet.name)
        }
        return try CanonicalManualISIDraftImport.decode(tables: tables)
    }

    private static func table(
        from staged: StagedScientificImport,
        worksheetName: String
    ) throws -> ManualISIExportTable {
        let headers = try staged.columns.map { column -> String in
            guard let header = column.header else {
                throw ManualISIResultXLSXImporterError.headerlessWorksheet(
                    name: worksheetName
                )
            }
            return header
        }
        var rows = Array(
            repeating: Array(repeating: "", count: staged.columns.count),
            count: staged.dataRowCount
        )
        for (columnOffset, column) in staged.columns.enumerated() {
            for (rowOffset, value) in column.cells.enumerated() {
                switch value {
                case .text(let rawText):
                    rows[rowOffset][columnOffset] = rawText
                case .blank:
                    // The exporter writes even empty pattern cells as inline strings. A physical
                    // blank therefore cannot be silently normalized into an empty field.
                    throw ManualISIResultXLSXImporterError.blankCell(
                        worksheet: worksheetName,
                        row: rowOffset + 2,
                        column: columnOffset + 1
                    )
                case .spreadsheetNumber:
                    throw ManualISIResultXLSXImporterError.numericCell(
                        worksheet: worksheetName,
                        row: rowOffset + 2,
                        column: columnOffset + 1
                    )
                }
            }
        }
        return ManualISIExportTable(headers: headers, rows: rows)
    }
}

public enum ManualISIResultXLSXImporterError: Error, Equatable, Sendable, LocalizedError {
    case noVisibleWorksheets
    case headerlessWorksheet(name: String)
    case blankCell(worksheet: String, row: Int, column: Int)
    case numericCell(worksheet: String, row: Int, column: Int)

    public var errorDescription: String? {
        switch self {
        case .noVisibleWorksheets:
            return "The manual-ISI workbook has no visible worksheets."
        case .headerlessWorksheet(let name):
            return "Worksheet \(name) has no header row."
        case .blankCell(let worksheet, let row, let column):
            return "Worksheet \(worksheet), cell \(column):\(row) is physically blank."
        case .numericCell(let worksheet, let row, let column):
            return "Worksheet \(worksheet), cell \(column):\(row) was converted to a number. Exact manual-ISI imports require text cells."
        }
    }
}
