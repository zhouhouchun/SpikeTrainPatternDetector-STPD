import Foundation
import STPDCore
import Testing
import ZIPFoundation
@testable import STPDTabularIO

private let worksheetNamespace =
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
private let documentRelationshipsNamespace =
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
private let packageRelationshipsNamespace =
    "http://schemas.openxmlformats.org/package/2006/relationships"
private let contentTypesNamespace =
    "http://schemas.openxmlformats.org/package/2006/content-types"

private struct WorksheetArchiveEntry {
    let path: String
    let data: Data

    init(_ path: String, _ text: String) {
        self.path = path
        self.data = Data(text.utf8)
    }
}

private func worksheetArchive(
    worksheetXML: String,
    sharedStringsXML: String? = nil,
    stylesXML: String? = nil,
    sheetName: String = "Data",
    sheetState: String? = nil
) throws -> Data {
    let sharedRelationship = sharedStringsXML == nil ? "" : """
    <Relationship Id="shared" Type="\(documentRelationshipsNamespace)/sharedStrings"
                  Target="sharedStrings.xml"/>
    """
    let stylesRelationship = stylesXML == nil ? "" : """
    <Relationship Id="styles" Type="\(documentRelationshipsNamespace)/styles"
                  Target="styles.xml"/>
    """
    let sharedOverride = sharedStringsXML == nil ? "" : """
    <Override PartName="/xl/sharedStrings.xml"
              ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>
    """
    let stylesOverride = stylesXML == nil ? "" : """
    <Override PartName="/xl/styles.xml"
              ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>
    """
    let state = sheetState.map { " state=\"\($0)\"" } ?? ""
    var entries = [
        WorksheetArchiveEntry(
            "[Content_Types].xml",
            """
            <Types xmlns="\(contentTypesNamespace)">
              <Default Extension="rels"
                       ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
              <Override PartName="/xl/workbook.xml"
                        ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
              <Override PartName="/xl/worksheets/sheet1.xml"
                        ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
              \(sharedOverride)\(stylesOverride)
            </Types>
            """
        ),
        WorksheetArchiveEntry(
            "_rels/.rels",
            """
            <Relationships xmlns="\(packageRelationshipsNamespace)">
              <Relationship Id="root" Type="\(documentRelationshipsNamespace)/officeDocument"
                            Target="xl/workbook.xml"/>
            </Relationships>
            """
        ),
        WorksheetArchiveEntry(
            "xl/workbook.xml",
            """
            <workbook xmlns="\(worksheetNamespace)" xmlns:r="\(documentRelationshipsNamespace)">
              <sheets><sheet name="\(sheetName)" sheetId="1"\(state) r:id="sheet"/></sheets>
            </workbook>
            """
        ),
        WorksheetArchiveEntry(
            "xl/_rels/workbook.xml.rels",
            """
            <Relationships xmlns="\(packageRelationshipsNamespace)">
              <Relationship Id="sheet" Type="\(documentRelationshipsNamespace)/worksheet"
                            Target="worksheets/sheet1.xml"/>
              \(sharedRelationship)\(stylesRelationship)
            </Relationships>
            """
        ),
        WorksheetArchiveEntry("xl/worksheets/sheet1.xml", worksheetXML),
    ]
    if let sharedStringsXML {
        entries.append(WorksheetArchiveEntry("xl/sharedStrings.xml", sharedStringsXML))
    }
    if let stylesXML {
        entries.append(WorksheetArchiveEntry("xl/styles.xml", stylesXML))
    }

    let archive = try Archive(accessMode: .create)
    for entry in entries {
        try archive.addEntry(
            with: entry.path,
            type: .file,
            uncompressedSize: Int64(entry.data.count),
            compressionMethod: .none,
            bufferSize: 64 * 1_024
        ) { position, size in
            let lower = Int(position)
            let upper = min(lower + size, entry.data.count)
            return entry.data.subdata(in: lower..<upper)
        }
    }
    return try #require(archive.data)
}

private func inspectWorksheet(_ data: Data) throws -> (
    CanonicalXLSXWorkbookInspection,
    CanonicalXLSXWorksheetDescriptor
) {
    let inspection = try CanonicalXLSXWorkbookInspector.inspect(
        data: data,
        limits: .supportedDatasetEnvelope
    )
    return (inspection, try #require(inspection.worksheets.first))
}

private func stageWorksheet(
    worksheetXML: String,
    sharedStringsXML: String? = nil,
    stylesXML: String? = nil,
    headerDecision: CanonicalTabularHeaderDecision = .headerless,
    sheetState: String? = nil,
    limits: CanonicalXLSXWorksheetStagingLimits = .supportedDatasetEnvelope
) throws -> CanonicalXLSXWorksheetStagingResult {
    let data = try worksheetArchive(
        worksheetXML: worksheetXML,
        sharedStringsXML: sharedStringsXML,
        stylesXML: stylesXML,
        sheetState: sheetState
    )
    let (inspection, descriptor) = try inspectWorksheet(data)
    return try CanonicalXLSXWorksheetStagingReader.readWithProvenance(
        inspection: inspection,
        worksheet: descriptor,
        headerDecision: headerDecision,
        limits: limits
    )
}

private func stagingError(
    worksheetXML: String,
    sharedStringsXML: String? = nil,
    stylesXML: String? = nil,
    headerDecision: CanonicalTabularHeaderDecision = .headerless,
    limits: CanonicalXLSXWorksheetStagingLimits = .supportedDatasetEnvelope
) -> CanonicalXLSXWorksheetStagingReaderError? {
    do {
        _ = try stageWorksheet(
            worksheetXML: worksheetXML,
            sharedStringsXML: sharedStringsXML,
            stylesXML: stylesXML,
            headerDecision: headerDecision,
            limits: limits
        )
        return nil
    } catch let error as CanonicalXLSXWorksheetStagingReaderError {
        return error
    } catch {
        Issue.record("Unexpected error type: \(error)")
        return nil
    }
}

private func worksheetDocument(
    _ body: String,
    rootAttributes: String = ""
) -> String {
    "<worksheet xmlns=\"\(worksheetNamespace)\"\(rootAttributes)>\(body)</worksheet>"
}

private func simpleStyles(_ numberFormatIDs: [UInt16]) -> String {
    let formats = numberFormatIDs.map { "<xf numFmtId=\"\($0)\"/>" }.joined()
    return """
    <styleSheet xmlns="\(worksheetNamespace)">
      <cellXfs count="\(numberFormatIDs.count)">\(formats)</cellXfs>
    </styleSheet>
    """
}

private func smallWorksheetLimits(
    columns: Int = 2,
    rows: Int = 2,
    cells: Int = 4,
    physicalRows: Int = 4,
    physicalCells: Int = 4,
    columnDefinitions: Int = 4,
    merges: Int = 4,
    decoded: Int = 8,
    total: Int = 32
) -> CanonicalXLSXWorksheetStagingLimits {
    CanonicalXLSXWorksheetStagingLimits(
        maximumColumnCount: columns,
        maximumLogicalDataRowCount: rows,
        maximumMaterializedDataCellCount: cells,
        maximumPhysicalRowCount: physicalRows,
        maximumPhysicalCellCount: physicalCells,
        maximumColumnDefinitionCount: columnDefinitions,
        maximumMergeRangeCount: merges,
        maximumDecodedCellUTF8ByteCount: decoded,
        maximumTotalMaterializedTextUTF8ByteCount: total
    )
}

private func supportedWorksheetLimits(
    replacing kind: CanonicalXLSXWorksheetStagingLimitKind,
    with actual: Int
) -> CanonicalXLSXWorksheetStagingLimits {
    let supported = CanonicalXLSXWorksheetStagingLimits.supportedDatasetEnvelope
    var columns = supported.maximumColumnCount
    var rows = supported.maximumLogicalDataRowCount
    var cells = supported.maximumMaterializedDataCellCount
    var physicalRows = supported.maximumPhysicalRowCount
    var physicalCells = supported.maximumPhysicalCellCount
    var columnDefinitions = supported.maximumColumnDefinitionCount
    var merges = supported.maximumMergeRangeCount
    var decoded = supported.maximumDecodedCellUTF8ByteCount
    var total = supported.maximumTotalMaterializedTextUTF8ByteCount
    switch kind {
    case .columns: columns = actual
    case .logicalDataRows: rows = actual
    case .materializedDataCells: cells = actual
    case .physicalRows: physicalRows = actual
    case .physicalCells: physicalCells = actual
    case .columnDefinitions: columnDefinitions = actual
    case .mergeRanges: merges = actual
    case .decodedCellUTF8Bytes: decoded = actual
    case .totalMaterializedTextUTF8Bytes: total = actual
    }
    return CanonicalXLSXWorksheetStagingLimits(
        maximumColumnCount: columns,
        maximumLogicalDataRowCount: rows,
        maximumMaterializedDataCellCount: cells,
        maximumPhysicalRowCount: physicalRows,
        maximumPhysicalCellCount: physicalCells,
        maximumColumnDefinitionCount: columnDefinitions,
        maximumMergeRangeCount: merges,
        maximumDecodedCellUTF8ByteCount: decoded,
        maximumTotalMaterializedTextUTF8ByteCount: total
    )
}

@Test
func fixedA1RectangleIncludesPhysicalAndHiddenCellsButNotFormatting() throws {
    let result = try stageWorksheet(
        worksheetXML: worksheetDocument(
            """
            <dimension ref="B2:D3"/>
            <sheetViews><sheetView workbookViewId="0"/></sheetViews>
            <cols><col min="16384" max="16384" hidden="1"/></cols>
            <sheetData>
              <row r="2" hidden="1"><c r="B2"><v>1</v></c></row>
              <row r="3"><c r="D3"/></row>
              <row r="100" hidden="1"/>
            </sheetData>
            <mergeCells count="1"><mergeCell ref="F6:G6"/></mergeCells>
            <pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75"
                         header="0.3" footer="0.3"/>
            """
        )
    )

    #expect(result.stagedImport.dataRowCount == 3)
    #expect(result.stagedImport.columns.map(\.cells) == [
        [.blank, .blank, .blank],
        [.blank, .spreadsheetNumber(rawLexeme: "1"), .blank],
        [.blank, .blank, .blank],
        [.blank, .blank, .blank],
    ])
    #expect(result.provenance.fixedOriginRectangle.a1Reference == "A1:D3")
    #expect(result.provenance.declaredDimension?.a1Reference == "B2:D3")
    #expect(result.provenance.rows.map(\.oneBasedRow) == [2, 3, 100])
    #expect(result.provenance.rows.map(\.hidden) == [true, false, true])
    #expect(result.provenance.columnDefinitions == [
        CanonicalXLSXColumnDefinitionProvenance(
            minimumOneBasedColumn: 16_384,
            maximumOneBasedColumn: 16_384,
            hidden: true,
            styleIndex: nil
        ),
    ])
    #expect(result.provenance.physicalCells.map(\.address.a1Reference) == ["B2", "D3"])
    #expect(result.provenance.physicalCells.map(\.kind) == [
        .number(rawLexeme: "1"), .blank,
    ])
    #expect(result.provenance.mergeRanges.map(\.a1Reference) == ["F6:G6"])
}

@Test
func hiddenColumnWithAPhysicalCellRemainsScientificInput() throws {
    let result = try stageWorksheet(
        worksheetXML: worksheetDocument(
            """
            <cols><col min="1" max="1" hidden="1"/></cols>
            <sheetData><row r="1"><c r="A1"><v>1.000000</v></c></row></sheetData>
            """
        )
    )
    #expect(result.stagedImport.columns[0].cells == [
        .spreadsheetNumber(rawLexeme: "1.000000"),
    ])
    #expect(result.provenance.columnDefinitions[0].hidden)
    #expect(result.provenance.fixedOriginRectangle.a1Reference == "A1:A1")
}

@Test
func headerDecisionPreservesTextKindsEmptyTextBlankAndExactBinding() throws {
    let worksheet = worksheetDocument(
        """
        <sheetData>
          <row r="1">
            <c r="A1" t="s"><v>0</v></c>
            <c r="B1" t="inlineStr"><is><t>event</t></is></c>
            <c r="C1" t="str"><v>channel</v></c>
            <c r="D1"/>
          </row>
          <row r="2">
            <c r="A2"><v>1.000000</v></c>
            <c r="B2" t="inlineStr"><is><t/></is></c>
            <c r="C2" t="s"><v>1</v></c>
            <c r="D2" t="n"><v>-2.5E-3</v></c>
          </row>
        </sheetData>
        """,
        rootAttributes: " xmlns:mc=\"\(worksheetMarkupCompatibilityNamespaceForTests)\" xmlns:xr=\"urn:revision\" mc:Ignorable=\"xr\" xr:uid=\"id\""
    )
    let shared = """
    <sst xmlns="\(worksheetNamespace)" count="2" uniqueCount="2">
      <si><t>unit</t></si><si><t/></si>
    </sst>
    """
    let result = try stageWorksheet(
        worksheetXML: worksheet,
        sharedStringsXML: shared,
        headerDecision: .firstRecordIsHeader
    )

    #expect(result.stagedImport.columns.map(\.header) == ["unit", "event", "channel", ""])
    #expect(result.stagedImport.columns.map(\.cells) == [
        [.spreadsheetNumber(rawLexeme: "1.000000")],
        [.text(rawText: "")],
        [.text(rawText: "")],
        [.spreadsheetNumber(rawLexeme: "-2.5E-3")],
    ])
    #expect(result.stagedImport.suggestions == .none)
    #expect(result.stagedImport.source == .excelWorkbook(worksheetName: "Data"))
    #expect(result.stagedImport.sourceTransactionBinding?.sourceBytesSHA256
        == result.provenance.sourceSHA256)
    #expect(result.stagedImport.sourceTransactionBinding?.selection == .excelWorksheet(
        worksheetName: "Data",
        sheetID: 1,
        relationshipID: "sheet",
        normalizedPartPath: "xl/worksheets/sheet1.xml"
    ))
    #expect(result.provenance.physicalCells.map(\.kind) == [
        .sharedString(index: 0, decodedText: "unit"),
        .inlineString(decodedText: "event"),
        .directString(rawLexeme: "channel", decodedText: "channel"),
        .blank,
        .number(rawLexeme: "1.000000"),
        .inlineString(decodedText: ""),
        .sharedString(index: 1, decodedText: ""),
        .number(rawLexeme: "-2.5E-3"),
    ])

    let headerless = try stageWorksheet(
        worksheetXML: worksheet,
        sharedStringsXML: shared,
        headerDecision: .headerless
    )
    #expect(headerless.stagedImport.columns.map(\.header) == [nil, nil, nil, nil])
    #expect(headerless.stagedImport.columns[3].cells[0] == .blank)
    #expect(headerless.stagedImport.columns[1].cells[1] == .text(rawText: ""))
}

private let worksheetMarkupCompatibilityNamespaceForTests =
    "http://schemas.openxmlformats.org/markup-compatibility/2006"

@Test
func numericHeaderIsNotSilentlyConvertedToText() {
    let error = stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"><v>12.00</v></c></row></sheetData>"
        ),
        headerDecision: .firstRecordIsHeader
    )
    #expect(error == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .numericHeaderUnsupported,
        reference: "A1",
        value: nil
    ))
}

@Test(arguments: [
    ("<c r=\"A1\"><f>1+1</f><v>2</v></c>", CanonicalXLSXWorksheetIssue.formulaUnsupported),
    ("<c r=\"A1\" t=\"b\"><v>1</v></c>", .unsupportedCellType),
    ("<c r=\"A1\" t=\"e\"><v>#N/A</v></c>", .unsupportedCellType),
    ("<c r=\"A1\" t=\"d\"><v>2026-01-01</v></c>", .unsupportedCellType),
    ("<c r=\"A1\" t=\"unknown\"><v>1</v></c>", .unsupportedCellType),
    ("<c r=\"A1\"><v/></c>", .invalidNumericLexeme),
    ("<c r=\"A1\" t=\"str\"/>", .invalidCellValueStructure),
    ("<c r=\"A1\" t=\"inlineStr\"><is/></c>", .invalidString),
])
func unsupportedOrMalformedCellRepresentationsFailClosed(
    cellXML: String,
    expectedIssue: CanonicalXLSXWorksheetIssue
) {
    let error = stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\">\(cellXML)</row></sheetData>"
        )
    )
    guard case .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: expectedIssue,
        reference: "A1",
        value: _
    ) = error else {
        Issue.record("Unexpected error: \(String(describing: error))")
        return
    }
}

@Test
func sharedStringsMustBeRelationshipBoundAndInBounds() {
    let worksheet = worksheetDocument(
        "<sheetData><row r=\"1\"><c r=\"A1\" t=\"s\"><v>1</v></c></row></sheetData>"
    )
    #expect(stagingError(worksheetXML: worksheet) == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .sharedStringsRequired,
        reference: "A1",
        value: nil
    ))
    #expect(stagingError(
        worksheetXML: worksheet,
        sharedStringsXML: "<sst xmlns=\"\(worksheetNamespace)\"><si><t>x</t></si></sst>"
    ) == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .sharedStringIndexOutOfBounds,
        reference: "A1",
        value: "1:1"
    ))
}

@Test
func supportPartFailuresNeverLeakInternalParserErrors() {
    let worksheet = worksheetDocument(
        "<sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>"
    )
    let invalidStyles = stagingError(
        worksheetXML: worksheet,
        stylesXML: "<styleSheet xmlns=\"\(worksheetNamespace)\"><cellXfs count=\"0\"/></styleSheet>"
    )
    #expect(invalidStyles == .invalidSupportPart(
        part: "xl/styles.xml",
        kind: .styles
    ))

    let sharedWorksheet = worksheetDocument(
        "<sheetData><row r=\"1\"><c r=\"A1\" t=\"s\"><v>0</v></c></row></sheetData>"
    )
    let invalidSharedStrings = stagingError(
        worksheetXML: sharedWorksheet,
        sharedStringsXML: "<sst xmlns=\"\(worksheetNamespace)\"><si/></sst>"
    )
    #expect(invalidSharedStrings == .invalidSupportPart(
        part: "xl/sharedStrings.xml",
        kind: .sharedStrings
    ))

    let unsafeStyles = """
    <!DOCTYPE styleSheet [<!ENTITY forbidden "x">]>
    <styleSheet xmlns="\(worksheetNamespace)"><cellXfs count="1"><xf numFmtId="0"/></cellXfs></styleSheet>
    """
    let first = stagingError(worksheetXML: worksheet, stylesXML: unsafeStyles)
    let second = stagingError(worksheetXML: worksheet, stylesXML: unsafeStyles)
    #expect(first == second)
    #expect(first == .sourcePackageFailure(.xmlSecurityViolation(
        part: "xl/styles.xml",
        issue: .documentTypeDeclaration
    )))
}

@Test(arguments: [
    ("<sheetData><row><c r=\"A1\"><v>1</v></c></row></sheetData>", CanonicalXLSXWorksheetIssue.missingRequiredAttribute),
    ("<sheetData><row r=\"1\"><c><v>1</v></c></row></sheetData>", .missingRequiredAttribute),
    ("<sheetData><row r=\"1\"><c r=\"a1\"><v>1</v></c></row></sheetData>", .invalidCellReference),
    ("<sheetData><row r=\"1\"><c r=\"A2\"><v>1</v></c></row></sheetData>", .cellRowMismatch),
    ("<sheetData><row r=\"2\"><c r=\"A2\"><v>1</v></c></row><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>", .nonascendingRows),
    ("<sheetData><row r=\"1\"><c r=\"B1\"><v>1</v></c><c r=\"A1\"><v>1</v></c></row></sheetData>", .nonascendingCells),
    ("<dimension ref=\"A0:B2\"/><sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>", .invalidRangeReference),
    ("<dimension ref=\"B2:C3\"/><sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>", .dimensionDoesNotContainCell),
])
func coordinatesAndDeclaredDimensionFailClosed(
    body: String,
    expectedIssue: CanonicalXLSXWorksheetIssue
) {
    let error = stagingError(worksheetXML: worksheetDocument(body))
    guard case .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: expectedIssue,
        reference: _,
        value: _
    ) = error else {
        Issue.record("Unexpected error: \(String(describing: error))")
        return
    }
}

@Test
func declaredDimensionIsRecordedButNeverExpandsTheFixedOriginRectangle() throws {
    let result = try stageWorksheet(
        worksheetXML: worksheetDocument(
            """
            <dimension ref="A1:XFD1048576"/>
            <sheetData><row r="1"><c r="A1"><v>1</v></c></row></sheetData>
            """
        )
    )
    #expect(result.provenance.declaredDimension?.a1Reference == "A1:XFD1048576")
    #expect(result.provenance.fixedOriginRectangle.a1Reference == "A1:A1")
    #expect(result.stagedImport.columns.count == 1)
    #expect(result.stagedImport.dataRowCount == 1)
}

@Test
func numericCellsPreserveFiniteRawLexemesAndRejectInvalidStorage() throws {
    let accepted = try stageWorksheet(
        worksheetXML: worksheetDocument(
            """
            <sheetData><row r="1">
              <c r="A1"><v>0.0000005</v></c>
              <c r="B1"><v>1e19</v></c>
              <c r="C1"><v>+1.2500E-3</v></c>
            </row></sheetData>
            """
        )
    )
    #expect(accepted.stagedImport.columns.map(\.cells) == [
        [.spreadsheetNumber(rawLexeme: "0.0000005")],
        [.spreadsheetNumber(rawLexeme: "1e19")],
        [.spreadsheetNumber(rawLexeme: "+1.2500E-3")],
    ])

    for raw in [" 1", "1 ", "NaN", "INF", "1e999", String(repeating: "1", count: 129)] {
        let error = stagingError(
            worksheetXML: worksheetDocument(
                "<sheetData><row r=\"1\"><c r=\"A1\"><v>\(raw)</v></c></row></sheetData>"
            )
        )
        #expect(error == .invalidWorksheet(
            part: "xl/worksheets/sheet1.xml",
            issue: .invalidNumericLexeme,
            reference: "A1",
            value: nil
        ))
    }
}

@Test
func stylesApplyCellThenRowThenColumnPrecedenceAndOnlyNumbersRequireNonDateProof() throws {
    let styles = simpleStyles([0, 14, 5])
    let accepted = try stageWorksheet(
        worksheetXML: worksheetDocument(
            """
            <cols><col min="1" max="2" style="1"/></cols>
            <sheetData>
              <row r="1" s="1" customFormat="1">
                <c r="A1" s="0"><v>1</v></c>
                <c r="B1" s="1" t="inlineStr"><is><t>text</t></is></c>
                <c r="C1" s="2"/>
              </row>
            </sheetData>
            """
        ),
        stylesXML: styles
    )
    #expect(accepted.stagedImport.columns.map(\.cells) == [
        [.spreadsheetNumber(rawLexeme: "1")],
        [.text(rawText: "text")],
        [.blank],
    ])
    #expect(accepted.provenance.physicalCells.map(\.numberFormatDisposition) == [
        .provenNonDate, .dateOrTime, .unsupported,
    ])
    #expect(accepted.provenance.physicalCells.map(\.effectiveStyleSource) == [
        .cell, .cell, .cell,
    ])

    for body in [
        "<sheetData><row r=\"1\"><c r=\"A1\" s=\"1\"><v>1</v></c></row></sheetData>",
        "<cols><col min=\"1\" max=\"1\" style=\"1\"/></cols><sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>",
        "<sheetData><row r=\"1\" s=\"1\" customFormat=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>",
    ] {
        let error = stagingError(
            worksheetXML: worksheetDocument(body),
            stylesXML: styles
        )
        guard case .invalidWorksheet(
            part: "xl/worksheets/sheet1.xml",
            issue: .dateOrTimeNumberFormat,
            reference: "A1",
            value: _
        ) = error else {
            Issue.record("Unexpected error: \(String(describing: error))")
            continue
        }
    }

    let unproven = stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\" s=\"2\"><v>1</v></c></row></sheetData>"
        ),
        stylesXML: styles
    )
    #expect(unproven == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .unprovenNumberFormat,
        reference: "A1",
        value: "2:5"
    ))
}

@Test
func unsupportedDuplicateForeignAndOutOfOrderWorksheetSectionsFailClosed() {
    let cases: [(String, CanonicalXLSXWorksheetIssue)] = [
        (
            "<scenarios/><sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>",
            .unsupportedSection
        ),
        (
            "<sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData><cols/>",
            .invalidSectionOrder
        ),
        (
            "<dimension ref=\"A1\"/><dimension ref=\"A1\"/><sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>",
            .duplicateSection
        ),
        (
            "<futureSection/><sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>",
            .invalidElementStructure
        ),
    ]
    for (body, expectedIssue) in cases {
        let error = stagingError(worksheetXML: worksheetDocument(body))
        guard case .invalidWorksheet(
            part: "xl/worksheets/sheet1.xml",
            issue: expectedIssue,
            reference: _,
            value: _
        ) = error else {
            Issue.record("Unexpected error: \(String(describing: error))")
            continue
        }
    }

    let foreign = stagingError(
        worksheetXML: worksheetDocument(
            "<x:foreign xmlns:x=\"urn:foreign\"/><sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>"
        )
    )
    guard case .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .invalidElementStructure,
        reference: _,
        value: _
    ) = foreign else {
        Issue.record("Unexpected error: \(String(describing: foreign))")
        return
    }
}

@Test
func styleReferencesRequireABoundCatalogAndValidIndex() {
    let noStyles = stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\" s=\"0\"/></row></sheetData>"
        )
    )
    #expect(noStyles == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .styleIndexRequiresStylesPart,
        reference: "A1",
        value: "0"
    ))
    let outOfBounds = stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\" s=\"1\"/></row></sheetData>"
        ),
        stylesXML: simpleStyles([0])
    )
    #expect(outOfBounds == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .styleIndexOutOfBounds,
        reference: "A1",
        value: "1:1"
    ))
}

@Test
func mergeRangesAreValidatedAndCannotIntersectTheLogicalTable() {
    let intersecting = stagingError(
        worksheetXML: worksheetDocument(
            """
            <sheetData><row r="1"><c r="A1"><v>1</v></c></row></sheetData>
            <mergeCells><mergeCell ref="A1:B1"/></mergeCells>
            """
        )
    )
    #expect(intersecting == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .mergeIntersectsTable,
        reference: "A1:B1",
        value: "A1:A1"
    ))
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            """
            <sheetData><row r="1"><c r="A1"><v>1</v></c></row></sheetData>
            <mergeCells><mergeCell ref="C3:C3"/></mergeCells>
            """
        )
    ) == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .mergeMustSpanMultipleCells,
        reference: "C3:C3",
        value: nil
    ))
}

@Test
func worksheetDescriptorIsBoundToOneExactInspectionEvenForIdenticalBytes() throws {
    let data = try worksheetArchive(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>"
        )
    )
    let (inspectionA, descriptorA) = try inspectWorksheet(data)
    let (inspectionB, descriptorB) = try inspectWorksheet(data)
    #expect(inspectionA.sourceSHA256 == inspectionB.sourceSHA256)
    #expect(throws: CanonicalXLSXWorksheetStagingReaderError
        .worksheetSelectionNotInInspection) {
        try CanonicalXLSXWorksheetStagingReader.read(
            inspection: inspectionA,
            worksheet: descriptorB,
            headerDecision: .headerless,
            limits: .supportedDatasetEnvelope
        )
    }
    _ = try CanonicalXLSXWorksheetStagingReader.read(
        inspection: inspectionA,
        worksheet: descriptorA,
        headerDecision: .headerless,
        limits: .supportedDatasetEnvelope
    )
}

@Test(arguments: [
    (nil, CanonicalXLSXWorksheetVisibility.visible),
    ("hidden", .hidden),
    ("veryHidden", .veryHidden),
] as [(String?, CanonicalXLSXWorksheetVisibility)])
func explicitlySelectedWorksheetVisibilityIsProvenanceOnly(
    state: String?,
    expectedVisibility: CanonicalXLSXWorksheetVisibility
) throws {
    let result = try stageWorksheet(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>"
        ),
        sheetState: state
    )
    #expect(result.provenance.visibility == expectedVisibility)
    #expect(result.stagedImport.columns[0].cells == [.spreadsheetNumber(rawLexeme: "1")])
}

@Test
func xlsxAndCSVWorkloadEnvelopesRemainIndependentAndExact() {
    let xlsx = CanonicalXLSXWorksheetStagingLimits.supportedDatasetEnvelope
    #expect(xlsx.maximumColumnCount == 1_024)
    #expect(xlsx.maximumLogicalDataRowCount == 1_000_000)
    #expect(xlsx.maximumMaterializedDataCellCount == 5_000_000)
    #expect(xlsx.maximumPhysicalRowCount == 1_000_000)
    #expect(xlsx.maximumPhysicalCellCount == 1_000_000)
    #expect(xlsx.maximumColumnDefinitionCount == 16_384)
    #expect(xlsx.maximumMergeRangeCount == 100_000)
    #expect(xlsx.maximumDecodedCellUTF8ByteCount == 65_794)
    #expect(xlsx.maximumTotalMaterializedTextUTF8ByteCount == 67_108_864)

    let csv = CanonicalCSVStagingLimits.supportedDatasetEnvelope
    #expect(csv.maximumColumnCount == 512)
    #expect(csv.maximumLogicalDataRowCount == 1_000_000)
    #expect(csv.maximumMaterializedDataCellCount == 1_000_000)
}

@Test
func worksheetLimitsFailAtTheFirstExactExceededBoundary() throws {
    let exact = try stageWorksheet(
        worksheetXML: worksheetDocument(
            """
            <sheetData>
              <row r="1"><c r="A1" t="inlineStr"><is><t>aa</t></is></c>
                         <c r="B1" t="inlineStr"><is><t>b</t></is></c></row>
              <row r="2"><c r="A2" t="inlineStr"><is><t>c</t></is></c>
                         <c r="B2" t="inlineStr"><is><t>dd</t></is></c></row>
            </sheetData>
            """
        ),
        limits: smallWorksheetLimits(decoded: 2, total: 6)
    )
    #expect(exact.stagedImport.dataRowCount == 2)
    #expect(exact.provenance.totalMaterializedTextUTF8ByteCount == 6)

    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"C1\"><v>1</v></c></row></sheetData>"
        ),
        limits: smallWorksheetLimits()
    ) == .columnLimitExceeded(maximum: 2, actual: 3, reference: "C1"))
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"3\"><c r=\"A3\"><v>1</v></c></row></sheetData>"
        ),
        limits: smallWorksheetLimits()
    ) == .logicalDataRowLimitExceeded(maximum: 2, actual: 3))
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row><row r=\"2\"><c r=\"B2\"><v>2</v></c></row></sheetData>"
        ),
        limits: smallWorksheetLimits(cells: 3)
    ) == .materializedDataCellLimitExceeded(maximum: 3, actual: 4))
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>abc</t></is></c></row></sheetData>"
        ),
        limits: smallWorksheetLimits(decoded: 2)
    ) == .decodedCellByteLimitExceeded(reference: "A1", maximum: 2, actual: 3))
}

@Test
func everyCallerSelectedLimitMustBePositiveAndWithinTheSupportedEnvelope() throws {
    let source = try worksheetArchive(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"><v>1</v></c></row></sheetData>"
        )
    )
    let (inspection, descriptor) = try inspectWorksheet(source)
    let supported = CanonicalXLSXWorksheetStagingLimits.supportedDatasetEnvelope
    let cases: [(CanonicalXLSXWorksheetStagingLimitKind, Int, Int)] = [
        (.columns, 0, supported.maximumColumnCount),
        (.logicalDataRows, 0, supported.maximumLogicalDataRowCount),
        (.materializedDataCells, 0, supported.maximumMaterializedDataCellCount),
        (.physicalRows, 0, supported.maximumPhysicalRowCount),
        (.physicalCells, 0, supported.maximumPhysicalCellCount),
        (.columnDefinitions, 0, supported.maximumColumnDefinitionCount),
        (.mergeRanges, 0, supported.maximumMergeRangeCount),
        (.decodedCellUTF8Bytes, 0, supported.maximumDecodedCellUTF8ByteCount),
        (
            .totalMaterializedTextUTF8Bytes,
            0,
            supported.maximumTotalMaterializedTextUTF8ByteCount
        ),
    ]
    for (kind, invalid, maximum) in cases {
        #expect(throws: CanonicalXLSXWorksheetStagingReaderError.invalidLimit(
            kind: kind,
            actual: invalid
        )) {
            try CanonicalXLSXWorksheetStagingReader.read(
                inspection: inspection,
                worksheet: descriptor,
                headerDecision: .headerless,
                limits: supportedWorksheetLimits(replacing: kind, with: invalid)
            )
        }
        let excessive = maximum + 1
        #expect(throws: CanonicalXLSXWorksheetStagingReaderError.limitExceedsSupportedEnvelope(
            kind: kind,
            maximumSupported: maximum,
            actual: excessive
        )) {
            try CanonicalXLSXWorksheetStagingReader.read(
                inspection: inspection,
                worksheet: descriptor,
                headerDecision: .headerless,
                limits: supportedWorksheetLimits(replacing: kind, with: excessive)
            )
        }
    }
}

@Test
func physicalStructureLimitsFailOnTheFirstExcessItem() {
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"/></row><row r=\"2\"><c r=\"A2\"/></row></sheetData>"
        ),
        limits: smallWorksheetLimits(physicalRows: 1)
    ) == .physicalRowLimitExceeded(maximum: 1, actual: 2, reference: "2"))
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"/><c r=\"B1\"/></row></sheetData>"
        ),
        limits: smallWorksheetLimits(physicalCells: 1)
    ) == .physicalCellLimitExceeded(maximum: 1, actual: 2, reference: "B1"))
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<cols><col min=\"1\" max=\"1\"/><col min=\"2\" max=\"2\"/></cols><sheetData><row r=\"1\"><c r=\"A1\"/></row></sheetData>"
        ),
        limits: smallWorksheetLimits(columnDefinitions: 1)
    ) == .columnDefinitionLimitExceeded(maximum: 1, actual: 2))
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"1\"><c r=\"A1\"/></row></sheetData><mergeCells><mergeCell ref=\"C3:D3\"/><mergeCell ref=\"E4:F4\"/></mergeCells>"
        ),
        limits: smallWorksheetLimits(merges: 1)
    ) == .mergeRangeLimitExceeded(maximum: 1, actual: 2))
}

@Test
func firstDenseCellBeyondSupportedEnvelopeFailsBeforeDenseAllocation() {
    #expect(stagingError(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"5001\"><c r=\"ALL5001\"><v>1</v></c></row></sheetData>"
        )
    ) == .materializedDataCellLimitExceeded(maximum: 5_000_000, actual: 5_001_000))
}

@Test
func repeatedSharedStringsCountEveryMaterializedOccurrence() {
    let worksheet = worksheetDocument(
        """
        <sheetData><row r="1">
          <c r="A1" t="s"><v>0</v></c><c r="B1" t="s"><v>0</v></c>
        </row></sheetData>
        """
    )
    let shared = "<sst xmlns=\"\(worksheetNamespace)\"><si><t>xxx</t></si></sst>"
    #expect(stagingError(
        worksheetXML: worksheet,
        sharedStringsXML: shared,
        limits: smallWorksheetLimits(total: 5)
    ) == .totalMaterializedTextByteLimitExceeded(
        reference: "B1",
        maximum: 5,
        actual: 6
    ))
}

@Test
func lateWorksheetFailureIsAtomicAndDeterministic() throws {
    let worksheet = worksheetDocument(
        """
        <sheetData>
          <row r="1"><c r="A1"><v>1</v></c></row>
          <row r="1000"><c r="A1000" t="s"><v>1</v></c></row>
        </sheetData>
        """
    )
    let shared = "<sst xmlns=\"\(worksheetNamespace)\"><si><t>x</t></si></sst>"
    let first = stagingError(worksheetXML: worksheet, sharedStringsXML: shared)
    let second = stagingError(worksheetXML: worksheet, sharedStringsXML: shared)
    #expect(first == second)
    #expect(first == .invalidWorksheet(
        part: "xl/worksheets/sheet1.xml",
        issue: .sharedStringIndexOutOfBounds,
        reference: "A1000",
        value: "1:1"
    ))
}

@Test
func ownerRealWorkbookBenchmark() throws {
    guard let path = ProcessInfo.processInfo.environment["STPD_REAL_XLSX_BENCHMARK_PATH"] else {
        return
    }
    let source = try Data(contentsOf: URL(fileURLWithPath: path))
    let (inspection, descriptor) = try inspectWorksheet(source)
    let started = ContinuousClock.now
    let result = try CanonicalXLSXWorksheetStagingReader.readWithProvenance(
        inspection: inspection,
        worksheet: descriptor,
        headerDecision: .firstRecordIsHeader,
        limits: .supportedDatasetEnvelope
    )
    let elapsed = started.duration(to: .now)
    if path.contains("PD_GPi") {
        #expect(result.stagedImport.columns.count == 198)
        #expect(result.stagedImport.dataRowCount == 23_770)
    } else if path.contains("PD_STN") {
        #expect(result.stagedImport.columns.count == 672)
        #expect(result.stagedImport.dataRowCount == 4_422)
    }
    print("REAL_XLSX_BENCHMARK path=\(path) elapsed=\(elapsed)")
}

@Test
func syntheticSupportedEnvelopeBenchmark() throws {
    guard ProcessInfo.processInfo.environment["STPD_XLSX_CAP_BENCHMARK"] == "1" else {
        return
    }
    let started = ContinuousClock.now
    let result = try stageWorksheet(
        worksheetXML: worksheetDocument(
            "<sheetData><row r=\"5000\"><c r=\"ALL5000\"><v>1</v></c></row></sheetData>"
        )
    )
    let elapsed = started.duration(to: .now)
    #expect(result.stagedImport.columns.count == 1_000)
    #expect(result.stagedImport.dataRowCount == 5_000)
    #expect(result.stagedImport.columns[999].cells[4_999]
        == .spreadsheetNumber(rawLexeme: "1"))
    print("SYNTHETIC_XLSX_CAP_BENCHMARK elapsed=\(elapsed)")
}
