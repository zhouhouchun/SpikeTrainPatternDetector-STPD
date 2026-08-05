import Foundation
import Testing
@testable import STPDTabularIO

private let stylePart = "xl/styles.xml"
private let styleNamespace =
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main"

private func styleDocument(
    _ body: String,
    rootNamespace: String = styleNamespace,
    rootAttributes: String = ""
) -> Data {
    Data(
        """
        <styleSheet xmlns="\(rootNamespace)"\(rootAttributes)>\(body)</styleSheet>
        """.utf8
    )
}

private func parseStyles(
    _ body: String,
    rootNamespace: String = styleNamespace,
    rootAttributes: String = ""
) throws -> CanonicalXLSXStyleCatalog {
    try CanonicalXLSXStyleCatalogParser.parse(
        data: styleDocument(
            body,
            rootNamespace: rootNamespace,
            rootAttributes: rootAttributes
        ),
        part: stylePart,
        limits: .supportedDatasetEnvelope
    )
}

private func xmlAttribute(_ value: String) -> String {
    value
        .replacingOccurrences(of: "&", with: "&amp;")
        .replacingOccurrences(of: "<", with: "&lt;")
        .replacingOccurrences(of: ">", with: "&gt;")
        .replacingOccurrences(of: "\"", with: "&quot;")
        .replacingOccurrences(of: "'", with: "&apos;")
}

private func customFormatDocument(
    code: String,
    id: UInt16 = 164,
    cellXFAttributes: String = ""
) -> String {
    """
    <numFmts count="1"><numFmt numFmtId="\(id)" formatCode="\(xmlAttribute(code))"/></numFmts>
    <cellXfs count="1"><xf numFmtId="\(id)"\(cellXFAttributes)/></cellXfs>
    """
}

private func expectStyleIssue(
    _ expectedIssue: CanonicalXLSXStyleIssue,
    value expectedValue: String? = nil,
    body: String,
    rootNamespace: String = styleNamespace,
    rootAttributes: String = ""
) {
    do {
        _ = try parseStyles(
            body,
            rootNamespace: rootNamespace,
            rootAttributes: rootAttributes
        )
        Issue.record("Expected style issue \(expectedIssue.rawValue)")
    } catch let error as CanonicalXLSXStyleParsingError {
        #expect(error == .invalidStyles(
            part: stylePart,
            issue: expectedIssue,
            value: expectedValue
        ))
    } catch {
        Issue.record("Unexpected error type: \(error)")
    }
}

private func expectedDisposition(
    _ expected: CanonicalXLSXNumberFormatDisposition,
    for code: String
) throws {
    let catalog = try parseStyles(customFormatDocument(code: code))
    #expect(catalog.cellFormats == [
        CanonicalXLSXCellFormat(numberFormatID: 164, disposition: expected),
    ])
}

@Test
func xlsxStyleCatalogClassifiesEveryFrozenBuiltInFormatID() throws {
    let provenNonDate = Array(0...4) + Array(9...13) + Array(37...40) + [48, 49]
    let dateOrTime = Array(14...22) + Array(27...36) + Array(45...47)
        + Array(50...58) + Array(71...81)
    let unsupported = [5, 8, 23, 26, 41, 44, 59, 70, 82, 163, 164, 65_535]
    let all = provenNonDate + dateOrTime + unsupported
    let xfs = all.map { "<xf numFmtId=\"\($0)\"/>" }.joined()

    let catalog = try parseStyles(
        "<cellXfs count=\"\(all.count)\">\(xfs)</cellXfs>"
    )

    #expect(catalog.cellFormats.map(\.numberFormatID) == all.map(UInt16.init))
    #expect(catalog.cellFormats.map(\.disposition) ==
        Array(repeating: .provenNonDate, count: provenNonDate.count)
        + Array(repeating: .dateOrTime, count: dateOrTime.count)
        + Array(repeating: .unsupported, count: unsupported.count))
}

@Test(arguments: [
    "0.000000",
    "#0.000",
    "\"m/s\"0",
    "0\\m",
    "0_m",
    "0*m",
    "[Red]0.0",
    "[Color56]0",
    "[<=100.5]0",
    "@",
])
func xlsxCustomNumericFormatsHaveAConservativeNonDateProof(_ code: String) throws {
    try expectedDisposition(.provenNonDate, for: code)
}

@Test(arguments: [
    "yyyy-mm-dd",
    "mm:ss",
    "[h]:mm",
    "AM/PM",
])
func xlsxCustomDateOrTimeFormatsAreDetected(_ code: String) throws {
    try expectedDisposition(.dateOrTime, for: code)
}

@Test(arguments: [
    "General",
    "[$-F400]0",
    "0;0;0;0;0",
    "0*a*b",
    "\"unclosed",
    "0.00E+00",
    "0年",
    "0\\",
    "[Orange]0",
    "[Color57]0",
    "[<=]0",
])
func xlsxUnsupportedCustomFormatsNeverAcquireANonDateProof(_ code: String) throws {
    try expectedDisposition(.unsupported, for: code)
}

@Test
func xlsxQuotedBackslashCannotHideAnActiveDateToken() throws {
    // A quote ends the literal even when immediately preceded by a backslash.
    try expectedDisposition(.dateOrTime, for: "\"x\\\"m\\m0")
}

@Test
func xlsxStylesAvailabilityFreezesTheStyleSelectionMatrix() throws {
    let absent = CanonicalXLSXStylesAvailability.absent
    #expect(try absent.requireTimestampSafeCellFormat(explicitStyleIndex: nil) ==
        CanonicalXLSXCellFormat(numberFormatID: 0, disposition: .provenNonDate))
    #expect(throws: CanonicalXLSXStyleResolutionError.styleIndexRequiresStylesPart(index: 0)) {
        try absent.requireTimestampSafeCellFormat(explicitStyleIndex: 0)
    }

    let catalog = try parseStyles(
        """
        <cellXfs count="3">
          <xf numFmtId="0"/><xf numFmtId="14"/><xf numFmtId="5"/>
        </cellXfs>
        """
    )
    let present = CanonicalXLSXStylesAvailability.present(catalog)
    #expect(try present.requireTimestampSafeCellFormat(explicitStyleIndex: nil).numberFormatID == 0)
    #expect(throws: CanonicalXLSXStyleResolutionError.dateOrTimeNumberFormat(
        styleIndex: 1,
        numberFormatID: 14
    )) {
        try present.requireTimestampSafeCellFormat(explicitStyleIndex: 1)
    }
    #expect(throws: CanonicalXLSXStyleResolutionError.unprovenNumberFormat(
        styleIndex: 2,
        numberFormatID: 5
    )) {
        try present.requireTimestampSafeCellFormat(explicitStyleIndex: 2)
    }
    #expect(throws: CanonicalXLSXStyleResolutionError.cellStyleIndexOutOfBounds(
        index: 3,
        count: 3
    )) {
        try present.requireTimestampSafeCellFormat(explicitStyleIndex: 3)
    }
}

@Test
func xlsxStyleInheritanceUsesCellXFAndRequiresExplicitOverride() throws {
    let catalog = try parseStyles(
        """
        <cellStyleXfs count="2"><xf numFmtId="14"/><xf numFmtId="0"/></cellStyleXfs>
        <cellXfs count="3">
          <xf numFmtId="14" xfId="0"/>
          <xf numFmtId="0" xfId="0" applyNumberFormat="true"/>
          <xf numFmtId="0" xfId="1" applyNumberFormat="0"/>
        </cellXfs>
        """
    )
    #expect(catalog.cellFormats.map(\.numberFormatID) == [14, 0, 0])
    #expect(catalog.cellFormats.map(\.disposition) == [
        .dateOrTime, .provenNonDate, .provenNonDate,
    ])
}

@Test(arguments: [nil, "false", "0"] as [String?])
func xlsxStyleInheritanceRejectsAnUnappliedNumberFormatDifference(
    _ applyNumberFormat: String?
) {
    let apply = applyNumberFormat.map { " applyNumberFormat=\"\($0)\"" } ?? ""
    expectStyleIssue(
        .incoherentNumberFormatApplication,
        value: "cellXfs[0]",
        body: """
        <cellStyleXfs count="1"><xf numFmtId="14"/></cellStyleXfs>
        <cellXfs count="1"><xf numFmtId="0" xfId="0"\(apply)/></cellXfs>
        """
    )
}

@Test
func xlsxStyleParentReferencesAreStrictlyValidated() {
    expectStyleIssue(
        .styleParentMustNotReferenceParent,
        value: "0",
        body: """
        <cellStyleXfs count="1"><xf numFmtId="0" xfId="0"/></cellStyleXfs>
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        """
    )
    expectStyleIssue(
        .styleParentReferenceOutOfRange,
        value: "cellXfs[0].xfId=0",
        body: "<cellXfs count=\"1\"><xf numFmtId=\"0\" xfId=\"0\"/></cellXfs>"
    )
}

@Test(arguments: ["true", "false", "1", "0"])
func xlsxStyleBooleanLexemesAreExactAndComplete(_ value: String) throws {
    _ = try parseStyles(
        "<cellXfs count=\"1\"><xf numFmtId=\"0\" applyNumberFormat=\"\(value)\"/></cellXfs>"
    )
}

@Test(arguments: ["True", "FALSE", "yes", " 1"])
func xlsxStyleRejectsInvalidBooleanLexemes(_ value: String) {
    expectStyleIssue(
        .invalidBoolean,
        value: value,
        body: "<cellXfs count=\"1\"><xf numFmtId=\"0\" applyNumberFormat=\"\(value)\"/></cellXfs>"
    )
}

@Test
func xlsxStyleFormatCodeLengthUsesUTF16Boundaries() throws {
    let exact = "0" + String(repeating: " ", count: 253)
    let catalog = try parseStyles(customFormatDocument(code: exact))
    #expect(catalog.cellFormats[0].disposition == .provenNonDate)

    let oversized = exact + " "
    expectStyleIssue(
        .formatCodeTooLong,
        value: "164",
        body: customFormatDocument(code: oversized)
    )
}

@Test
func xlsxStyleRejectsReservedAndDuplicateNumberFormatDefinitions() {
    expectStyleIssue(
        .reservedNumberFormatRedefined,
        value: "14",
        body: """
        <numFmts count="1"><numFmt numFmtId="14" formatCode="0"/></numFmts>
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        """
    )
    expectStyleIssue(
        .duplicateNumberFormatID,
        value: "164",
        body: """
        <numFmts count="2">
          <numFmt numFmtId="164" formatCode="0"/>
          <numFmt numFmtId="164" formatCode="0.0"/>
        </numFmts>
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        """
    )
}

@Test
func xlsxStyleCustomFormatLimitIsEnforcedWhileStreaming() throws {
    let maximum = CanonicalXLSXStyleCatalogParser.maximumCustomNumberFormatCount
    let exactRecords = (0..<maximum).map {
        "<numFmt numFmtId=\"\(164 + $0)\" formatCode=\"0\"/>"
    }.joined()
    _ = try parseStyles(
        "<numFmts count=\"\(maximum)\">\(exactRecords)</numFmts>"
        + "<cellXfs count=\"1\"><xf numFmtId=\"164\"/></cellXfs>"
    )

    let oversizedRecords = exactRecords
        + "<numFmt numFmtId=\"\(164 + maximum)\" formatCode=\"0\"/>"
    expectStyleIssue(
        .formatRecordLimitExceeded,
        value: "numFmt",
        body: "<numFmts>\(oversizedRecords)</numFmts>"
            + "<cellXfs count=\"1\"><xf numFmtId=\"164\"/></cellXfs>"
    )
}

@Test
func xlsxStyleCombinedXFLimitIsEnforcedBeforeAppendingPastTheBoundary() throws {
    let maximum = CanonicalXLSXStyleCatalogParser.maximumCombinedXFCount
    let parentRecords = String(repeating: "<xf numFmtId=\"0\"/>", count: maximum - 1)
    _ = try parseStyles(
        "<cellStyleXfs count=\"\(maximum - 1)\">\(parentRecords)</cellStyleXfs>"
        + "<cellXfs count=\"1\"><xf numFmtId=\"0\"/></cellXfs>"
    )

    let fullParentRecords = parentRecords + "<xf numFmtId=\"0\"/>"
    expectStyleIssue(
        .formatRecordLimitExceeded,
        value: "xf",
        body: "<cellStyleXfs count=\"\(maximum)\">\(fullParentRecords)</cellStyleXfs>"
            + "<cellXfs count=\"1\"><xf numFmtId=\"0\"/></cellXfs>"
    )
}

@Test
func xlsxStyleCountsAndRequiredCellFormatsAreValidated() {
    expectStyleIssue(
        .declaredCountMismatch,
        value: "cellXfs:2:1",
        body: "<cellXfs count=\"2\"><xf numFmtId=\"0\"/></cellXfs>"
    )
    expectStyleIssue(.missingCellFormats, body: "<fonts count=\"0\"/>")
    expectStyleIssue(
        .missingRequiredAttribute,
        value: "xf.numFmtId",
        body: "<cellXfs count=\"1\"><xf/></cellXfs>"
    )
}

@Test
func xlsxStyleTopLevelOrderAndDuplicatesHaveDistinctDeterministicErrors() {
    expectStyleIssue(
        .duplicateSection,
        value: "cellXfs",
        body: """
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        """
    )
    expectStyleIssue(
        .invalidSectionOrder,
        value: "numFmts",
        body: """
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        <numFmts count="0"/>
        """
    )
}

@Test
func xlsxStyleSemanticXMLStructureFailsClosed() throws {
    expectStyleIssue(
        .unexpectedRoot,
        value: "http://purl.oclc.org/ooxml/spreadsheetml/main",
        body: "",
        rootNamespace: "http://purl.oclc.org/ooxml/spreadsheetml/main"
    )
    expectStyleIssue(
        .invalidElementStructure,
        value: "evil:xf",
        body: """
        <cellXfs count="1" xmlns:evil="urn:evil"><evil:xf numFmtId="0"/></cellXfs>
        """
    )
    expectStyleIssue(
        .invalidElementStructure,
        value: "child",
        body: """
        <cellXfs count="1"><xf numFmtId="0"><alignment><child/></alignment></xf></cellXfs>
        """
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: "extLst",
        body: """
        <cellXfs count="1"><xf numFmtId="0"><extLst/></xf></cellXfs>
        """
    )

    let catalog = try parseStyles(
        """
        <cellXfs count="1">
          <xf numFmtId="0"><alignment horizontal="center"/><protection locked="1"/></xf>
        </cellXfs>
        """
    )
    #expect(catalog.cellFormats[0].disposition == .provenNonDate)

    expectStyleIssue(
        .invalidElementStructure,
        value: "extension",
        body: """
        <cellXfs count="1"><xf numFmtId="0"><alignment extension="value"/></xf></cellXfs>
        """
    )
}

@Test
func xlsxStyleAllowsOnlyTheKnownTerminalPresentationExtensions() throws {
    let catalog = try parseStyles(
        """
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        <extLst>
          <ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
               uri="{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}">
            <x14:slicerStyles defaultSlicerStyle="SlicerStyleLight1"/>
          </ext>
          <ext xmlns:x15="http://schemas.microsoft.com/office/spreadsheetml/2010/11/main"
               uri="{9260A510-F301-46a8-8635-F512D64BE5F5}">
            <x15:timelineStyles defaultTimelineStyle="TimeSlicerStyleLight1"/>
          </ext>
        </extLst>
        """
    )
    #expect(catalog.cellFormats == [
        CanonicalXLSXCellFormat(numberFormatID: 0, disposition: .provenNonDate),
    ])

    expectStyleIssue(
        .unsupportedExtensionList,
        value: "{00000000-0000-0000-0000-000000000000}",
        body: """
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        <extLst><ext uri="{00000000-0000-0000-0000-000000000000}"/></extLst>
        """
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: "x14:unknown",
        body: """
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        <extLst>
          <ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
               uri="{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}"><x14:unknown/></ext>
        </extLst>
        """
    )
    expectStyleIssue(
        .invalidSectionOrder,
        value: "cellXfs",
        body: """
        <extLst>
          <ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
               uri="{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}">
            <x14:slicerStyles defaultSlicerStyle="SlicerStyleLight1"/>
          </ext>
        </extLst>
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        """
    )
}

@Test
func xlsxStyleRejectsMalformedOrSemanticPresentationExtensions() {
    let cellFormats = "<cellXfs count=\"1\"><xf numFmtId=\"0\"/></cellXfs>"
    let slicerURI = "{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}"
    let timelineURI = "{9260A510-F301-46a8-8635-F512D64BE5F5}"

    expectStyleIssue(
        .unsupportedExtensionList,
        value: "ext",
        body: cellFormats + "<extLst><ext uri=\"\(slicerURI)\"/></extLst>"
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: "x14:slicerStyles",
        body: cellFormats + """
        <extLst><ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
        uri="\(slicerURI)"><x14:slicerStyles/></ext></extLst>
        """
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: "x14:child",
        body: cellFormats + """
        <extLst><ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
        uri="\(slicerURI)"><x14:slicerStyles defaultSlicerStyle="S"><x14:child/>
        </x14:slicerStyles></ext></extLst>
        """
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: "x15:timelineStyles",
        body: cellFormats + """
        <extLst><ext xmlns:x15="http://schemas.microsoft.com/office/spreadsheetml/2010/11/main"
        uri="\(slicerURI)"><x15:timelineStyles defaultTimelineStyle="T"/></ext></extLst>
        """
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: slicerURI,
        body: cellFormats + """
        <extLst>
          <ext xmlns:x15="http://schemas.microsoft.com/office/spreadsheetml/2010/11/main"
          uri="\(timelineURI)"><x15:timelineStyles defaultTimelineStyle="T"/></ext>
          <ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
          uri="\(slicerURI)"><x14:slicerStyles defaultSlicerStyle="S"/></ext>
        </extLst>
        """
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: slicerURI,
        body: cellFormats + """
        <extLst>
          <ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
          uri="\(slicerURI)"><x14:slicerStyles defaultSlicerStyle="S"/></ext>
          <ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
          uri="\(slicerURI)"><x14:slicerStyles defaultSlicerStyle="S"/></ext>
        </extLst>
        """
    )
    expectStyleIssue(
        .unsupportedExtensionList,
        value: "x14:slicerStyles",
        body: cellFormats + """
        <extLst><ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
        uri="\(slicerURI)"><x14:slicerStyles defaultSlicerStyle="S" extra="x"/></ext></extLst>
        """
    )
}

@Test
func xlsxPresentationStyleNameUsesTheSchemaUnicodeScalarBoundary() throws {
    let slicerURI = "{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}"
    func document(styleName: String) -> String {
        """
        <cellXfs count="1"><xf numFmtId="0"/></cellXfs>
        <extLst><ext xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
        uri="\(slicerURI)"><x14:slicerStyles defaultSlicerStyle="\(styleName)"/></ext></extLst>
        """
    }

    _ = try parseStyles(document(styleName: String(repeating: "\u{1F9E0}", count: 255)))
    expectStyleIssue(
        .unsupportedExtensionList,
        value: "x14:slicerStyles",
        body: document(styleName: String(repeating: "\u{1F9E0}", count: 256))
    )
}

@Test
func xlsxStyleFormatCode16IsRejectedRegardlessOfPrefixOrStandardFallback() {
    expectStyleIssue(
        .formatCode16Unsupported,
        value: "numFmt",
        body: """
        <numFmts count="1" xmlns:x15="http://schemas.microsoft.com/office/spreadsheetml/2010/11/main">
          <numFmt numFmtId="164" formatCode="0" x15:formatCode16="0.000"/>
        </numFmts>
        <cellXfs count="1"><xf numFmtId="164"/></cellXfs>
        """
    )
}

@Test
func xlsxStyleOnlyAcceptsXMLWhitespaceBetweenElements() {
    expectStyleIssue(
        .unexpectedText,
        value: "\u{00A0}",
        body: "<cellXfs count=\"1\"><xf numFmtId=\"0\"/>\u{00A0}</cellXfs>"
    )
    expectStyleIssue(
        .invalidCDATA,
        body: "<cellXfs count=\"1\"><xf numFmtId=\"0\"/><![CDATA[ ]]></cellXfs>"
    )
}

@Test
func xlsxStyleUnknownAttributesProduceStableDiagnostics() {
    expectStyleIssue(
        .invalidElementStructure,
        value: "alpha",
        body: """
        <numFmts count="1">
          <numFmt numFmtId="164" formatCode="0" zeta="z" alpha="a"/>
        </numFmts>
        <cellXfs count="1"><xf numFmtId="164"/></cellXfs>
        """
    )
}

@Test
func xlsxStyleAllowsBoundIgnorablePrefixesButRejectsAnUnboundDeclaration() throws {
    _ = try parseStyles(
        "<cellXfs count=\"1\"><xf numFmtId=\"0\"/></cellXfs>",
        rootAttributes: """
         xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:x14="urn:x14" mc:Ignorable="x14"
        """
    )
    _ = try parseStyles(
        "<cellXfs count=\"1\"><xf numFmtId=\"0\"/></cellXfs>",
        rootAttributes: """
         xmlns:compat="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:x14="urn:x14" compat:Ignorable="x14"
        """
    )
    expectStyleIssue(
        .invalidElementStructure,
        value: "missing",
        body: "<cellXfs count=\"1\"><xf numFmtId=\"0\"/></cellXfs>",
        rootAttributes: """
         xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="missing"
        """
    )
}
