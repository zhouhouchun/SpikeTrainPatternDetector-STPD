import Foundation
import Testing
@testable import STPDTabularIO

private let sharedStringsPart = "xl/sharedStrings.xml"
private let spreadsheetNamespace =
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
private let markupCompatibilityNamespace =
    "http://schemas.openxmlformats.org/markup-compatibility/2006"

private func sharedStringsDocument(
    _ body: String,
    rootNamespace: String = spreadsheetNamespace,
    rootAttributes: String = ""
) -> Data {
    Data(
        """
        <sst xmlns="\(rootNamespace)"\(rootAttributes)>\(body)</sst>
        """.utf8
    )
}

private func sharedStringLimits(
    items: Int = 32,
    decoded: Int = 256,
    total: Int = 1_024
) -> CanonicalXLSXStringParsingLimits {
    CanonicalXLSXStringParsingLimits(
        maximumSharedStringItemCount: items,
        maximumDecodedStringUTF8ByteCount: decoded,
        maximumTotalDecodedSharedStringUTF8ByteCount: total
    )
}

private func sharedStringXMLLimits(
    text: Int = CanonicalXLSXInspectionLimits.supportedDatasetEnvelope
        .maximumXMLTextUTF8ByteCount
) -> CanonicalXLSXInspectionLimits {
    let supported = CanonicalXLSXInspectionLimits.supportedDatasetEnvelope
    return CanonicalXLSXInspectionLimits(
        maximumSourceByteCount: supported.maximumSourceByteCount,
        maximumZIPEntryCount: supported.maximumZIPEntryCount,
        maximumArchivePathUTF8ByteCount: supported.maximumArchivePathUTF8ByteCount,
        maximumPartUncompressedByteCount: supported.maximumPartUncompressedByteCount,
        maximumTotalUncompressedByteCount: supported.maximumTotalUncompressedByteCount,
        maximumCompressionRatio: supported.maximumCompressionRatio,
        maximumWorksheetCount: supported.maximumWorksheetCount,
        maximumContentTypeDeclarationCount: supported.maximumContentTypeDeclarationCount,
        maximumRelationshipCountPerPart: supported.maximumRelationshipCountPerPart,
        maximumXMLDepth: supported.maximumXMLDepth,
        maximumXMLAttributeCountPerElement: supported.maximumXMLAttributeCountPerElement,
        maximumXMLElementCountPerPart: supported.maximumXMLElementCountPerPart,
        maximumXMLTextUTF8ByteCount: text
    )
}

private func parseSharedStrings(
    _ body: String,
    rootNamespace: String = spreadsheetNamespace,
    rootAttributes: String = "",
    xmlLimits: CanonicalXLSXInspectionLimits = .supportedDatasetEnvelope,
    stringLimits: CanonicalXLSXStringParsingLimits = sharedStringLimits()
) throws -> CanonicalXLSXSharedStringTable {
    try CanonicalXLSXSharedStringTableParser.parse(
        data: sharedStringsDocument(
            body,
            rootNamespace: rootNamespace,
            rootAttributes: rootAttributes
        ),
        part: sharedStringsPart,
        xmlLimits: xmlLimits,
        stringLimits: stringLimits
    )
}

private func expectSharedStringError(
    _ expected: CanonicalXLSXSharedStringParsingError,
    body: String,
    rootNamespace: String = spreadsheetNamespace,
    rootAttributes: String = "",
    xmlLimits: CanonicalXLSXInspectionLimits = .supportedDatasetEnvelope,
    stringLimits: CanonicalXLSXStringParsingLimits = sharedStringLimits()
) {
    do {
        _ = try parseSharedStrings(
            body,
            rootNamespace: rootNamespace,
            rootAttributes: rootAttributes,
            xmlLimits: xmlLimits,
            stringLimits: stringLimits
        )
        Issue.record("Expected shared-string parsing error: \(expected)")
    } catch let error as CanonicalXLSXSharedStringParsingError {
        #expect(error == expected)
    } catch {
        Issue.record("Unexpected error type: \(error)")
    }
}

@Test
func canonicalXLSXSharedStringsPreserveOrderDuplicatesAndExplicitEmptyText() throws {
    let table = try parseSharedStrings(
        """
        <si><t>alpha</t></si>
        <si><t/></si>
        <si><t>alpha</t></si>
        """,
        rootAttributes: " count=\"7\" uniqueCount=\"3\""
    )

    #expect(table.values == ["alpha", "", "alpha"])
    #expect(table.declaredCount == 7)
    #expect(table.declaredUniqueCount == 3)
    #expect(try table.value(at: 0) == "alpha")
    #expect(try table.value(at: 1).isEmpty)
    #expect(throws: CanonicalXLSXSharedStringResolutionError.indexOutOfBounds(
        index: 3,
        count: 3
    )) {
        try table.value(at: 3)
    }
}

@Test
func canonicalXLSXSharedStringsAcceptEmptyTableWithOrWithoutPairedCounts() throws {
    let absent = try parseSharedStrings("")
    #expect(absent.values.isEmpty)
    #expect(absent.declaredCount == nil)
    #expect(absent.declaredUniqueCount == nil)

    let zero = try parseSharedStrings("", rootAttributes: " count=\"0\" uniqueCount=\"0\"")
    #expect(zero.values.isEmpty)
    #expect(zero.declaredCount == 0)
    #expect(zero.declaredUniqueCount == 0)
}

@Test
func canonicalXLSXSharedStringsPreserveUnicodeAndXMLSpaceExactly() throws {
    let table = try parseSharedStrings(
        """
        <si><t>神经元 α</t></si>
        <si><t xml:space="default">  a\tb\n  </t></si>
        <si><t xml:space="preserve">  a\tb\n  </t></si>
        """,
        rootAttributes: " count=\"3\" uniqueCount=\"3\""
    )

    #expect(table.values == ["神经元 α", "  a\tb\n  ", "  a\tb\n  "])
}

@Test
func canonicalXLSXSharedStringsFlattenRichTextButDiscardFormatting() throws {
    let table = try parseSharedStrings(
        """
        <si>
          <r><rPr><b/><color rgb="FF000000"/></rPr><t>unit_</t></r>
          <r><rPr><i val="1"/></rPr><t>A</t></r>
        </si>
        """,
        rootAttributes: " count=\"1\" uniqueCount=\"1\""
    )

    #expect(table.values == ["unit_A"])
}

@Test
func canonicalXLSXSharedStringsValidateButDiscardPhoneticPresentation() throws {
    let table = try parseSharedStrings(
        """
        <si>
          <t>刺激</t>
          <rPh sb="0" eb="1"><t>しげき</t></rPh>
          <phoneticPr fontId="0" type="Hiragana" alignment="center"/>
        </si>
        """,
        rootAttributes: " count=\"1\" uniqueCount=\"1\""
    )

    #expect(table.values == ["刺激"])
}

@Test
func canonicalXLSXSharedStringsDecodeOneTextNodeAcrossXMLCallbacks() throws {
    let backspace = String(Unicode.Scalar(0x08)!)
    let table = try parseSharedStrings(
        """
        <si><t>_x00<![CDATA[08]]>_</t></si>
        <si><t>_x00<!-- split --><?split data?>08_</t></si>
        """,
        rootAttributes: " count=\"2\" uniqueCount=\"2\""
    )

    #expect(table.values == [backspace, backspace])
}

@Test
func canonicalXLSXSharedStringsNeverAssembleEscapesAcrossRichTextNodes() throws {
    let table = try parseSharedStrings(
        """
        <si><r><t>_x00</t></r><r><t>08_</t></r></si>
        """,
        rootAttributes: " count=\"1\" uniqueCount=\"1\""
    )

    #expect(table.values == ["_x0008_"])
}

@Test
func canonicalXLSXSharedStringsUseTheConfirmedStrictBaseRepresentationSubset() {
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .mixedPlainAndRich,
            value: nil
        ),
        body: "<si><t>a</t><r><t>b</t></r></si>"
    )
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .missingBaseText,
            value: nil
        ),
        body: "<si/>"
    )
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .missingBaseText,
            value: nil
        ),
        body: "<si><rPh sb=\"0\" eb=\"1\"><t>x</t></rPh></si>"
    )
}

@Test
func canonicalXLSXSharedStringsRejectIncompleteOrOutOfOrderRichAndPhoneticContent() {
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .missingRunText,
            value: nil
        ),
        body: "<si><r/></si>"
    )
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .invalidElementStructure,
            value: "r"
        ),
        body: "<si><t>x</t><rPh sb=\"0\" eb=\"1\"><t>y</t></rPh><r><t>z</t></r></si>"
    )
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .invalidElementStructure,
            value: "phoneticPr"
        ),
        body: """
        <si><t>x</t><phoneticPr fontId="0"/><phoneticPr fontId="0"/></si>
        """
    )
}

@Test
func canonicalXLSXSharedStringsRejectInvalidTextAndPresentationAttributes() {
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .invalidXMLSpace,
            value: "trim"
        ),
        body: "<si><t xml:space=\"trim\">x</t></si>"
    )
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .invalidPhoneticRun,
            value: "2:2"
        ),
        body: "<si><t>x</t><rPh sb=\"2\" eb=\"2\"><t>y</t></rPh></si>"
    )
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .invalidPhoneticProperties,
            value: "sideways"
        ),
        body: "<si><t>x</t><phoneticPr fontId=\"0\" alignment=\"sideways\"/></si>"
    )
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .unexpectedText,
            value: nil
        ),
        body: "<si><r><rPr><b> </b></rPr><t>x</t></r></si>"
    )
}

@Test
func canonicalXLSXSharedStringsValidateDiscardedPhoneticTextEscapes() {
    expectSharedStringError(
        .invalidStringText(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            textNodeIndex: 1,
            error: .unsupportedEscape(.nonCanonicalEscape(codeUnit: 0x0041))
        ),
        body: "<si><t>x</t><rPh sb=\"0\" eb=\"1\"><t>_x0041_</t></rPh></si>"
    )
}

@Test
func canonicalXLSXSharedStringsRejectUnexpectedRootStructureTextAndCDATA() {
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .unexpectedRoot,
            value: "urn:wrong"
        ),
        body: "",
        rootNamespace: "urn:wrong"
    )
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .unsupportedExtensionList,
            value: "extLst"
        ),
        body: "<extLst/>"
    )
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .unexpectedText,
            value: nil
        ),
        body: "not-whitespace"
    )
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .invalidCDATA,
            value: nil
        ),
        body: "<![CDATA[x]]>"
    )
}

@Test
func canonicalXLSXSharedStringsAcceptBoundMarkupCompatibilityPrefixOnly() throws {
    let table = try parseSharedStrings(
        "<si><t>x</t></si>",
        rootAttributes: """
         xmlns:compat="\(markupCompatibilityNamespace)"
         xmlns:future="urn:future"
         compat:Ignorable="future"
        """
    )
    #expect(table.values == ["x"])

    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .invalidElementStructure,
            value: "compat:Ignorable"
        ),
        body: "<si><t>x</t></si>",
        rootAttributes: " xmlns:compat=\"urn:not-mc\" compat:Ignorable=\"future\""
    )
}

@Test
func canonicalXLSXSharedStringsEnforcePairedAndCoherentCounts() {
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .pairedCountsRequired,
            value: nil
        ),
        body: "",
        rootAttributes: " count=\"0\""
    )
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .invalidUnsignedInteger,
            value: "4294967296"
        ),
        body: "",
        rootAttributes: " count=\"4294967296\" uniqueCount=\"0\""
    )
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .countLessThanUniqueCount,
            value: "0:1"
        ),
        body: "",
        rootAttributes: " count=\"0\" uniqueCount=\"1\""
    )
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .declaredUniqueCountMismatch,
            value: "2:1"
        ),
        body: "<si><t>x</t></si>",
        rootAttributes: " count=\"2\" uniqueCount=\"2\""
    )
}

@Test
func canonicalXLSXSharedStringsDoNotReserveFromUntrustedReferenceCount() throws {
    let table = try parseSharedStrings(
        "<si><t>x</t></si>",
        rootAttributes: " count=\"4294967295\" uniqueCount=\"1\""
    )
    #expect(table.values == ["x"])
    #expect(table.declaredCount == UInt32.max)
}

@Test
func canonicalXLSXSharedStringsEnforceItemCountBeforeMaterializingExtraItems() {
    let limits = sharedStringLimits(items: 1)
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .itemLimitExceeded,
            value: "1"
        ),
        body: "<si><t>a</t></si><si><t>b</t></si>",
        stringLimits: limits
    )
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .itemLimitExceeded,
            value: "2"
        ),
        body: "",
        rootAttributes: " count=\"2\" uniqueCount=\"2\"",
        stringLimits: limits
    )
}

@Test
func canonicalXLSXSharedStringsEnforceDecodedItemAggregateAndTableLimits() throws {
    let itemLimits = sharedStringLimits(decoded: 3, total: 8)
    let exactItem = try parseSharedStrings(
        "<si><r><t>a</t></r><r><t>bc</t></r></si>",
        stringLimits: itemLimits
    )
    #expect(exactItem.values == ["abc"])
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            issue: .decodedItemLimitExceeded,
            value: "3:4"
        ),
        body: "<si><r><t>ab</t></r><r><t>cd</t></r></si>",
        stringLimits: itemLimits
    )

    let tableLimits = sharedStringLimits(decoded: 3, total: 3)
    let exactTable = try parseSharedStrings(
        "<si><t>a</t></si><si><t>bc</t></si>",
        stringLimits: tableLimits
    )
    #expect(exactTable.values == ["a", "bc"])
    expectSharedStringError(
        .invalidSharedStrings(
            part: sharedStringsPart,
            issue: .totalDecodedLimitExceeded,
            value: "3:4"
        ),
        body: "<si><t>ab</t></si><si><t>cd</t></si>",
        stringLimits: tableLimits
    )
}

@Test
func canonicalXLSXSharedStringsEnforceRawAndDecodedTextNodeLimitsAtExactBoundaries() throws {
    let limits = sharedStringLimits(decoded: 2, total: 8)
    let backspace = String(Unicode.Scalar(0x08)!)
    let exactRaw = try parseSharedStrings(
        "<si><t>_x0008__x0008_</t></si>",
        stringLimits: limits
    )
    #expect(exactRaw.values == [backspace + backspace])

    do {
        _ = try parseSharedStrings(
            "<si><t>_x0008__x0008__x0008_</t></si>",
            stringLimits: limits
        )
        Issue.record("Expected the raw text-node limit")
    } catch let error as CanonicalXLSXWorkbookInspectionError {
        #expect(error == .xmlLimitExceeded(
            part: sharedStringsPart,
            kind: .xmlTextUTF8Bytes,
            maximum: 14,
            actual: 21
        ))
    }

    expectSharedStringError(
        .invalidStringText(
            part: sharedStringsPart,
            context: .sharedString(index: 0),
            textNodeIndex: 0,
            error: .decodedTextLimitExceeded(maximum: 2, actual: 3)
        ),
        body: "<si><t>abc</t></si>",
        stringLimits: limits
    )
}

@Test
func canonicalXLSXSharedStringsExpandOnlyActiveTextCharacterBudget() throws {
    let xmlLimits = sharedStringXMLLimits(text: 100)
    let stringLimits = sharedStringLimits(decoded: 101, total: 200)
    let value = String(repeating: "x", count: 101)
    let table = try parseSharedStrings(
        "<si><t>\(value)</t></si>",
        xmlLimits: xmlLimits,
        stringLimits: stringLimits
    )
    #expect(table.values == [value])

    do {
        _ = try parseSharedStrings(
            "<si><t>x</t></si>\(String(repeating: " ", count: 101))",
            xmlLimits: xmlLimits,
            stringLimits: stringLimits
        )
        Issue.record("Expected the metadata character-data limit")
    } catch let error as CanonicalXLSXWorkbookInspectionError {
        #expect(error == .xmlLimitExceeded(
            part: sharedStringsPart,
            kind: .xmlTextUTF8Bytes,
            maximum: 100,
            actual: 101
        ))
    }
}

@Test
func canonicalXLSXSharedStringsValidateEveryCallerControlledLimitBeforeParsing() {
    let supported = CanonicalXLSXStringParsingLimits.supportedDatasetEnvelope
    let invalid: [(CanonicalXLSXStringParsingLimitKind, CanonicalXLSXStringParsingLimits)] = [
        (.sharedStringItems, sharedStringLimits(items: 0)),
        (.decodedStringUTF8Bytes, sharedStringLimits(decoded: 0)),
        (.totalDecodedSharedStringUTF8Bytes, sharedStringLimits(total: 0)),
    ]
    for (kind, limits) in invalid {
        expectSharedStringError(
            .invalidLimit(kind: kind, actual: 0),
            body: "",
            stringLimits: limits
        )
    }

    let excessive: [(CanonicalXLSXStringParsingLimitKind, Int, CanonicalXLSXStringParsingLimits)] = [
        (
            .sharedStringItems,
            supported.maximumSharedStringItemCount,
            CanonicalXLSXStringParsingLimits(
                maximumSharedStringItemCount: supported.maximumSharedStringItemCount + 1,
                maximumDecodedStringUTF8ByteCount: 1,
                maximumTotalDecodedSharedStringUTF8ByteCount: 1
            )
        ),
        (
            .decodedStringUTF8Bytes,
            supported.maximumDecodedStringUTF8ByteCount,
            CanonicalXLSXStringParsingLimits(
                maximumSharedStringItemCount: 1,
                maximumDecodedStringUTF8ByteCount:
                    supported.maximumDecodedStringUTF8ByteCount + 1,
                maximumTotalDecodedSharedStringUTF8ByteCount: 1
            )
        ),
        (
            .totalDecodedSharedStringUTF8Bytes,
            supported.maximumTotalDecodedSharedStringUTF8ByteCount,
            CanonicalXLSXStringParsingLimits(
                maximumSharedStringItemCount: 1,
                maximumDecodedStringUTF8ByteCount: 1,
                maximumTotalDecodedSharedStringUTF8ByteCount:
                    supported.maximumTotalDecodedSharedStringUTF8ByteCount + 1
            )
        ),
    ]
    for (kind, maximum, limits) in excessive {
        expectSharedStringError(
            .limitExceedsSupportedEnvelope(
                kind: kind,
                maximumSupported: maximum,
                actual: maximum + 1
            ),
            body: "",
            stringLimits: limits
        )
    }
}

@Test
func canonicalXLSXSharedStringsReturnNothingWhenALateItemFails() {
    expectSharedStringError(
        .invalidStringItem(
            part: sharedStringsPart,
            context: .sharedString(index: 1),
            issue: .missingRunText,
            value: nil
        ),
        body: "<si><t>valid</t></si><si><r/></si>"
    )
}
