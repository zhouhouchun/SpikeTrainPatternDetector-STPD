import Foundation
import Testing
@testable import STPDTabularIO

private enum BoundedXMLHookTestError: Error, Equatable {
    case characters
    case cdata
}

private final class ThrowingCharactersDelegate: BoundedXMLDelegate {
    private(set) var observed: [String] = []

    override func handleCharacters(_ string: String) throws {
        observed.append(string)
        throw BoundedXMLHookTestError.characters
    }
}

private final class ThrowingCDATADelegate: BoundedXMLDelegate {
    override func handleCDATA(_ data: Data) throws {
        throw BoundedXMLHookTestError.cdata
    }
}

private final class RecordingCharactersDelegate: BoundedXMLDelegate {
    private(set) var observed: [String] = []

    override func handleCharacters(_ string: String) throws {
        observed.append(string)
    }
}

private final class ExpandedCharacterBudgetDelegate: BoundedXMLDelegate {
    private(set) var observed: [String] = []

    override var maximumElementCharacterDataUTF8ByteCount: Int { 7 }

    override func handleCharacters(_ string: String) throws {
        observed.append(string)
    }

    override func handleCDATA(_ data: Data) throws {
        observed.append(String(decoding: data, as: UTF8.self))
    }
}

private func xmlLimits(maximumTextUTF8Bytes: Int) -> CanonicalXLSXInspectionLimits {
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
        maximumXMLTextUTF8ByteCount: maximumTextUTF8Bytes
    )
}

@Test
func boundedXMLCharacterHookPropagatesItsExactTypedError() throws {
    let part = "worksheet.xml"
    let delegate = ThrowingCharactersDelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 16)
    )

    do {
        try parseXML(
            data: Data("<root>payload</root>".utf8),
            delegate: delegate
        )
        Issue.record("Expected the character hook error")
    } catch let error as BoundedXMLHookTestError {
        #expect(error == .characters)
    }
    #expect(delegate.observed == ["payload"])
}

@Test
func boundedXMLTextBudgetFailsBeforeDeliveringOversizedCharacters() throws {
    let part = "worksheet.xml"
    let delegate = RecordingCharactersDelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 3)
    )

    do {
        try parseXML(
            data: Data("<root>four</root>".utf8),
            delegate: delegate
        )
        Issue.record("Expected the XML text budget error")
    } catch let error as CanonicalXLSXWorkbookInspectionError {
        #expect(error == .xmlLimitExceeded(
            part: part,
            kind: .xmlTextUTF8Bytes,
            maximum: 3,
            actual: 4
        ))
    }
    #expect(delegate.observed.isEmpty)
}

@Test
func boundedXMLCDATAHookPropagatesItsExactTypedError() throws {
    let part = "shared-strings.xml"
    let delegate = ThrowingCDATADelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 16)
    )

    do {
        try parseXML(
            data: Data("<root><![CDATA[payload]]></root>".utf8),
            delegate: delegate
        )
        Issue.record("Expected the CDATA hook error")
    } catch let error as BoundedXMLHookTestError {
        #expect(error == .cdata)
    }
}

@Test
func boundedXMLDelegateIsTheOnlyPartAndLimitAuthority() throws {
    let part = "authoritative-part.xml"
    let delegate = RecordingCharactersDelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 16)
    )

    do {
        try parseXML(
            data: Data("<!DOCTYPE root><root/>".utf8),
            delegate: delegate
        )
        Issue.record("Expected the XML security error")
    } catch let error as CanonicalXLSXWorkbookInspectionError {
        #expect(error == .xmlSecurityViolation(
            part: part,
            issue: .documentTypeDeclaration
        ))
    }
}

@Test
func boundedXMLCharacterBudgetCanExpandWithoutExpandingMetadataBudget() throws {
    let part = "shared-strings.xml"
    let characterDelegate = ExpandedCharacterBudgetDelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 3)
    )
    try parseXML(
        data: Data("<r>payload</r>".utf8),
        delegate: characterDelegate
    )
    #expect(characterDelegate.observed == ["payload"])

    let oversizedCharacterDelegate = ExpandedCharacterBudgetDelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 3)
    )
    do {
        try parseXML(
            data: Data("<r>payloadx</r>".utf8),
            delegate: oversizedCharacterDelegate
        )
        Issue.record("Expected the expanded character-data budget")
    } catch let error as CanonicalXLSXWorkbookInspectionError {
        #expect(error == .xmlLimitExceeded(
            part: part,
            kind: .xmlTextUTF8Bytes,
            maximum: 7,
            actual: 8
        ))
    }
    #expect(oversizedCharacterDelegate.observed.isEmpty)

    let mixedDelegate = ExpandedCharacterBudgetDelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 3)
    )
    try parseXML(
        data: Data("<r>pay<![CDATA[load]]></r>".utf8),
        delegate: mixedDelegate
    )
    #expect(mixedDelegate.observed == ["pay", "load"])

    let oversizedMixedDelegate = ExpandedCharacterBudgetDelegate(
        part: part,
        limits: xmlLimits(maximumTextUTF8Bytes: 3)
    )
    do {
        try parseXML(
            data: Data("<r>pay<![CDATA[loadx]]></r>".utf8),
            delegate: oversizedMixedDelegate
        )
        Issue.record("Expected the cumulative character/CDATA budget")
    } catch let error as CanonicalXLSXWorkbookInspectionError {
        #expect(error == .xmlLimitExceeded(
            part: part,
            kind: .xmlTextUTF8Bytes,
            maximum: 7,
            actual: 8
        ))
    }
    #expect(oversizedMixedDelegate.observed == ["pay"])

    let oversizedMetadata: [(source: String, actual: Int)] = [
        ("<root/>", 4),
        ("<r a=\"four\"/>", 4),
        ("<r xmlns:p=\"four\"/>", 4),
        ("<r><!--four--></r>", 4),
        ("<r><?long?></r>", 4),
    ]
    for example in oversizedMetadata {
        let metadataDelegate = ExpandedCharacterBudgetDelegate(
            part: part,
            limits: xmlLimits(maximumTextUTF8Bytes: 3)
        )
        do {
            try parseXML(
                data: Data(example.source.utf8),
                delegate: metadataDelegate
            )
            Issue.record("Expected the unchanged XML metadata budget")
        } catch let error as CanonicalXLSXWorkbookInspectionError {
            #expect(error == .xmlLimitExceeded(
                part: part,
                kind: .xmlTextUTF8Bytes,
                maximum: 3,
                actual: example.actual
            ))
        }
    }
}
