import Foundation
import Testing
@testable import STPDTabularIO

@Test
func xlsxStringDecoderPreservesOrdinaryUnicodeAndWhitespace() throws {
    let source = "  ASCII 神经 cafe\u{301} 😀\t\n  "
    #expect(try CanonicalXLSXStringDecoder.decodeTextNode(source) == source)
}

@Test
func xlsxStringDecoderAcceptsOnlyXMLForbiddenBMPAndCarriageReturnEscapes() throws {
    let source = "_x0000__x0008__x000b__x000C__x000e__x001f__xFFFE__xFFFF__x000D_"
    let expectedScalars: [Unicode.Scalar] = [
        Unicode.Scalar(0x0000)!, Unicode.Scalar(0x0008)!, Unicode.Scalar(0x000B)!,
        Unicode.Scalar(0x000C)!, Unicode.Scalar(0x000E)!, Unicode.Scalar(0x001F)!,
        Unicode.Scalar(0xFFFE)!, Unicode.Scalar(0xFFFF)!, Unicode.Scalar(0x000D)!,
    ]
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNode(source)
            == String(String.UnicodeScalarView(expectedScalars))
    )
}

@Test
func xlsxStringDecoderProtectsLiteralTokenWithoutRecursiveDecoding() throws {
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNode("_x005F_x0008_") == "_x0008_"
    )
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNode("_x005f_xAbCd_") == "_xAbCd_"
    )
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNode("_x005F_x005F_x0041_")
            == "_x005F_x0041_"
    )
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNode("_x005F_x0008__x0008_")
            == "_x0008_\u{0008}"
    )
}

@Test
func malformedTokenShapesRemainLiteralText() throws {
    for source in ["_X0008_", "_x12G4_", "_x123_", "_x12345_"] {
        #expect(try CanonicalXLSXStringDecoder.decodeTextNode(source) == source)
    }
}

@Test
func xlsxStringDecoderRejectsValidXMLScalarEscapesAndElementTextControls() {
    for codeUnit: UInt16 in [
        0x0041, 0x0031, 0x0009, 0x000A, 0x007F, 0xFFFD,
        0x0040, 0x003D, 0x002B, 0x002D, 0x002E, 0x0065, 0x0045,
        0x0020, 0xD7FF, 0xE000,
    ] {
        let source = String(format: "_x%04X_", codeUnit)
        #expect(throws: CanonicalXLSXStringDecodingError.unsupportedEscape(
            .nonCanonicalEscape(codeUnit: codeUnit)
        )) {
            try CanonicalXLSXStringDecoder.decodeTextNode(source)
        }
    }
}

@Test
func xlsxStringDecoderRejectsMisplacedUnderscoreProtection() {
    for source in ["_x005F_", "_x005F_x12G4_", "_x005F__x0008_"] {
        #expect(throws: CanonicalXLSXStringDecodingError.unsupportedEscape(
            .misplacedLiteralUnderscoreEscape
        )) {
            try CanonicalXLSXStringDecoder.decodeTextNode(source)
        }
    }
}

@Test
func xlsxStringDecoderRejectsEverySurrogateTokenIncludingAnAdjacentPair() {
    for (source, firstCodeUnit): (String, UInt16) in [
        ("_xD800_", 0xD800),
        ("_xDBFF_", 0xDBFF),
        ("_xDC00_", 0xDC00),
        ("_xDFFF_", 0xDFFF),
        ("_xD83D_", 0xD83D),
        ("_xDE00_", 0xDE00),
        ("_xDE00__xD83D_", 0xDE00),
        ("_xD83D__xDE00_", 0xD83D),
    ] {
        #expect(throws: CanonicalXLSXStringDecodingError.unsupportedEscape(
            .surrogateEscapeUnsupported(codeUnit: firstCodeUnit)
        )) {
            try CanonicalXLSXStringDecoder.decodeTextNode(source)
        }
    }
}

@Test
func xlsxStringDecoderRejectsDirectCarriageReturn() {
    #expect(throws: CanonicalXLSXStringDecodingError.unsupportedEscape(
        .directCarriageReturnUnsupported
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNode("before\rafter")
    }
}

@Test
func independentTextNodesCannotAssembleAnEscapeAcrossRichTextRuns() throws {
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNodes(["_x00", "08_"]) == "_x0008_"
    )
}

@Test
func independentTextNodesEnforceTheAggregateDecodedCellLimit() throws {
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNodes(
            ["ab", "c"],
            maximumDecodedCellUTF8ByteCount: 3
        ) == "abc"
    )
    #expect(throws: CanonicalXLSXStringDecodingError.decodedTextLimitExceeded(
        maximum: 3,
        actual: 4
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNodes(
            ["ab", "cd"],
            maximumDecodedCellUTF8ByteCount: 3
        )
    }
    #expect(throws: CanonicalXLSXStringDecodingError.decodedTextLimitExceeded(
        maximum: 3,
        actual: 4
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNodes(
            ["€", "a"],
            maximumDecodedCellUTF8ByteCount: 3
        )
    }
}

@Test
func xlsxStringDecoderEnforcesExactRawAndDecodedBoundaries() throws {
    let decodedMaximum =
        CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount
    let rawMaximum = CanonicalXLSXStringDecoder.supportedMaximumRawTextNodeUTF8ByteCount
    #expect(rawMaximum == 460_558)

    let exactDecoded = String(repeating: "a", count: decodedMaximum)
    #expect(try CanonicalXLSXStringDecoder.decodeTextNode(exactDecoded) == exactDecoded)
    #expect(throws: CanonicalXLSXStringDecodingError.decodedTextLimitExceeded(
        maximum: decodedMaximum,
        actual: decodedMaximum + 1
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNode(exactDecoded + "a")
    }

    let exactRaw = String(repeating: "_x0000_", count: decodedMaximum)
    let decoded = try CanonicalXLSXStringDecoder.decodeTextNode(exactRaw)
    #expect(decoded.utf8.count == decodedMaximum)
    #expect(throws: CanonicalXLSXStringDecodingError.rawTextNodeLimitExceeded(
        maximum: rawMaximum,
        actual: rawMaximum + 7
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNode(exactRaw + "_x0000_")
    }
}

@Test
func xlsxStringDecoderRejectsNonpositiveLimits() {
    #expect(throws: CanonicalXLSXStringDecodingError.invalidLimit(
        kind: .rawTextNodeUTF8Bytes,
        actual: 0
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNode("", maximumRawUTF8ByteCount: 0)
    }
    #expect(throws: CanonicalXLSXStringDecodingError.invalidLimit(
        kind: .decodedTextUTF8Bytes,
        actual: 0
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNode("", maximumDecodedUTF8ByteCount: 0)
    }
    #expect(throws: CanonicalXLSXStringDecodingError.invalidLimit(
        kind: .rawTextNodeUTF8Bytes,
        actual: 0
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNodes(
            [],
            maximumRawTextNodeUTF8ByteCount: 0
        )
    }
    #expect(throws: CanonicalXLSXStringDecodingError.invalidLimit(
        kind: .decodedTextUTF8Bytes,
        actual: 0
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNodes(
            [],
            maximumDecodedCellUTF8ByteCount: 0
        )
    }

    let rawMaximum = CanonicalXLSXStringDecoder.supportedMaximumRawTextNodeUTF8ByteCount
    for actual in [rawMaximum + 1, Int.max] {
        #expect(throws: CanonicalXLSXStringDecodingError.limitExceedsSupportedEnvelope(
            kind: .rawTextNodeUTF8Bytes,
            maximumSupported: rawMaximum,
            actual: actual
        )) {
            try CanonicalXLSXStringDecoder.decodeTextNode(
                "",
                maximumRawUTF8ByteCount: actual
            )
        }
        #expect(throws: CanonicalXLSXStringDecodingError.limitExceedsSupportedEnvelope(
            kind: .rawTextNodeUTF8Bytes,
            maximumSupported: rawMaximum,
            actual: actual
        )) {
            try CanonicalXLSXStringDecoder.decodeTextNodes(
                [],
                maximumRawTextNodeUTF8ByteCount: actual
            )
        }
    }
    let decodedMaximum =
        CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount
    for actual in [decodedMaximum + 1, Int.max] {
        #expect(throws: CanonicalXLSXStringDecodingError.limitExceedsSupportedEnvelope(
            kind: .decodedTextUTF8Bytes,
            maximumSupported: decodedMaximum,
            actual: actual
        )) {
            try CanonicalXLSXStringDecoder.decodeTextNode(
                "",
                maximumDecodedUTF8ByteCount: actual
            )
        }
        #expect(throws: CanonicalXLSXStringDecodingError.limitExceedsSupportedEnvelope(
            kind: .decodedTextUTF8Bytes,
            maximumSupported: decodedMaximum,
            actual: actual
        )) {
            try CanonicalXLSXStringDecoder.decodeTextNodes(
                [],
                maximumDecodedCellUTF8ByteCount: actual
            )
        }
    }
}

@Test
func xlsxStringDecoderCountsMultibyteUTF8AtExactBoundaries() throws {
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNode(
            "€",
            maximumRawUTF8ByteCount: 3,
            maximumDecodedUTF8ByteCount: 3
        ) == "€"
    )
    #expect(
        try CanonicalXLSXStringDecoder.decodeTextNode(
            "😀",
            maximumRawUTF8ByteCount: 4,
            maximumDecodedUTF8ByteCount: 4
        ) == "😀"
    )
    #expect(throws: CanonicalXLSXStringDecodingError.rawTextNodeLimitExceeded(
        maximum: 3,
        actual: 4
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNode(
            "😀",
            maximumRawUTF8ByteCount: 3,
            maximumDecodedUTF8ByteCount: 4
        )
    }
    #expect(throws: CanonicalXLSXStringDecodingError.decodedTextLimitExceeded(
        maximum: 3,
        actual: 4
    )) {
        try CanonicalXLSXStringDecoder.decodeTextNode(
            "😀",
            maximumRawUTF8ByteCount: 4,
            maximumDecodedUTF8ByteCount: 3
        )
    }
}
