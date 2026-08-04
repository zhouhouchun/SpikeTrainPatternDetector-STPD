import Foundation
import STPDCore
import Testing

@Test
func scientificImportScalarModelSemanticIDsUseNFCWithoutCaseOrWhitespaceRewriting() throws {
    let composed = try ScientificSemanticID(validating: "caf\u{00E9}")
    let decomposed = try ScientificSemanticID(validating: "cafe\u{0301}")
    #expect(composed == decomposed)
    #expect(composed.canonicalText == "caf\u{00E9}")

    #expect(try ScientificSemanticID(validating: "Unit") != ScientificSemanticID(validating: "unit"))
    #expect(try ScientificSemanticID(validating: " unit ").canonicalText == " unit ")
    #expect(try ScientificSemanticID(validating: "unit") == ScientificSemanticID(validating: "unit"))
}

@Test
func scientificImportScalarModelSemanticIDsEnforceContentAndBothByteBounds() throws {
    #expect(scientificSemanticIDError("") == .empty)
    #expect(scientificSemanticIDError(" \u{2003}\t") == .blank)
    #expect(scientificSemanticIDError("unit\nA") == .containsControlCharacter)
    #expect(scientificSemanticIDError("unit\tA") == .containsControlCharacter)
    #expect(scientificSemanticIDError("unit A") == nil)

    let maximum = ScientificSemanticID.maximumSourceUTF8ByteCount
    #expect(try ScientificSemanticID(validating: String(repeating: "a", count: maximum))
        .canonicalText.utf8.count == maximum)
    #expect(
        scientificSemanticIDError(String(repeating: "a", count: maximum + 1))
            == .sourceTooLong(maximumUTF8Bytes: maximum)
    )

    // Each starter prevents combining marks from adjacent repetitions interacting. Thus each
    // `x + U+0344` is three source bytes and five NFC bytes, crossing only the canonical bound.
    let nfcExpanding = String(repeating: "x\u{0344}", count: 52)
    #expect(nfcExpanding.utf8.count <= ScientificSemanticID.maximumSourceUTF8ByteCount)
    #expect(nfcExpanding.precomposedStringWithCanonicalMapping.utf8.count
        > ScientificSemanticID.maximumCanonicalUTF8ByteCount)
    #expect(
        scientificSemanticIDError(nfcExpanding)
            == .canonicalValueTooLong(
                maximumUTF8Bytes: ScientificSemanticID.maximumCanonicalUTF8ByteCount
            )
    )
}

@Test
func scientificImportScalarModelAttributeKeysReserveUnitSuggestionSuffix() throws {
    #expect(eventAttributeKeyError("") == .empty)
    #expect(eventAttributeKeyError("\u{2002}\t ") == .blank)
    #expect(eventAttributeKeyError("condition\nname") == .containsControlCharacter)
    #expect(eventAttributeKeyError("condition=drug") == .containsMetadataSeparator)
    #expect(eventAttributeKeyError("intensity.unit") == .reservedUnitSuffix)
    #expect(eventAttributeKeyError(".unit") == .reservedUnitSuffix)
    #expect(try EventAttributeKey(validating: "intensity.Unit").canonicalText == "intensity.Unit")
    #expect(try EventAttributeKey(validating: "@condition").canonicalText == "@condition")
    #expect(try EventAttributeKey(validating: " intensity ").canonicalText == " intensity ")

    let composed = try EventAttributeKey(validating: "caf\u{00E9}")
    let decomposed = try EventAttributeKey(validating: "cafe\u{0301}")
    #expect(composed == decomposed)
    #expect(
        eventAttributeKeyError(String(repeating: "k", count: 257))
            == .sourceTooLong(maximumUTF8Bytes: 256)
    )
}

@Test
func scientificImportScalarModelCanonicalStringsUseNFCAndPermitExplicitEmpty() throws {
    #expect(
        try CanonicalStringValue(validating: "caf\u{00E9}")
            == CanonicalStringValue(validating: "cafe\u{0301}")
    )
    #expect(try CanonicalStringValue(validating: "").canonicalText.isEmpty)
    #expect(try CanonicalStringValue(validating: " \u{2003}\t").canonicalText == " \u{2003}\t")
    #expect(try CanonicalStringValue(validating: " Value ").canonicalText == " Value ")

    let maximum = CanonicalStringValue.maximumSourceUTF8ByteCount
    #expect(
        canonicalStringError(String(repeating: "s", count: maximum + 1))
            == .sourceTooLong(maximumUTF8Bytes: maximum)
    )
}

@Test
func scientificImportScalarModelExactIntegersCanonicalizeWithoutMagnitudeLoss() throws {
    let cases: [(String, String, Bool, String)] = [
        ("0", "0", false, "0"),
        ("-0", "0", false, "0"),
        ("+000", "0", false, "0"),
        ("000123", "123", false, "123"),
        ("-000123", "-123", true, "123"),
        ("\t+42 \t", "42", false, "42"),
        (String(repeating: "9", count: 128), String(repeating: "9", count: 128), false, String(repeating: "9", count: 128)),
    ]

    for (source, canonical, negative, magnitude) in cases {
        let value = try ExactIntegerValue.parse(source)
        #expect(value.canonicalText == canonical)
        #expect(value.isNegative == negative)
        #expect(value.magnitudeDigits == magnitude)
    }
}

@Test
func scientificImportScalarModelExactDecimalsHaveOneCanonicalRepresentation() throws {
    let equivalent = try ["1", "1.0", "01e0", "100e-2", "+0001.000e+0"]
        .map(ExactDecimalValue.parse)
    #expect(Set(equivalent).count == 1)
    #expect(equivalent[0].canonicalText == "1")
    #expect(equivalent[0].coefficientDigits == "1")
    #expect(equivalent[0].base10Exponent == 0)

    let hundred = try ExactDecimalValue.parse("100.")
    #expect(hundred.coefficientDigits == "1")
    #expect(hundred.base10Exponent == 2)
    #expect(hundred.canonicalText == "1e2")

    let hundredth = try ExactDecimalValue.parse("0.0100")
    #expect(hundredth.coefficientDigits == "1")
    #expect(hundredth.base10Exponent == -2)
    #expect(hundredth.canonicalText == "1e-2")

    let negativeEquivalent = try ["-1", "-1.0", "-01e0", "-100e-2"]
        .map(ExactDecimalValue.parse)
    #expect(Set(negativeEquivalent).count == 1)
    #expect(negativeEquivalent[0].canonicalText == "-1")

    let multipleZeroEquivalent = try ["1200", "12e2", "1.2e3", "0.01200e5"]
        .map(ExactDecimalValue.parse)
    #expect(Set(multipleZeroEquivalent).count == 1)
    #expect(multipleZeroEquivalent[0].canonicalText == "12e2")
}

@Test
func scientificImportScalarModelIntegerAndExactDecimalRemainDifferentTypes() throws {
    let integer = EventAttributeValue.integer(try ExactIntegerValue.parse("1"))
    let decimal = EventAttributeValue.exactDecimal(try ExactDecimalValue.parse("1.0"))

    #expect(integer.scalarType == .integer)
    #expect(decimal.scalarType == .exactDecimal)
    #expect(integer != decimal)
}

@Test
func scientificImportScalarModelExactDecimalsNormalizeEverySignedZeroAfterSyntax() throws {
    for source in ["0", "-0", "+0.000", "-0e999999999999999999999999999999"] {
        let value = try ExactDecimalValue.parse(source)
        #expect(value.canonicalText == "0")
        #expect(!value.isNegative)
        #expect(value.coefficientDigits == "0")
        #expect(value.base10Exponent == 0)
    }

    let malformed = "0e999999999999999999999999x"
    #expect(
        exactAttributeError(malformed, parse: ExactDecimalValue.parse)
            == .invalidSyntax(
                utf8Offset: malformed.utf8.count - 1,
                issue: .unexpectedCharacter
            )
    )
}

@Test
func scientificImportScalarModelExactDecimalExponentBoundariesAreChecked() throws {
    let maximum = try ExactDecimalValue.parse("1e9223372036854775807")
    #expect(maximum.base10Exponent == .max)
    #expect(maximum.canonicalText == "1e9223372036854775807")

    let minimum = try ExactDecimalValue.parse("1e-9223372036854775808")
    #expect(minimum.base10Exponent == .min)
    #expect(minimum.canonicalText == "1e-9223372036854775808")

    #expect(
        exactAttributeError("1e9223372036854775808", parse: ExactDecimalValue.parse)
            == .normalizedExponentOutOfRange
    )
    #expect(
        exactAttributeError("1e-9223372036854775809", parse: ExactDecimalValue.parse)
            == .normalizedExponentOutOfRange
    )
}

@Test
func scientificImportScalarModelExactDecimalNormalizationChecksExponentAdjustment() throws {
    #expect(try ExactDecimalValue.parse("1.0e9223372036854775807").base10Exponent == .max)
    #expect(try ExactDecimalValue.parse("1.0e-9223372036854775808").base10Exponent == .min)

    let positiveCompensation = try ExactDecimalValue.parse("0.1e9223372036854775808")
    let positiveBoundary = try ExactDecimalValue.parse("1e9223372036854775807")
    #expect(positiveCompensation == positiveBoundary)
    #expect(positiveCompensation.base10Exponent == .max)

    let negativeCompensation = try ExactDecimalValue.parse("10e-9223372036854775809")
    let negativeBoundary = try ExactDecimalValue.parse("1e-9223372036854775808")
    #expect(negativeCompensation == negativeBoundary)
    #expect(negativeCompensation.base10Exponent == .min)

    #expect(
        exactAttributeError("10e9223372036854775807", parse: ExactDecimalValue.parse)
            == .normalizedExponentOutOfRange
    )
    #expect(
        exactAttributeError("1.1e-9223372036854775808", parse: ExactDecimalValue.parse)
            == .normalizedExponentOutOfRange
    )
}

@Test
func scientificImportScalarModelExactParsersRejectMalformedAndNonASCIIGrammar() {
    let integerCases: [(String, ExactEventAttributeParseError)] = [
        ("+", .invalidSyntax(utf8Offset: 1, issue: .missingMantissaDigits)),
        ("1.0", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1e2", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("−1", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("１２", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("1 2", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1\n", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
    ]
    for (source, expected) in integerCases {
        #expect(exactAttributeError(source, parse: ExactIntegerValue.parse) == expected)
    }

    let decimalCases: [(String, ExactEventAttributeParseError)] = [
        (".", .invalidSyntax(utf8Offset: 1, issue: .missingMantissaDigits)),
        (".e1", .invalidSyntax(utf8Offset: 1, issue: .missingMantissaDigits)),
        ("1e", .invalidSyntax(utf8Offset: 2, issue: .missingExponentDigits)),
        ("1e+", .invalidSyntax(utf8Offset: 3, issue: .missingExponentDigits)),
        ("NaN", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("1,5", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1_0", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1ms", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1\u{00A0}", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
    ]
    for (source, expected) in decimalCases {
        #expect(exactAttributeError(source, parse: ExactDecimalValue.parse) == expected)
    }
}

@Test
func scientificImportScalarModelExactParserErrorPriorityIsLengthThenEmptyThenSyntaxThenRange() {
    let tooLong = String(repeating: " ", count: 128) + "0"
    #expect(
        exactAttributeError(tooLong, parse: ExactIntegerValue.parse)
            == .lexemeTooLong(maximumUTF8Bytes: 128)
    )
    #expect(exactAttributeError("\t \t", parse: ExactIntegerValue.parse) == .empty)
    #expect(
        exactAttributeError("1e999999999999999999999999x", parse: ExactDecimalValue.parse)
            == .invalidSyntax(utf8Offset: 26, issue: .unexpectedCharacter)
    )
    #expect(
        exactAttributeError("1e999999999999999999999999", parse: ExactDecimalValue.parse)
            == .exponentOutOfRange
    )
}

@Test
func scientificImportScalarModelExactParserAcceptsRaw128ByteBoundaryBeforeTrim() throws {
    let accepted = String(repeating: " ", count: 127) + "0"
    let rejected = accepted + " "
    #expect(accepted.utf8.count == ExactIntegerValue.maximumLexemeUTF8ByteCount)
    #expect(try ExactIntegerValue.parse(accepted).canonicalText == "0")

    let exactDecimalBoundary = String(repeating: "9", count: 128)
    #expect(exactDecimalBoundary.utf8.count == ExactDecimalValue.maximumLexemeUTF8ByteCount)
    #expect(try ExactDecimalValue.parse(exactDecimalBoundary).coefficientDigits
        == exactDecimalBoundary)
    #expect(
        exactAttributeError(rejected, parse: ExactDecimalValue.parse)
            == .lexemeTooLong(maximumUTF8Bytes: 128)
    )
}

@Test
func scientificImportScalarModelUnitsPreserveThreeExplicitStates() throws {
    let composed = try OpaqueUnitSymbol(validating: "\u{00B5}V")
    let decomposed = try OpaqueUnitSymbol(validating: "\u{03BC}V")
    // MICRO SIGN and GREEK SMALL LETTER MU are intentionally different symbols after NFC.
    #expect(composed != decomposed)

    let nfcComposed = try OpaqueUnitSymbol(validating: "m\u{00E9}")
    let nfcDecomposed = try OpaqueUnitSymbol(validating: "me\u{0301}")
    #expect(nfcComposed == nfcDecomposed)

    let specified = ConfirmedEventAttributeUnit.specified(
        try OpaqueUnitSymbol(validating: "mV")
    )
    let states: Set<ConfirmedEventAttributeUnit> = [
        .notApplicable,
        .dimensionless,
        specified,
    ]
    #expect(states.count == 3)

    guard case .specified(let symbol) = specified else {
        Issue.record("Expected the specified unit state")
        return
    }
    #expect(symbol.canonicalText == "mV")

    #expect(opaqueUnitError("") == .empty)
    #expect(opaqueUnitError(" \u{2003}\t") == .blank)
    #expect(opaqueUnitError("m\nV") == .containsControlCharacter)
    #expect(try OpaqueUnitSymbol(validating: " mV ").canonicalText == " mV ")
    #expect(
        opaqueUnitError(String(repeating: "u", count: 257))
            == .sourceTooLong(maximumUTF8Bytes: 256)
    )
}

@Test
func scientificImportScalarModelEventAttributeValuesExposeEveryDistinctScalarTag() throws {
    let values: [EventAttributeValue] = [
        .string(try CanonicalStringValue(validating: "value")),
        .integer(try ExactIntegerValue.parse("1")),
        .exactDecimal(try ExactDecimalValue.parse("1.0")),
        .boolean(true),
    ]
    #expect(values.map(\.scalarType) == [.string, .integer, .exactDecimal, .boolean])
    #expect(Set(values).count == 4)

    guard case .string(let stringValue) = values[0] else {
        Issue.record("Expected the string scalar tag")
        return
    }
    guard case .integer(let integerValue) = values[1] else {
        Issue.record("Expected the integer scalar tag")
        return
    }
    guard case .exactDecimal(let decimalValue) = values[2] else {
        Issue.record("Expected the exact-decimal scalar tag")
        return
    }
    guard case .boolean(let booleanValue) = values[3] else {
        Issue.record("Expected the Boolean scalar tag")
        return
    }
    #expect(stringValue.canonicalText == "value")
    #expect(integerValue.canonicalText == "1")
    #expect(decimalValue.canonicalText == "1")
    #expect(booleanValue)
}

private func canonicalStringError(_ source: String) -> CanonicalStringValueError? {
    do {
        _ = try CanonicalStringValue(validating: source)
        return nil
    } catch let error as CanonicalStringValueError {
        return error
    } catch {
        Issue.record("Unexpected canonical-string error type: \(error)")
        return nil
    }
}

private func scientificSemanticIDError(_ source: String) -> ScientificSemanticIDError? {
    do {
        _ = try ScientificSemanticID(validating: source)
        return nil
    } catch let error as ScientificSemanticIDError {
        return error
    } catch {
        Issue.record("Unexpected semantic ID error type: \(error)")
        return nil
    }
}

private func eventAttributeKeyError(_ source: String) -> EventAttributeKeyError? {
    do {
        _ = try EventAttributeKey(validating: source)
        return nil
    } catch let error as EventAttributeKeyError {
        return error
    } catch {
        Issue.record("Unexpected attribute-key error type: \(error)")
        return nil
    }
}

private func opaqueUnitError(_ source: String) -> OpaqueUnitSymbolError? {
    do {
        _ = try OpaqueUnitSymbol(validating: source)
        return nil
    } catch let error as OpaqueUnitSymbolError {
        return error
    } catch {
        Issue.record("Unexpected unit-symbol error type: \(error)")
        return nil
    }
}

private func exactAttributeError<Value>(
    _ source: String,
    parse: (String) throws -> Value
) -> ExactEventAttributeParseError? {
    do {
        _ = try parse(source)
        Issue.record("Expected exact attribute parsing to fail")
        return nil
    } catch let error as ExactEventAttributeParseError {
        return error
    } catch {
        Issue.record("Unexpected exact attribute error type: \(error)")
        return nil
    }
}
