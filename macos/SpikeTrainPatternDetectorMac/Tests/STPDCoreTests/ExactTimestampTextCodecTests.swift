import STPDCore
import Testing

@Test
func exactTimestampEquivalentSecondMillisecondAndScientificSpellings() throws {
    let cases: [(String, SpikeTimeUnit)] = [
        ("0.000001", .seconds),
        ("1e-6", .seconds),
        ("0.001", .milliseconds),
        ("1e-3", .milliseconds),
    ]

    for (text, unit) in cases {
        #expect(try ExactTimestampTextCodec.decode(text, sourceUnit: unit).microseconds == 1)
    }
}

@Test
func exactTimestampAcceptsExactTrailingZeroReduction() throws {
    let cases: [(String, SpikeTimeUnit, Int64)] = [
        ("1.0000000", .seconds, 1_000_000),
        ("0.0010", .milliseconds, 1),
        ("10e-7", .seconds, 1),
        ("+1.", .seconds, 1_000_000),
        (".001", .milliseconds, 1),
    ]

    for (text, unit, expected) in cases {
        #expect(try ExactTimestampTextCodec.decode(text, sourceUnit: unit).microseconds == expected)
    }
}

@Test
func exactTimestampRejectsValuesOffTheMicrosecondGrid() {
    for (text, unit) in [
        ("0.0000005", SpikeTimeUnit.seconds),
        ("0.0005", SpikeTimeUnit.milliseconds),
        ("1e-7", SpikeTimeUnit.seconds),
    ] {
        #expect(exactTimestampError(text, unit: unit) == .notExactlyRepresentableInMicroseconds)
    }
}

@Test
func exactTimestampNormalizesEverySignedZero() throws {
    for text in ["0", "-0", "+0.000000", "-0.000000", "-0e+999", "0e-999"] {
        let tick = try ExactTimestampTextCodec.decode(text, sourceUnit: .seconds)
        #expect(tick == .zero)
        #expect(ExactTimestampTextCodec.encode(tick, displayUnit: .seconds) == "0.000000")
        #expect(ExactTimestampTextCodec.encode(tick, displayUnit: .milliseconds) == "0.000")
    }
}

@Test
func exactTimestampSignedBoundariesRoundTripAndOutsideValuesFail() throws {
    let cases: [(String, SpikeTimeUnit, Int64)] = [
        ("9223372036854.775807", .seconds, .max),
        ("-9223372036854.775808", .seconds, .min),
        ("9223372036854775.807", .milliseconds, .max),
        ("-9223372036854775.808", .milliseconds, .min),
    ]

    for (text, unit, expected) in cases {
        let tick = try ExactTimestampTextCodec.decode(text, sourceUnit: unit)
        #expect(tick.microseconds == expected)
        #expect(ExactTimestampTextCodec.encode(tick, displayUnit: unit) == text)
    }

    #expect(exactTimestampError("9223372036854.775808", unit: .seconds) == .outsideSignedMicrosecondRange)
    #expect(exactTimestampError("-9223372036854.775809", unit: .seconds) == .outsideSignedMicrosecondRange)
    #expect(exactTimestampError("9223372036854775.808", unit: .milliseconds) == .outsideSignedMicrosecondRange)
    #expect(exactTimestampError("-9223372036854775.809", unit: .milliseconds) == .outsideSignedMicrosecondRange)
}

@Test
func exactTimestampKeepsAdjacentTicksAboveBinaryFloatingPointIntegerPrecision() throws {
    let lower = try ExactTimestampTextCodec.decode("9007199254.740992", sourceUnit: .seconds)
    let upper = try ExactTimestampTextCodec.decode("9007199254.740993", sourceUnit: .seconds)

    #expect(lower.microseconds == 9_007_199_254_740_992)
    #expect(upper.microseconds == 9_007_199_254_740_993)
    #expect(try upper.interval(since: lower) == 1)
}

@Test
func exactTimestampAppliesRawUTF8LengthLimitBeforeASCIITrim() throws {
    let exactlyAtLimit = String(repeating: " ", count: 127) + "0"
    let aboveLimit = String(repeating: " ", count: 128) + "0"

    #expect(exactlyAtLimit.utf8.count == ExactTimestampTextCodec.maximumLexemeUTF8ByteCount)
    #expect(try ExactTimestampTextCodec.decode(exactlyAtLimit, sourceUnit: .seconds) == .zero)
    #expect(exactTimestampError(aboveLimit, unit: .seconds) == .lexemeTooLong(maximumUTF8Bytes: 128))
    #expect(
        exactTimestampError(String(repeating: "x", count: 129), unit: .seconds)
            == .lexemeTooLong(maximumUTF8Bytes: 128)
    )
}

@Test
func exactTimestampTrimsOnlyLeadingAndTrailingASCIISpaceAndTab() throws {
    #expect(
        try ExactTimestampTextCodec.decode(" \t-0.001\t ", sourceUnit: .milliseconds)
            .microseconds == -1
    )
    #expect(exactTimestampError("\u{00A0}1", unit: .seconds) == .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter))
    #expect(exactTimestampError("1\n", unit: .seconds) == .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter))
}

@Test
func exactTimestampMalformedGrammarHasStableUTF8Offsets() {
    let cases: [(String, ExactTimestampParseError)] = [
        ("NaN", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("Inf", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("null", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("1,5", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1_000", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("0x10", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1ms", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("−1", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("１２", .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)),
        ("1 2", .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)),
        ("1..0", .invalidSyntax(utf8Offset: 2, issue: .unexpectedCharacter)),
        ("+", .invalidSyntax(utf8Offset: 1, issue: .missingMantissaDigits)),
        (".", .invalidSyntax(utf8Offset: 1, issue: .missingMantissaDigits)),
        (".e1", .invalidSyntax(utf8Offset: 1, issue: .missingMantissaDigits)),
        ("1e", .invalidSyntax(utf8Offset: 2, issue: .missingExponentDigits)),
        ("1e+", .invalidSyntax(utf8Offset: 3, issue: .missingExponentDigits)),
        ("1eX", .invalidSyntax(utf8Offset: 2, issue: .missingExponentDigits)),
    ]

    for (text, expected) in cases {
        #expect(exactTimestampError(text, unit: .seconds) == expected)
    }
}

@Test
func exactTimestampUsesFixedErrorPriority() {
    #expect(exactTimestampError("\t \t", unit: .seconds) == .empty)
    #expect(
        exactTimestampError(String(repeating: " ", count: 129), unit: .seconds)
            == .lexemeTooLong(maximumUTF8Bytes: 128)
    )
    #expect(exactTimestampError("1e999x", unit: .seconds) == .invalidSyntax(utf8Offset: 5, issue: .unexpectedCharacter))
    #expect(exactTimestampError("1e-999", unit: .seconds) == .notExactlyRepresentableInMicroseconds)
    #expect(exactTimestampError("1e999", unit: .seconds) == .outsideSignedMicrosecondRange)
    #expect(
        exactTimestampError("92233720368547758080.0000005", unit: .seconds)
            == .notExactlyRepresentableInMicroseconds
    )
}

@Test
func exactTimestampHandlesSaturatedExponentsAfterFullSyntaxValidation() throws {
    #expect(exactTimestampError("1e9999", unit: .seconds) == .outsideSignedMicrosecondRange)
    #expect(
        exactTimestampError("1e-9999", unit: .seconds)
            == .notExactlyRepresentableInMicroseconds
    )
    #expect(try ExactTimestampTextCodec.decode("-0e9999", sourceUnit: .seconds) == .zero)
    #expect(
        exactTimestampError("0e9999x", unit: .seconds)
            == .invalidSyntax(utf8Offset: 6, issue: .unexpectedCharacter)
    )
}

@Test
func exactTimestampEncodeDecodeRoundTripsRepresentativeTicksInBothUnits() throws {
    let values: [Int64] = [
        .min,
        -9_007_199_254_740_993,
        -1_000_001,
        -1,
        0,
        1,
        1_000_001,
        9_007_199_254_740_992,
        9_007_199_254_740_993,
        .max,
    ]

    for value in values {
        let tick = MicrosecondTick(microseconds: value)
        for unit in [SpikeTimeUnit.seconds, .milliseconds] {
            let encoded = ExactTimestampTextCodec.encode(tick, displayUnit: unit)
            #expect(try ExactTimestampTextCodec.decode(encoded, sourceUnit: unit) == tick)
        }
    }
}

@Test
func exactTimestampFormatterHasFixedPrecisionAndHandlesSignedMinimum() {
    let cases: [(Int64, String, String)] = [
        (0, "0.000000", "0.000"),
        (1, "0.000001", "0.001"),
        (-1, "-0.000001", "-0.001"),
        (1_000_001, "1.000001", "1000.001"),
        (.max, "9223372036854.775807", "9223372036854775.807"),
        (.min, "-9223372036854.775808", "-9223372036854775.808"),
    ]

    for (value, seconds, milliseconds) in cases {
        let tick = MicrosecondTick(microseconds: value)
        #expect(ExactTimestampTextCodec.encode(tick, displayUnit: .seconds) == seconds)
        #expect(ExactTimestampTextCodec.encode(tick, displayUnit: .milliseconds) == milliseconds)
    }
}

@Test
func microsecondTickCheckedIntervalAndRebaseNeverWrap() throws {
    let absolute = MicrosecondTick(microseconds: -250)
    let origin = MicrosecondTick(microseconds: -1_000)
    #expect(try absolute.rebased(relativeTo: origin) == MicrosecondTick(microseconds: 750))
    #expect(try absolute.interval(since: origin) == 750)

    #expect(microsecondArithmeticError { try MicrosecondTick(microseconds: .max).interval(since: .init(microseconds: .min)) } == .overflow)
    #expect(microsecondArithmeticError { try MicrosecondTick(microseconds: .min).interval(since: .init(microseconds: 1)) } == .overflow)
    #expect(microsecondArithmeticError { try MicrosecondTick(microseconds: .max).rebased(relativeTo: .init(microseconds: -1)) } == .overflow)
    #expect(microsecondArithmeticError { try MicrosecondTick(microseconds: .min).rebased(relativeTo: .init(microseconds: 1)) } == .overflow)
}

private func exactTimestampError(
    _ text: String,
    unit: SpikeTimeUnit
) -> ExactTimestampParseError? {
    do {
        _ = try ExactTimestampTextCodec.decode(text, sourceUnit: unit)
        Issue.record("Expected exact timestamp parsing to fail")
        return nil
    } catch let error as ExactTimestampParseError {
        return error
    } catch {
        Issue.record("Unexpected error type: \(error)")
        return nil
    }
}

private func microsecondArithmeticError(
    _ operation: () throws -> some Any
) -> MicrosecondArithmeticError? {
    do {
        _ = try operation()
        Issue.record("Expected microsecond arithmetic to fail")
        return nil
    } catch let error as MicrosecondArithmeticError {
        return error
    } catch {
        Issue.record("Unexpected error type: \(error)")
        return nil
    }
}
