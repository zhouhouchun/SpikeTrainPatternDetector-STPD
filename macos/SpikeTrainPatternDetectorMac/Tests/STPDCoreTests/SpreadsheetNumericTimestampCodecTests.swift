@testable import STPDCore
import Testing

private func spreadsheetTimestampError(
    _ rawLexeme: String,
    unit: SpikeTimeUnit = .seconds
) -> SpreadsheetNumericTimestampDecodeError? {
    do {
        _ = try SpreadsheetNumericTimestampCodec.decode(
            rawLexeme: rawLexeme,
            sourceUnit: unit
        )
        return nil
    } catch let error as SpreadsheetNumericTimestampDecodeError {
        return error
    } catch {
        Issue.record("Unexpected error type: \(error)")
        return nil
    }
}

private func canonicalSpreadsheetLexeme(
    _ microseconds: Int64,
    unit: SpikeTimeUnit
) -> String {
    ExactTimestampTextCodec.encode(
        MicrosecondTick(microseconds: microseconds),
        displayUnit: unit
    )
}

private func requireCanonicalProjection(
    _ microseconds: Int64,
    unit: SpikeTimeUnit
) throws -> Double {
    try #require(Double(canonicalSpreadsheetLexeme(microseconds, unit: unit)))
}

@Test
func spreadsheetNumericStoredLexemeValidationDoesNotRequireAUnitOrTickGrid() throws {
    for raw in [
        "0", "+1.25", "-2.5E-3", ".5", "1.",
        "0.0000005", // finite but not on the whole-microsecond grid
        "1e19", // finite but outside the signed-microsecond range
    ] {
        try SpreadsheetNumericTimestampCodec.validateStoredNumberLexeme(raw)
    }
    #expect(throws: SpreadsheetNumericTimestampDecodeError.invalidSyntax(
        utf8Offset: 0,
        issue: .unexpectedCharacter
    )) {
        try SpreadsheetNumericTimestampCodec.validateStoredNumberLexeme(" 1")
    }
    #expect(throws: SpreadsheetNumericTimestampDecodeError.storedBinary64IsNotFinite) {
        try SpreadsheetNumericTimestampCodec.validateStoredNumberLexeme("1e999")
    }
    #expect(throws: SpreadsheetNumericTimestampDecodeError.empty) {
        try SpreadsheetNumericTimestampCodec.validateStoredNumberLexeme("")
    }
}

@Test
func spreadsheetNumericOneMicrosecondEquivalenceAcrossUnitsAndSigns() throws {
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "0.000001",
        sourceUnit: .seconds
    ) == MicrosecondTick(microseconds: 1))
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "0.001",
        sourceUnit: .milliseconds
    ) == MicrosecondTick(microseconds: 1))
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "-0.000001",
        sourceUnit: .seconds
    ) == MicrosecondTick(microseconds: -1))
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "-0.001",
        sourceUnit: .milliseconds
    ) == MicrosecondTick(microseconds: -1))
}

@Test
func spreadsheetNumericExponentSpellingsReconstructStoredBinary64Only() throws {
    for raw in ["1e-6", "+1E-6", "10e-7", ".000001e+0", "1.e-6"] {
        #expect(try SpreadsheetNumericTimestampCodec.decode(
            rawLexeme: raw,
            sourceUnit: .seconds
        ) == MicrosecondTick(microseconds: 1))
    }
    for raw in ["1e-3", "+1E-3", "10e-4", ".001e+0"] {
        #expect(try SpreadsheetNumericTimestampCodec.decode(
            rawLexeme: raw,
            sourceUnit: .milliseconds
        ) == MicrosecondTick(microseconds: 1))
    }
}

@Test
func spreadsheetNumericAcceptsNoncanonicalDecimalThatStoresCanonicalOneMicrosecond() throws {
    let canonical = try #require(Double("0.000001"))
    let noncanonicalRaw = "0.00000100000000000000001"
    let reconstructed = try #require(Double(noncanonicalRaw))
    #expect(reconstructed.bitPattern == canonical.bitPattern)

    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: noncanonicalRaw,
        sourceUnit: .seconds
    ) == MicrosecondTick(microseconds: 1))
}

@Test
func spreadsheetNumericSignedZeroNormalizesToTickZero() throws {
    for raw in ["0", "+0.0", "-0", "-0e999", "0e-999"] {
        #expect(try SpreadsheetNumericTimestampCodec.decode(
            rawLexeme: raw,
            sourceUnit: .seconds
        ) == .zero)
        #expect(try SpreadsheetNumericTimestampCodec.decode(
            rawLexeme: raw,
            sourceUnit: .milliseconds
        ) == .zero)
    }
}

@Test
func spreadsheetNumericHalfMicrosecondAndOffGridValuesAreRejected() throws {
    #expect(spreadsheetTimestampError("0.0000005") == .noWholeMicrosecondRoundTrip)
    #expect(spreadsheetTimestampError("-0.0000005") == .noWholeMicrosecondRoundTrip)
    #expect(spreadsheetTimestampError("0.0005", unit: .milliseconds)
        == .noWholeMicrosecondRoundTrip)
    #expect(spreadsheetTimestampError("-0.0005", unit: .milliseconds)
        == .noWholeMicrosecondRoundTrip)

    let oneMicrosecondInSeconds = try #require(Double("0.000001"))
    #expect(spreadsheetTimestampError(String(oneMicrosecondInSeconds.nextUp))
        == .noWholeMicrosecondRoundTrip)
    #expect(spreadsheetTimestampError(String(oneMicrosecondInSeconds.nextDown))
        == .noWholeMicrosecondRoundTrip)
}

@Test
func spreadsheetNumericGrammarRejectsExtensionsWithoutTrimming() {
    let malformed: [(String, Int, SpreadsheetNumericTimestampSyntaxIssue)] = [
        ("+", 1, .missingMantissaDigits),
        (".", 1, .missingMantissaDigits),
        ("1e", 2, .missingExponentDigits),
        ("1e+", 3, .missingExponentDigits),
        (" 1", 0, .unexpectedCharacter),
        ("1 ", 1, .unexpectedCharacter),
        ("\t1", 0, .unexpectedCharacter),
        ("1\n", 1, .unexpectedCharacter),
        ("1,2", 1, .unexpectedCharacter),
        ("0x1", 1, .unexpectedCharacter),
        ("1_0", 1, .unexpectedCharacter),
        ("--1", 1, .unexpectedCharacter),
        ("NaN", 0, .unexpectedCharacter),
        ("Inf", 0, .unexpectedCharacter),
        ("+Infinity", 1, .unexpectedCharacter),
        ("１", 0, .unexpectedCharacter),
        ("\u{00A0}1", 0, .unexpectedCharacter),
    ]

    for (raw, offset, issue) in malformed {
        #expect(spreadsheetTimestampError(raw) == .invalidSyntax(
            utf8Offset: offset,
            issue: issue
        ))
    }
    #expect(spreadsheetTimestampError("") == .empty)
}

@Test
func spreadsheetNumericBoundsRawLexemeBeforeBinary64Parsing() throws {
    let exact = String(repeating: "0", count: 128)
    let overlong = String(repeating: "0", count: 129)
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: exact,
        sourceUnit: .seconds
    ) == .zero)
    #expect(spreadsheetTimestampError(overlong) == .lexemeTooLong(
        maximumUTF8Bytes: 128
    ))
}

@Test
func spreadsheetNumericDistinguishesNonfiniteAndSignedRangeFailures() {
    #expect(spreadsheetTimestampError("1e999") == .storedBinary64IsNotFinite)
    #expect(spreadsheetTimestampError("-1e999") == .storedBinary64IsNotFinite)
    #expect(spreadsheetTimestampError("1e19") == .outsideSignedMicrosecondRange)
    #expect(spreadsheetTimestampError("-1e19") == .outsideSignedMicrosecondRange)
    #expect(spreadsheetTimestampError("1e19", unit: .milliseconds)
        == .outsideSignedMicrosecondRange)
    #expect(spreadsheetTimestampError("-1e19", unit: .milliseconds)
        == .outsideSignedMicrosecondRange)
}

@Test
func spreadsheetNumericPinsSecondsAdjacentCollisionBoundaryForBothSigns() throws {
    // At exactly 2^33 seconds, binary64 spacing changes. These expected ticks are derived from
    // that IEEE-754 binade transition and the independently specified 1,000,000 ticks/second.
    let boundary: Int64 = 8_589_934_592_000_000

    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "8589934592.000000",
        sourceUnit: .seconds
    ) == MicrosecondTick(microseconds: boundary))
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "-8589934592.000000",
        sourceUnit: .seconds
    ) == MicrosecondTick(microseconds: -boundary))
    #expect(spreadsheetTimestampError("8589934592.000001") == .adjacentTickCollision)
    #expect(spreadsheetTimestampError("-8589934592.000001") == .adjacentTickCollision)
}

@Test
func spreadsheetNumericPinsMillisecondsAdjacentCollisionBoundaryForBothSigns() throws {
    // At exactly 2^43 milliseconds, binary64 spacing changes. The tick boundary follows from
    // the independently specified 1,000 ticks/millisecond.
    let boundary: Int64 = 8_796_093_022_208_000

    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "8796093022208.000",
        sourceUnit: .milliseconds
    ) == MicrosecondTick(microseconds: boundary))
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: "-8796093022208.000",
        sourceUnit: .milliseconds
    ) == MicrosecondTick(microseconds: -boundary))
    #expect(spreadsheetTimestampError("8796093022208.001", unit: .milliseconds)
        == .adjacentTickCollision)
    #expect(spreadsheetTimestampError("-8796093022208.001", unit: .milliseconds)
        == .adjacentTickCollision)
}

@Test
func spreadsheetNumericRejectsAmbiguousSignedInt64EndpointProjections() {
    for unit in [SpikeTimeUnit.seconds, .milliseconds] {
        #expect(spreadsheetTimestampError(
            canonicalSpreadsheetLexeme(.min, unit: unit),
            unit: unit
        ) == .adjacentTickCollision)
        #expect(spreadsheetTimestampError(
            canonicalSpreadsheetLexeme(.max, unit: unit),
            unit: unit
        ) == .adjacentTickCollision)
    }
}

@Test
func spreadsheetNumericInverseAvoidsScaledDoubleRoundingGaps() throws {
    let secondsTick: Int64 = 4_427_784_480_183_209
    let secondsRaw = "4427784480.183209"
    let secondsStored = try #require(Double(secondsRaw))
    let secondsNaive = try #require(Int64(exactly: (secondsStored * 1_000_000).rounded()))
    #expect(secondsNaive != secondsTick)
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: secondsRaw,
        sourceUnit: .seconds
    ) == MicrosecondTick(microseconds: secondsTick))

    let millisecondsTick: Int64 = -4_409_913_900_991_569
    let millisecondsRaw = "-4409913900991.569"
    let millisecondsStored = try #require(Double(millisecondsRaw))
    let millisecondsNaive = try #require(Int64(
        exactly: (millisecondsStored * 1_000).rounded()
    ))
    #expect(millisecondsNaive != millisecondsTick)
    #expect(try SpreadsheetNumericTimestampCodec.decode(
        rawLexeme: millisecondsRaw,
        sourceUnit: .milliseconds
    ) == MicrosecondTick(microseconds: millisecondsTick))
}

@Test
func spreadsheetNumericRepresentativeTicksRoundTripAndRemainLocallyUnique() throws {
    let step: Int64 = 7_000_000_000_000
    for index in -512...512 {
        let tick = Int64(index) * step
        for unit in [SpikeTimeUnit.seconds, .milliseconds] {
            let raw = canonicalSpreadsheetLexeme(tick, unit: unit)
            let stored = try #require(Double(raw))
            #expect(try SpreadsheetNumericTimestampCodec.decode(
                rawLexeme: raw,
                sourceUnit: unit
            ) == MicrosecondTick(microseconds: tick))
            #expect(try requireCanonicalProjection(tick, unit: unit).bitPattern
                == stored.bitPattern)
            #expect(try requireCanonicalProjection(tick - 1, unit: unit).bitPattern
                != stored.bitPattern)
            #expect(try requireCanonicalProjection(tick + 1, unit: unit).bitPattern
                != stored.bitPattern)
        }
    }
}
