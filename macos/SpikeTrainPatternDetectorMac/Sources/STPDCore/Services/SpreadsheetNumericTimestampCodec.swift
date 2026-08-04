import Foundation

/// Stable lexical reasons for rejecting a raw OOXML numeric value.
public enum SpreadsheetNumericTimestampSyntaxIssue: String, Hashable, Sendable {
    case missingMantissaDigits
    case missingExponentDigits
    case unexpectedCharacter
}

/// A bounded failure produced while proving that one stored binary64 has one microsecond inverse.
public enum SpreadsheetNumericTimestampDecodeError: Error, Hashable, Sendable, LocalizedError {
    case empty
    case lexemeTooLong(maximumUTF8Bytes: Int)
    case invalidSyntax(utf8Offset: Int, issue: SpreadsheetNumericTimestampSyntaxIssue)
    case storedBinary64IsNotFinite
    case outsideSignedMicrosecondRange
    case noWholeMicrosecondRoundTrip
    case adjacentTickCollision
    case binary64ReconstructionInvariant
    case canonicalProjectionInvariant

    public var errorDescription: String? {
        switch self {
        case .empty:
            return "The stored spreadsheet numeric value is empty."
        case .lexemeTooLong(let maximumUTF8Bytes):
            return "The stored spreadsheet numeric value exceeds \(maximumUTF8Bytes) UTF-8 bytes."
        case .invalidSyntax(let utf8Offset, let issue):
            return "Invalid stored spreadsheet number at UTF-8 byte offset \(utf8Offset): \(issue.rawValue)."
        case .storedBinary64IsNotFinite:
            return "The stored spreadsheet number does not reconstruct a finite binary64 value."
        case .outsideSignedMicrosecondRange:
            return "The stored spreadsheet number is outside the signed microsecond range."
        case .noWholeMicrosecondRoundTrip:
            return "No whole microsecond tick round-trips to the stored binary64 value."
        case .adjacentTickCollision:
            return "An adjacent microsecond tick maps to the same stored binary64 value."
        case .binary64ReconstructionInvariant:
            return "The validated numeric spelling could not be reconstructed as binary64."
        case .canonicalProjectionInvariant:
            return "A canonical microsecond spelling could not be projected to binary64."
        }
    }
}

/// Validates the representability of an XLSX stored binary64 as one unique microsecond tick.
///
/// This proves a property of the value stored in the workbook, not the accuracy of the value that
/// a producer originally measured or intended. Cell display formatting is deliberately irrelevant.
/// The raw lexeme is the untrimmed OOXML numeric `<v>` text and must already satisfy the grammar.
public enum SpreadsheetNumericTimestampCodec {
    public static let maximumRawLexemeUTF8ByteCount = 128

    private static let signedRankMask: UInt64 = 1 << 63

    /// Accepted ASCII grammar:
    /// `sign? (digits ("." digits?)? | "." digits) (("e" | "E") sign? digits)?`
    public static func decode(
        rawLexeme: String,
        sourceUnit: SpikeTimeUnit
    ) throws -> MicrosecondTick {
        try validateGrammar(rawLexeme)

        guard let storedValue = Double(rawLexeme) else {
            throw SpreadsheetNumericTimestampDecodeError.binary64ReconstructionInvariant
        }
        guard storedValue.isFinite else {
            throw SpreadsheetNumericTimestampDecodeError.storedBinary64IsNotFinite
        }

        let minimumProjection = try canonicalProjection(
            MicrosecondTick(microseconds: .min),
            unit: sourceUnit
        )
        let maximumProjection = try canonicalProjection(
            MicrosecondTick(microseconds: .max),
            unit: sourceUnit
        )
        guard storedValue >= minimumProjection, storedValue <= maximumProjection else {
            throw SpreadsheetNumericTimestampDecodeError.outsideSignedMicrosecondRange
        }

        // Signed Int64 order is mapped monotonically onto UInt64, so this lower-bound search covers
        // the entire tick domain in at most 64 iterations (69 total canonical projections including
        // range, candidate, and neighbor checks), without signed arithmetic or a guessed
        // floating-point neighborhood. Each projection is one correctly-rounded parse of the exact
        // canonical decimal k/1_000_000 (seconds) or k/1_000 (milliseconds); operational
        // `Double(k) / scale` is intentionally not used because it can double-round.
        var lowerRank: UInt64 = .min
        var upperRank: UInt64 = .max
        while lowerRank < upperRank {
            let middleRank = lowerRank + (upperRank - lowerRank) / 2
            let middleTick = tick(atSignedRank: middleRank)
            let middleProjection = try canonicalProjection(middleTick, unit: sourceUnit)
            if middleProjection < storedValue {
                lowerRank = middleRank + 1
            } else {
                upperRank = middleRank
            }
        }

        var candidate = tick(atSignedRank: lowerRank)
        let candidateProjection = try canonicalProjection(candidate, unit: sourceUnit)
        guard sameStoredBinary64(candidateProjection, storedValue) else {
            throw SpreadsheetNumericTimestampDecodeError.noWholeMicrosecondRoundTrip
        }

        // Signed zero is the one intentional bit-pattern normalization. It still undergoes the
        // same adjacent-tick uniqueness proof below.
        if storedValue == 0 {
            candidate = .zero
        }

        let (previousMicroseconds, previousOverflow) = candidate.microseconds
            .subtractingReportingOverflow(1)
        let (nextMicroseconds, nextOverflow) = candidate.microseconds
            .addingReportingOverflow(1)
        if !previousOverflow {
            let previousProjection = try canonicalProjection(
                MicrosecondTick(microseconds: previousMicroseconds),
                unit: sourceUnit
            )
            guard !sameStoredBinary64(previousProjection, storedValue) else {
                throw SpreadsheetNumericTimestampDecodeError.adjacentTickCollision
            }
        }
        if !nextOverflow {
            let nextProjection = try canonicalProjection(
                MicrosecondTick(microseconds: nextMicroseconds),
                unit: sourceUnit
            )
            guard !sameStoredBinary64(nextProjection, storedValue) else {
                throw SpreadsheetNumericTimestampDecodeError.adjacentTickCollision
            }
        }

        // Canonical projection is monotone. Its preimage for one binary64 is therefore contiguous;
        // equality at the candidate plus inequality at each in-domain adjacent tick proves global
        // uniqueness.
        return candidate
    }

    private static func validateGrammar(_ rawLexeme: String) throws {
        guard rawLexeme.utf8.prefix(maximumRawLexemeUTF8ByteCount + 1).count
                <= maximumRawLexemeUTF8ByteCount else {
            throw SpreadsheetNumericTimestampDecodeError.lexemeTooLong(
                maximumUTF8Bytes: maximumRawLexemeUTF8ByteCount
            )
        }

        let bytes = Array(rawLexeme.utf8)
        guard !bytes.isEmpty else {
            throw SpreadsheetNumericTimestampDecodeError.empty
        }

        var index = 0
        if bytes[index] == 0x2B || bytes[index] == 0x2D { // + or -
            index += 1
        }

        var mantissaDigitCount = 0
        if index < bytes.count, isASCIIDigit(bytes[index]) {
            while index < bytes.count, isASCIIDigit(bytes[index]) {
                mantissaDigitCount += 1
                index += 1
            }
            if index < bytes.count, bytes[index] == 0x2E { // .
                index += 1
                while index < bytes.count, isASCIIDigit(bytes[index]) {
                    mantissaDigitCount += 1
                    index += 1
                }
            }
        } else if index < bytes.count, bytes[index] == 0x2E { // .
            let decimalPointOffset = index
            index += 1
            guard index < bytes.count, isASCIIDigit(bytes[index]) else {
                throw SpreadsheetNumericTimestampDecodeError.invalidSyntax(
                    utf8Offset: index < bytes.count ? index : decimalPointOffset + 1,
                    issue: .missingMantissaDigits
                )
            }
            while index < bytes.count, isASCIIDigit(bytes[index]) {
                mantissaDigitCount += 1
                index += 1
            }
        } else {
            guard index < bytes.count else {
                throw SpreadsheetNumericTimestampDecodeError.invalidSyntax(
                    utf8Offset: index,
                    issue: .missingMantissaDigits
                )
            }
            throw SpreadsheetNumericTimestampDecodeError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }

        guard mantissaDigitCount > 0 else {
            throw SpreadsheetNumericTimestampDecodeError.invalidSyntax(
                utf8Offset: index,
                issue: .missingMantissaDigits
            )
        }

        if index < bytes.count, bytes[index] == 0x65 || bytes[index] == 0x45 { // e or E
            index += 1
            if index < bytes.count, bytes[index] == 0x2B || bytes[index] == 0x2D {
                index += 1
            }
            let exponentStart = index
            while index < bytes.count, isASCIIDigit(bytes[index]) {
                index += 1
            }
            guard index > exponentStart else {
                throw SpreadsheetNumericTimestampDecodeError.invalidSyntax(
                    utf8Offset: index,
                    issue: .missingExponentDigits
                )
            }
        }

        guard index == bytes.count else {
            throw SpreadsheetNumericTimestampDecodeError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }
    }

    private static func canonicalProjection(
        _ tick: MicrosecondTick,
        unit: SpikeTimeUnit
    ) throws -> Double {
        let canonicalText = ExactTimestampTextCodec.encode(tick, displayUnit: unit)
        guard let value = Double(canonicalText), value.isFinite else {
            throw SpreadsheetNumericTimestampDecodeError.canonicalProjectionInvariant
        }
        return value
    }

    private static func tick(atSignedRank rank: UInt64) -> MicrosecondTick {
        MicrosecondTick(microseconds: Int64(bitPattern: rank ^ signedRankMask))
    }

    private static func sameStoredBinary64(_ lhs: Double, _ rhs: Double) -> Bool {
        if lhs == 0, rhs == 0 { return true }
        return lhs.bitPattern == rhs.bitPattern
    }

    private static func isASCIIDigit(_ byte: UInt8) -> Bool {
        byte >= 0x30 && byte <= 0x39
    }
}
