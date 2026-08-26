import Foundation

/// Stable lexical reasons for rejecting a raw OOXML numeric value.
public enum SpreadsheetNumericTimestampSyntaxIssue: String, Hashable, Sendable {
    case missingMantissaDigits
    case missingExponentDigits
    case unexpectedCharacter
}

/// A bounded failure produced while proving that one spreadsheet number has one microsecond inverse.
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
            return "No unique whole microsecond tick matches the stored spreadsheet number within the one-ULP serialization allowance."
        case .adjacentTickCollision:
            return "An adjacent microsecond tick maps to the same stored binary64 value."
        case .binary64ReconstructionInvariant:
            return "The validated numeric spelling could not be reconstructed as binary64."
        case .canonicalProjectionInvariant:
            return "A canonical microsecond spelling could not be projected to binary64."
        }
    }
}

/// Validates the representability of an XLSX number as one unique microsecond tick.
///
/// Excel can serialize a decimal value one binary64 ULP above or below the canonical projection of
/// the intended whole-microsecond tick. An exact projection always has priority. When there is no
/// exact projection, that single-ULP residue is accepted only when it identifies exactly one tick;
/// genuine fractional-microsecond values and ambiguous neighboring ticks remain invalid. Cell
/// display formatting is deliberately irrelevant. The raw lexeme is the untrimmed OOXML numeric
/// `<v>` text and must already satisfy the grammar.
public enum SpreadsheetNumericTimestampCodec {
    public static let maximumRawLexemeUTF8ByteCount = 128

    private static let signedRankMask: UInt64 = 1 << 63

    /// Proves only that an OOXML number has the accepted grammar and reconstructs a finite
    /// binary64. Unit selection and unique whole-microsecond inversion remain separate decisions.
    public static func validateStoredNumberLexeme(_ rawLexeme: String) throws {
        _ = try validatedStoredValue(rawLexeme)
    }

    /// Accepted ASCII grammar:
    /// `sign? (digits ("." digits?)? | "." digits) (("e" | "E") sign? digits)?`
    public static func decode(
        rawLexeme: String,
        sourceUnit: SpikeTimeUnit
    ) throws -> MicrosecondTick {
        let storedValue = try validatedStoredValue(rawLexeme)

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
        // the entire tick domain in at most 64 iterations, without signed arithmetic or a guessed
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
        var usedOneULPAllowance = false
        if !sameStoredBinary64(candidateProjection, storedValue) {
            var oneULPCandidates: [MicrosecondTick] = []
            if differsByOneBinary64ULP(candidateProjection, storedValue) {
                oneULPCandidates.append(candidate)
            }
            if lowerRank > .min {
                let previousCandidate = tick(atSignedRank: lowerRank - 1)
                let previousProjection = try canonicalProjection(previousCandidate, unit: sourceUnit)
                if differsByOneBinary64ULP(previousProjection, storedValue) {
                    oneULPCandidates.append(previousCandidate)
                }
            }

            guard oneULPCandidates.count == 1, let normalizedCandidate = oneULPCandidates.first else {
                if oneULPCandidates.count > 1 {
                    throw SpreadsheetNumericTimestampDecodeError.adjacentTickCollision
                }
                throw SpreadsheetNumericTimestampDecodeError.noWholeMicrosecondRoundTrip
            }
            candidate = normalizedCandidate
            usedOneULPAllowance = true
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
            let previousCollides = sameStoredBinary64(previousProjection, storedValue)
                || (usedOneULPAllowance
                    && differsByOneBinary64ULP(previousProjection, storedValue))
            guard !previousCollides else {
                throw SpreadsheetNumericTimestampDecodeError.adjacentTickCollision
            }
        }
        if !nextOverflow {
            let nextProjection = try canonicalProjection(
                MicrosecondTick(microseconds: nextMicroseconds),
                unit: sourceUnit
            )
            let nextCollides = sameStoredBinary64(nextProjection, storedValue)
                || (usedOneULPAllowance
                    && differsByOneBinary64ULP(nextProjection, storedValue))
            guard !nextCollides else {
                throw SpreadsheetNumericTimestampDecodeError.adjacentTickCollision
            }
        }

        // Canonical projection is monotone. Exact equality or the bounded one-ULP qualification at
        // the candidate, together with rejection of each in-domain adjacent tick under the same
        // applicable rule, proves global uniqueness.
        return candidate
    }

    private static func validatedStoredValue(_ rawLexeme: String) throws -> Double {
        try validateGrammar(rawLexeme)
        guard let storedValue = Double(rawLexeme) else {
            throw SpreadsheetNumericTimestampDecodeError.binary64ReconstructionInvariant
        }
        guard storedValue.isFinite else {
            throw SpreadsheetNumericTimestampDecodeError.storedBinary64IsNotFinite
        }
        return storedValue
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

    private static func differsByOneBinary64ULP(_ lhs: Double, _ rhs: Double) -> Bool {
        guard lhs.isFinite, rhs.isFinite, !sameStoredBinary64(lhs, rhs) else {
            return false
        }
        return sameStoredBinary64(lhs.nextUp, rhs)
            || sameStoredBinary64(lhs.nextDown, rhs)
    }

    private static func isASCIIDigit(_ byte: UInt8) -> Bool {
        byte >= 0x30 && byte <= 0x39
    }
}
