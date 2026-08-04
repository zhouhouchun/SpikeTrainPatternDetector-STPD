import Foundation

/// A source-located parser can add sheet, cell, column, and line provenance around these stable
/// lexical errors without retaining an arbitrarily large source token in the error value.
public enum ExactTimestampParseError: Error, Equatable, Sendable, LocalizedError {
    case empty
    case lexemeTooLong(maximumUTF8Bytes: Int)
    case invalidSyntax(utf8Offset: Int, issue: ExactTimestampSyntaxIssue)
    case notExactlyRepresentableInMicroseconds
    case outsideSignedMicrosecondRange

    public var errorDescription: String? {
        switch self {
        case .empty:
            return "Timestamp text is empty."
        case .lexemeTooLong(let maximumUTF8Bytes):
            return "Timestamp text exceeds the maximum of \(maximumUTF8Bytes) UTF-8 bytes."
        case .invalidSyntax(let utf8Offset, let issue):
            return "Invalid timestamp syntax at UTF-8 byte offset \(utf8Offset): \(issue.rawValue)."
        case .notExactlyRepresentableInMicroseconds:
            return "Timestamp is not exactly representable in whole microseconds."
        case .outsideSignedMicrosecondRange:
            return "Timestamp is outside the signed 64-bit microsecond range."
        }
    }
}

public enum ExactTimestampSyntaxIssue: String, Equatable, Sendable {
    case missingMantissaDigits
    case missingExponentDigits
    case unexpectedCharacter
}

/// Exact text conversion for the canonical signed-microsecond grid.
///
/// Accepted ASCII grammar:
/// `sign? (digits ("." digits?)? | "." digits) (("e" | "E") sign? digits)?`
/// Leading and trailing ASCII space or tab are ignored. No other whitespace is ignored.
public enum ExactTimestampTextCodec {
    public static let maximumLexemeUTF8ByteCount = 128

    private static let positiveLimit = "9223372036854775807".utf8.map { $0 - 0x30 }
    private static let negativeMagnitudeLimit = "9223372036854775808".utf8.map { $0 - 0x30 }

    // A token cannot contain enough mantissa digits to cancel an exponent above this bound.
    // Deriving it from the lexeme limit keeps that invariant intact if the input limit changes.
    private static let decisiveExponentMagnitude = maximumLexemeUTF8ByteCount + 32

    public static func decode(
        _ lexeme: String,
        sourceUnit: SpikeTimeUnit
    ) throws -> MicrosecondTick {
        // Inspect no more than the first byte beyond the accepted bound. The limit is deliberately
        // applied to the raw token before trimming.
        guard lexeme.utf8.prefix(maximumLexemeUTF8ByteCount + 1).count
                <= maximumLexemeUTF8ByteCount else {
            throw ExactTimestampParseError.lexemeTooLong(
                maximumUTF8Bytes: maximumLexemeUTF8ByteCount
            )
        }

        let bytes = Array(lexeme.utf8)
        var start = 0
        var end = bytes.count
        while start < end, isTrimmableASCII(bytes[start]) { start += 1 }
        while end > start, isTrimmableASCII(bytes[end - 1]) { end -= 1 }
        guard start < end else { throw ExactTimestampParseError.empty }

        var index = start
        var isNegative = false
        if bytes[index] == 0x2B || bytes[index] == 0x2D { // + or -
            isNegative = bytes[index] == 0x2D
            index += 1
        }

        var coefficientDigits: [UInt8] = []
        coefficientDigits.reserveCapacity(end - index)
        var fractionalDigitCount = 0

        if index < end, isASCIIDigit(bytes[index]) {
            while index < end, isASCIIDigit(bytes[index]) {
                coefficientDigits.append(bytes[index] - 0x30)
                index += 1
            }
            if index < end, bytes[index] == 0x2E { // .
                index += 1
                while index < end, isASCIIDigit(bytes[index]) {
                    coefficientDigits.append(bytes[index] - 0x30)
                    fractionalDigitCount += 1
                    index += 1
                }
            }
        } else if index < end, bytes[index] == 0x2E { // .
            let decimalPointOffset = index
            index += 1
            guard index < end, isASCIIDigit(bytes[index]) else {
                let offset = index < end ? index : decimalPointOffset + 1
                throw ExactTimestampParseError.invalidSyntax(
                    utf8Offset: offset,
                    issue: .missingMantissaDigits
                )
            }
            while index < end, isASCIIDigit(bytes[index]) {
                coefficientDigits.append(bytes[index] - 0x30)
                fractionalDigitCount += 1
                index += 1
            }
        } else {
            if index == end {
                throw ExactTimestampParseError.invalidSyntax(
                    utf8Offset: index,
                    issue: .missingMantissaDigits
                )
            }
            throw ExactTimestampParseError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }

        guard !coefficientDigits.isEmpty else {
            throw ExactTimestampParseError.invalidSyntax(
                utf8Offset: index,
                issue: .missingMantissaDigits
            )
        }

        var exponentSign = 1
        var exponentMagnitude = 0
        var exponentSaturated = false
        if index < end, bytes[index] == 0x65 || bytes[index] == 0x45 { // e or E
            index += 1
            if index < end, bytes[index] == 0x2B || bytes[index] == 0x2D {
                exponentSign = bytes[index] == 0x2D ? -1 : 1
                index += 1
            }

            guard index < end, isASCIIDigit(bytes[index]) else {
                throw ExactTimestampParseError.invalidSyntax(
                    utf8Offset: index,
                    issue: .missingExponentDigits
                )
            }

            while index < end, isASCIIDigit(bytes[index]) {
                let digit = Int(bytes[index] - 0x30)
                if !exponentSaturated {
                    let (timesTen, multiplicationOverflow) = exponentMagnitude
                        .multipliedReportingOverflow(by: 10)
                    let (next, additionOverflow) = timesTen.addingReportingOverflow(digit)
                    if multiplicationOverflow || additionOverflow
                        || next > decisiveExponentMagnitude {
                        exponentMagnitude = decisiveExponentMagnitude + 1
                        exponentSaturated = true
                    } else {
                        exponentMagnitude = next
                    }
                }
                index += 1
            }
        }

        guard index == end else {
            throw ExactTimestampParseError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }

        // All spellings of signed zero have one canonical value, even with an enormous exponent.
        guard coefficientDigits.contains(where: { $0 != 0 }) else { return .zero }

        if exponentSaturated {
            if exponentSign > 0 {
                throw ExactTimestampParseError.outsideSignedMicrosecondRange
            }
            throw ExactTimestampParseError.notExactlyRepresentableInMicroseconds
        }

        let signedExponent = exponentSign * exponentMagnitude
        let unitPower: Int
        switch sourceUnit {
        case .seconds:
            unitPower = 6
        case .milliseconds:
            unitPower = 3
        }
        let effectivePower = signedExponent - fractionalDigitCount + unitPower

        let firstNonzero = coefficientDigits.firstIndex(where: { $0 != 0 })!
        var normalizedDigits = Array(coefficientDigits[firstNonzero...])

        if effectivePower < 0 {
            let requiredTrailingZeros = -effectivePower
            guard requiredTrailingZeros <= normalizedDigits.count else {
                throw ExactTimestampParseError.notExactlyRepresentableInMicroseconds
            }
            let retainedCount = normalizedDigits.count - requiredTrailingZeros
            guard normalizedDigits[retainedCount...].allSatisfy({ $0 == 0 }) else {
                throw ExactTimestampParseError.notExactlyRepresentableInMicroseconds
            }
            normalizedDigits.removeLast(requiredTrailingZeros)
        } else if effectivePower > 0 {
            guard normalizedDigits.count + effectivePower <= positiveLimit.count else {
                throw ExactTimestampParseError.outsideSignedMicrosecondRange
            }
            normalizedDigits.append(contentsOf: repeatElement(0, count: effectivePower))
        }

        let limit = isNegative ? negativeMagnitudeLimit : positiveLimit
        guard normalizedDigits.count < limit.count
                || (normalizedDigits.count == limit.count
                    && normalizedDigits.lexicographicallyPrecedes(limit))
                || normalizedDigits == limit else {
            throw ExactTimestampParseError.outsideSignedMicrosecondRange
        }

        var magnitude: UInt64 = 0
        for digit in normalizedDigits {
            let (timesTen, multiplicationOverflow) = magnitude.multipliedReportingOverflow(by: 10)
            let (next, additionOverflow) = timesTen.addingReportingOverflow(UInt64(digit))
            guard !multiplicationOverflow, !additionOverflow else {
                throw ExactTimestampParseError.outsideSignedMicrosecondRange
            }
            magnitude = next
        }

        if isNegative {
            if magnitude == UInt64(Int64.max) + 1 {
                return MicrosecondTick(microseconds: .min)
            }
            return MicrosecondTick(microseconds: -Int64(magnitude))
        }
        return MicrosecondTick(microseconds: Int64(magnitude))
    }

    /// Returns numeric text only. Callers add a localized unit label separately.
    public static func encode(
        _ tick: MicrosecondTick,
        displayUnit: SpikeTimeUnit
    ) -> String {
        let divisor: UInt64
        let fractionalWidth: Int
        switch displayUnit {
        case .seconds:
            divisor = 1_000_000
            fractionalWidth = 6
        case .milliseconds:
            divisor = 1_000
            fractionalWidth = 3
        }

        let magnitude = tick.microseconds.magnitude
        let whole = magnitude / divisor
        let remainderText = String(magnitude % divisor)
        let padding = String(repeating: "0", count: fractionalWidth - remainderText.count)
        let sign = tick.microseconds < 0 ? "-" : ""
        return "\(sign)\(whole).\(padding)\(remainderText)"
    }

    private static func isASCIIDigit(_ byte: UInt8) -> Bool {
        byte >= 0x30 && byte <= 0x39
    }

    private static func isTrimmableASCII(_ byte: UInt8) -> Bool {
        byte == 0x20 || byte == 0x09
    }
}
