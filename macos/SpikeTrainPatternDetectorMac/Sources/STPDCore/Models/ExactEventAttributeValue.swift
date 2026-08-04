import Foundation

public enum ExactEventAttributeSyntaxIssue: String, Equatable, Sendable {
    case missingMantissaDigits
    case missingExponentDigits
    case unexpectedCharacter
}

public enum ExactEventAttributeParseError: Error, Equatable, Sendable, LocalizedError {
    case empty
    case lexemeTooLong(maximumUTF8Bytes: Int)
    case invalidSyntax(utf8Offset: Int, issue: ExactEventAttributeSyntaxIssue)
    case exponentOutOfRange
    case normalizedExponentOutOfRange

    public var errorDescription: String? {
        switch self {
        case .empty:
            return "Attribute value text is empty."
        case .lexemeTooLong(let maximumUTF8Bytes):
            return "Attribute value text exceeds the maximum of \(maximumUTF8Bytes) UTF-8 bytes."
        case .invalidSyntax(let utf8Offset, let issue):
            return "Invalid attribute value syntax at UTF-8 byte offset \(utf8Offset): \(issue.rawValue)."
        case .exponentOutOfRange:
            return "Attribute exponent magnitude exceeds the exact parser's capacity."
        case .normalizedExponentOutOfRange:
            return "Canonical attribute exponent is outside the signed 64-bit range."
        }
    }
}

public enum CanonicalStringValueError: Error, Equatable, Sendable {
    case sourceTooLong(maximumUTF8Bytes: Int)
    case canonicalValueTooLong(maximumUTF8Bytes: Int)
}

public struct CanonicalStringValue: Hashable, Sendable {
    /// Event text may be substantially larger than identifier-like fields, but remains bounded
    /// before and after NFC normalization.
    public static let maximumSourceUTF8ByteCount = 65_536
    public static let maximumCanonicalUTF8ByteCount = 65_536

    public let canonicalText: String

    public init(validating source: String) throws {
        guard !eventAttributeTextExceedsUTF8ByteLimit(
            source,
            maximum: Self.maximumSourceUTF8ByteCount
        ) else {
            throw CanonicalStringValueError.sourceTooLong(
                maximumUTF8Bytes: Self.maximumSourceUTF8ByteCount
            )
        }
        let canonical = source.precomposedStringWithCanonicalMapping
        guard !eventAttributeTextExceedsUTF8ByteLimit(
            canonical,
            maximum: Self.maximumCanonicalUTF8ByteCount
        ) else {
            throw CanonicalStringValueError.canonicalValueTooLong(
                maximumUTF8Bytes: Self.maximumCanonicalUTF8ByteCount
            )
        }
        self.canonicalText = canonical
    }
}

/// An exact, arbitrarily large integer bounded only by its source-token length.
public struct ExactIntegerValue: Hashable, Sendable {
    public static let maximumLexemeUTF8ByteCount = 128

    public let isNegative: Bool
    public let magnitudeDigits: String
    public let canonicalText: String

    private init(isNegative: Bool, magnitudeDigits: String) {
        self.isNegative = isNegative
        self.magnitudeDigits = magnitudeDigits
        self.canonicalText = isNegative ? "-\(magnitudeDigits)" : magnitudeDigits
    }

    public static func parse(_ lexeme: String) throws -> ExactIntegerValue {
        let bounded = try ExactAttributeLexeme(
            lexeme,
            maximumUTF8ByteCount: maximumLexemeUTF8ByteCount
        )
        var index = bounded.start
        var isNegative = false
        if bounded.bytes[index] == ExactAttributeASCII.plus
            || bounded.bytes[index] == ExactAttributeASCII.minus {
            isNegative = bounded.bytes[index] == ExactAttributeASCII.minus
            index += 1
        }

        guard index < bounded.end else {
            throw ExactEventAttributeParseError.invalidSyntax(
                utf8Offset: index,
                issue: .missingMantissaDigits
            )
        }
        guard ExactAttributeASCII.isDigit(bounded.bytes[index]) else {
            throw ExactEventAttributeParseError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }

        let digitStart = index
        while index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) {
            index += 1
        }
        guard index == bounded.end else {
            throw ExactEventAttributeParseError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }

        let digitBytes = bounded.bytes[digitStart..<bounded.end]
        guard let firstNonzero = digitBytes.firstIndex(where: { $0 != ExactAttributeASCII.zero }) else {
            return ExactIntegerValue(isNegative: false, magnitudeDigits: "0")
        }
        let magnitude = String(decoding: digitBytes[firstNonzero...], as: UTF8.self)
        return ExactIntegerValue(isNegative: isNegative, magnitudeDigits: magnitude)
    }
}

/// A finite exact decimal represented as `coefficientDigits × 10^base10Exponent` plus a sign.
/// Every nonzero coefficient is ASCII and has neither leading nor trailing zeroes.
public struct ExactDecimalValue: Hashable, Sendable {
    public static let maximumLexemeUTF8ByteCount = 128

    public let isNegative: Bool
    public let coefficientDigits: String
    public let base10Exponent: Int64
    public let canonicalText: String

    private init(
        isNegative: Bool,
        coefficientDigits: String,
        base10Exponent: Int64
    ) {
        self.isNegative = isNegative
        self.coefficientDigits = coefficientDigits
        self.base10Exponent = base10Exponent
        let sign = isNegative ? "-" : ""
        if base10Exponent == 0 {
            self.canonicalText = "\(sign)\(coefficientDigits)"
        } else {
            self.canonicalText = "\(sign)\(coefficientDigits)e\(base10Exponent)"
        }
    }

    public static func parse(_ lexeme: String) throws -> ExactDecimalValue {
        let bounded = try ExactAttributeLexeme(
            lexeme,
            maximumUTF8ByteCount: maximumLexemeUTF8ByteCount
        )
        var index = bounded.start
        var isNegative = false
        if bounded.bytes[index] == ExactAttributeASCII.plus
            || bounded.bytes[index] == ExactAttributeASCII.minus {
            isNegative = bounded.bytes[index] == ExactAttributeASCII.minus
            index += 1
        }

        var coefficient: [UInt8] = []
        coefficient.reserveCapacity(bounded.end - index)
        var fractionalDigitCount = 0

        if index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) {
            while index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) {
                coefficient.append(bounded.bytes[index])
                index += 1
            }
            if index < bounded.end, bounded.bytes[index] == ExactAttributeASCII.decimalPoint {
                index += 1
                while index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) {
                    coefficient.append(bounded.bytes[index])
                    fractionalDigitCount += 1
                    index += 1
                }
            }
        } else if index < bounded.end,
                  bounded.bytes[index] == ExactAttributeASCII.decimalPoint {
            let decimalPointOffset = index
            index += 1
            guard index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) else {
                throw ExactEventAttributeParseError.invalidSyntax(
                    utf8Offset: index < bounded.end ? index : decimalPointOffset + 1,
                    issue: .missingMantissaDigits
                )
            }
            while index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) {
                coefficient.append(bounded.bytes[index])
                fractionalDigitCount += 1
                index += 1
            }
        } else {
            if index == bounded.end {
                throw ExactEventAttributeParseError.invalidSyntax(
                    utf8Offset: index,
                    issue: .missingMantissaDigits
                )
            }
            throw ExactEventAttributeParseError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }

        guard !coefficient.isEmpty else {
            throw ExactEventAttributeParseError.invalidSyntax(
                utf8Offset: index,
                issue: .missingMantissaDigits
            )
        }

        var exponentIsNegative = false
        var exponentMagnitude: UInt64 = 0
        var exponentOverflow = false
        if index < bounded.end,
           (bounded.bytes[index] == ExactAttributeASCII.lowercaseE
            || bounded.bytes[index] == ExactAttributeASCII.uppercaseE) {
            index += 1
            if index < bounded.end,
               (bounded.bytes[index] == ExactAttributeASCII.plus
                || bounded.bytes[index] == ExactAttributeASCII.minus) {
                exponentIsNegative = bounded.bytes[index] == ExactAttributeASCII.minus
                index += 1
            }
            guard index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) else {
                throw ExactEventAttributeParseError.invalidSyntax(
                    utf8Offset: index,
                    issue: .missingExponentDigits
                )
            }
            while index < bounded.end, ExactAttributeASCII.isDigit(bounded.bytes[index]) {
                if !exponentOverflow {
                    let digit = UInt64(bounded.bytes[index] - ExactAttributeASCII.zero)
                    let (timesTen, multiplicationOverflow) = exponentMagnitude
                        .multipliedReportingOverflow(by: 10)
                    let (next, additionOverflow) = timesTen.addingReportingOverflow(digit)
                    if multiplicationOverflow || additionOverflow {
                        exponentOverflow = true
                    } else {
                        exponentMagnitude = next
                    }
                }
                index += 1
            }
        }

        guard index == bounded.end else {
            throw ExactEventAttributeParseError.invalidSyntax(
                utf8Offset: index,
                issue: .unexpectedCharacter
            )
        }

        // Syntax is fully validated before zero normalization. A zero value does not need a
        // representable exponent, so even a syntactically valid enormous exponent canonicalizes to 0.
        guard let firstNonzero = coefficient.firstIndex(where: {
            $0 != ExactAttributeASCII.zero
        }) else {
            return ExactDecimalValue(
                isNegative: false,
                coefficientDigits: "0",
                base10Exponent: 0
            )
        }

        guard !exponentOverflow else {
            throw ExactEventAttributeParseError.exponentOutOfRange
        }
        let lastNonzero = coefficient.lastIndex(where: {
            $0 != ExactAttributeASCII.zero
        })!
        let trailingZeroCount = coefficient.distance(
            from: coefficient.index(after: lastNonzero),
            to: coefficient.endIndex
        )
        let adjustment = Int64(trailingZeroCount) - Int64(fractionalDigitCount)
        let normalizedExponent = try normalizedExponent(
            rawMagnitude: exponentMagnitude,
            rawIsNegative: exponentIsNegative,
            adjustment: adjustment
        )

        let canonicalCoefficient = String(
            decoding: coefficient[firstNonzero...lastNonzero],
            as: UTF8.self
        )
        return ExactDecimalValue(
            isNegative: isNegative,
            coefficientDigits: canonicalCoefficient,
            base10Exponent: normalizedExponent
        )
    }

    /// Adds the small mantissa-derived adjustment to an exact signed-magnitude exponent. The raw
    /// exponent need not itself fit Int64; only the normalized result must fit.
    private static func normalizedExponent(
        rawMagnitude: UInt64,
        rawIsNegative: Bool,
        adjustment: Int64
    ) throws -> Int64 {
        let adjustmentIsNegative = adjustment < 0
        let adjustmentMagnitude = adjustment.magnitude
        let resultIsNegative: Bool
        let resultMagnitude: UInt64

        if rawIsNegative == adjustmentIsNegative {
            let (sum, overflow) = rawMagnitude.addingReportingOverflow(adjustmentMagnitude)
            guard !overflow else {
                throw ExactEventAttributeParseError.normalizedExponentOutOfRange
            }
            resultIsNegative = rawIsNegative
            resultMagnitude = sum
        } else if rawMagnitude >= adjustmentMagnitude {
            resultIsNegative = rawIsNegative
            resultMagnitude = rawMagnitude - adjustmentMagnitude
        } else {
            resultIsNegative = adjustmentIsNegative
            resultMagnitude = adjustmentMagnitude - rawMagnitude
        }

        if resultIsNegative {
            let negativeLimit = UInt64(Int64.max) + 1
            guard resultMagnitude <= negativeLimit else {
                throw ExactEventAttributeParseError.normalizedExponentOutOfRange
            }
            if resultMagnitude == negativeLimit { return .min }
            return -Int64(resultMagnitude)
        }
        guard resultMagnitude <= UInt64(Int64.max) else {
            throw ExactEventAttributeParseError.normalizedExponentOutOfRange
        }
        return Int64(resultMagnitude)
    }
}

public enum EventAttributeScalarType: String, CaseIterable, Hashable, Sendable {
    case string
    case integer
    case exactDecimal = "exact_decimal"
    case boolean
}

public enum EventAttributeValue: Hashable, Sendable {
    case string(CanonicalStringValue)
    case integer(ExactIntegerValue)
    case exactDecimal(ExactDecimalValue)
    case boolean(Bool)

    public var scalarType: EventAttributeScalarType {
        switch self {
        case .string:
            return .string
        case .integer:
            return .integer
        case .exactDecimal:
            return .exactDecimal
        case .boolean:
            return .boolean
        }
    }
}

public enum OpaqueUnitSymbolError: Error, Equatable, Sendable {
    case sourceTooLong(maximumUTF8Bytes: Int)
    case canonicalValueTooLong(maximumUTF8Bytes: Int)
    case empty
    case blank
    case containsControlCharacter
}

public struct OpaqueUnitSymbol: Hashable, Sendable {
    /// Bounds both untrusted source text and its stored NFC form.
    public static let maximumSourceUTF8ByteCount = 256
    public static let maximumCanonicalUTF8ByteCount = 256

    public let canonicalText: String

    public init(validating source: String) throws {
        guard !eventAttributeTextExceedsUTF8ByteLimit(
            source,
            maximum: Self.maximumSourceUTF8ByteCount
        ) else {
            throw OpaqueUnitSymbolError.sourceTooLong(
                maximumUTF8Bytes: Self.maximumSourceUTF8ByteCount
            )
        }
        let canonical = source.precomposedStringWithCanonicalMapping
        guard !eventAttributeTextExceedsUTF8ByteLimit(
            canonical,
            maximum: Self.maximumCanonicalUTF8ByteCount
        ) else {
            throw OpaqueUnitSymbolError.canonicalValueTooLong(
                maximumUTF8Bytes: Self.maximumCanonicalUTF8ByteCount
            )
        }
        guard !canonical.isEmpty else { throw OpaqueUnitSymbolError.empty }
        guard eventAttributeTextContainsNonWhitespaceScalar(canonical) else {
            throw OpaqueUnitSymbolError.blank
        }
        guard !canonical.unicodeScalars.contains(where: {
            CharacterSet.controlCharacters.contains($0)
        }) else {
            throw OpaqueUnitSymbolError.containsControlCharacter
        }
        self.canonicalText = canonical
    }
}

public enum ConfirmedEventAttributeUnit: Hashable, Sendable {
    case notApplicable
    case dimensionless
    case specified(OpaqueUnitSymbol)
}

private struct ExactAttributeLexeme {
    let bytes: [UInt8]
    let start: Int
    let end: Int

    init(_ source: String, maximumUTF8ByteCount: Int) throws {
        guard source.utf8.prefix(maximumUTF8ByteCount + 1).count
                <= maximumUTF8ByteCount else {
            throw ExactEventAttributeParseError.lexemeTooLong(
                maximumUTF8Bytes: maximumUTF8ByteCount
            )
        }
        let bytes = Array(source.utf8)
        var start = 0
        var end = bytes.count
        while start < end, ExactAttributeASCII.isTrimmable(bytes[start]) { start += 1 }
        while end > start, ExactAttributeASCII.isTrimmable(bytes[end - 1]) { end -= 1 }
        guard start < end else { throw ExactEventAttributeParseError.empty }
        self.bytes = bytes
        self.start = start
        self.end = end
    }
}

private func eventAttributeTextExceedsUTF8ByteLimit(
    _ value: String,
    maximum: Int
) -> Bool {
    value.utf8.prefix(maximum + 1).count > maximum
}

private func eventAttributeTextContainsNonWhitespaceScalar(_ value: String) -> Bool {
    value.unicodeScalars.contains {
        !CharacterSet.whitespacesAndNewlines.contains($0)
    }
}

private enum ExactAttributeASCII {
    static let zero: UInt8 = 0x30
    static let plus: UInt8 = 0x2B
    static let minus: UInt8 = 0x2D
    static let decimalPoint: UInt8 = 0x2E
    static let lowercaseE: UInt8 = 0x65
    static let uppercaseE: UInt8 = 0x45

    static func isDigit(_ byte: UInt8) -> Bool {
        byte >= zero && byte <= 0x39
    }

    static func isTrimmable(_ byte: UInt8) -> Bool {
        byte == 0x20 || byte == 0x09
    }
}
