import Foundation

enum CanonicalXLSXStringDecodingLimitKind: String, Hashable, Sendable {
    case rawTextNodeUTF8Bytes
    case decodedTextUTF8Bytes
}

enum CanonicalXLSXStringEscapeIssue: Hashable, Sendable {
    case nonCanonicalEscape(codeUnit: UInt16)
    case misplacedLiteralUnderscoreEscape
    case surrogateEscapeUnsupported(codeUnit: UInt16)
    case directCarriageReturnUnsupported
}

enum CanonicalXLSXStringDecodingError: Error, Hashable, Sendable {
    case invalidLimit(kind: CanonicalXLSXStringDecodingLimitKind, actual: Int)
    case limitExceedsSupportedEnvelope(
        kind: CanonicalXLSXStringDecodingLimitKind,
        maximumSupported: Int,
        actual: Int
    )
    case rawTextNodeLimitExceeded(maximum: Int, actual: Int)
    case decodedTextLimitExceeded(maximum: Int, actual: Int)
    case decodedTextByteCountOverflow(maximum: Int)
    case unsupportedEscape(CanonicalXLSXStringEscapeIssue)
}

/// Strict, single-pass decoding of one SpreadsheetML `ST_Xstring` text node.
///
/// Each `<t>` is decoded independently. Callers concatenate already-decoded rich-text runs, so a
/// token can never be assembled across run boundaries. This decoder intentionally rejects valid
/// XML scalar escapes because accepting them could silently turn display text into timestamp or
/// event-attribute syntax.
enum CanonicalXLSXStringDecoder {
    static let supportedMaximumRawTextNodeUTF8ByteCount: Int = {
        let (value, overflow) = 7.multipliedReportingOverflow(
            by: CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount
        )
        precondition(!overflow)
        return value
    }()

    static func decodeTextNode(
        _ source: String,
        maximumRawUTF8ByteCount: Int = supportedMaximumRawTextNodeUTF8ByteCount,
        maximumDecodedUTF8ByteCount: Int =
            CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount
    ) throws -> String {
        try validateLimits(
            maximumRawUTF8ByteCount: maximumRawUTF8ByteCount,
            maximumDecodedUTF8ByteCount: maximumDecodedUTF8ByteCount
        )

        let rawByteCount = source.utf8.count
        guard rawByteCount <= maximumRawUTF8ByteCount else {
            throw CanonicalXLSXStringDecodingError.rawTextNodeLimitExceeded(
                maximum: maximumRawUTF8ByteCount,
                actual: rawByteCount
            )
        }

        let scalars = Array(source.unicodeScalars)
        var result = String()
        result.reserveCapacity(min(source.count, maximumDecodedUTF8ByteCount))
        var decodedByteCount = 0
        var offset = 0

        while offset < scalars.count {
            let scalar = scalars[offset]
            if scalar.value == 0x0D {
                throw CanonicalXLSXStringDecodingError.unsupportedEscape(
                    .directCarriageReturnUnsupported
                )
            }

            if let codeUnit = recognizedToken(in: scalars, at: offset) {
                switch codeUnit {
                case 0x005F:
                    guard isProtectedLiteralTokenSuffix(in: scalars, at: offset + 7) else {
                        throw CanonicalXLSXStringDecodingError.unsupportedEscape(
                            .misplacedLiteralUnderscoreEscape
                        )
                    }
                    // Consume the protected six-character suffix as part of this escape. Merely
                    // emitting `_` and resuming at the suffix would allow its trailing underscore
                    // to overlap with another token, accidentally performing recursive decoding.
                    for protectedScalar in [Unicode.Scalar(0x5F)!]
                        + Array(scalars[(offset + 7)...(offset + 12)]) {
                        try append(
                            protectedScalar,
                            to: &result,
                            decodedByteCount: &decodedByteCount,
                            maximumDecodedUTF8ByteCount: maximumDecodedUTF8ByteCount
                        )
                    }
                    offset += 13
                    continue
                case 0x000D:
                    try append(
                        Unicode.Scalar(0x0D)!,
                        to: &result,
                        decodedByteCount: &decodedByteCount,
                        maximumDecodedUTF8ByteCount: maximumDecodedUTF8ByteCount
                    )
                case 0xD800...0xDFFF:
                    throw CanonicalXLSXStringDecodingError.unsupportedEscape(
                        .surrogateEscapeUnsupported(codeUnit: codeUnit)
                    )
                case 0x0000...0x0008, 0x000B, 0x000C, 0x000E...0x001F, 0xFFFE, 0xFFFF:
                    guard let decoded = Unicode.Scalar(UInt32(codeUnit)) else {
                        throw CanonicalXLSXStringDecodingError.unsupportedEscape(
                            .nonCanonicalEscape(codeUnit: codeUnit)
                        )
                    }
                    try append(
                        decoded,
                        to: &result,
                        decodedByteCount: &decodedByteCount,
                        maximumDecodedUTF8ByteCount: maximumDecodedUTF8ByteCount
                    )
                default:
                    throw CanonicalXLSXStringDecodingError.unsupportedEscape(
                        .nonCanonicalEscape(codeUnit: codeUnit)
                    )
                }
                offset += 7
                continue
            }

            try append(
                scalar,
                to: &result,
                decodedByteCount: &decodedByteCount,
                maximumDecodedUTF8ByteCount: maximumDecodedUTF8ByteCount
            )
            offset += 1
        }
        return result
    }

    /// Decodes independently delimited `<t>` nodes, then applies the cell-wide decoded-text cap.
    /// This is the entry point for plain, shared, inline, and rich-string cell assembly.
    static func decodeTextNodes(
        _ sources: [String],
        maximumRawTextNodeUTF8ByteCount: Int = supportedMaximumRawTextNodeUTF8ByteCount,
        maximumDecodedCellUTF8ByteCount: Int =
            CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount
    ) throws -> String {
        try validateLimits(
            maximumRawUTF8ByteCount: maximumRawTextNodeUTF8ByteCount,
            maximumDecodedUTF8ByteCount: maximumDecodedCellUTF8ByteCount
        )
        var result = String()
        var decodedCellByteCount = 0
        for source in sources {
            let decoded = try decodeTextNode(
                source,
                maximumRawUTF8ByteCount: maximumRawTextNodeUTF8ByteCount,
                maximumDecodedUTF8ByteCount: maximumDecodedCellUTF8ByteCount
            )
            let (proposed, overflow) = decodedCellByteCount.addingReportingOverflow(
                decoded.utf8.count
            )
            guard !overflow else {
                throw CanonicalXLSXStringDecodingError.decodedTextByteCountOverflow(
                    maximum: maximumDecodedCellUTF8ByteCount
                )
            }
            guard proposed <= maximumDecodedCellUTF8ByteCount else {
                throw CanonicalXLSXStringDecodingError.decodedTextLimitExceeded(
                    maximum: maximumDecodedCellUTF8ByteCount,
                    actual: proposed
                )
            }
            result.append(decoded)
            decodedCellByteCount = proposed
        }
        return result
    }

    private static func validateLimits(
        maximumRawUTF8ByteCount: Int,
        maximumDecodedUTF8ByteCount: Int
    ) throws {
        guard maximumRawUTF8ByteCount > 0 else {
            throw CanonicalXLSXStringDecodingError.invalidLimit(
                kind: .rawTextNodeUTF8Bytes,
                actual: maximumRawUTF8ByteCount
            )
        }
        guard maximumDecodedUTF8ByteCount > 0 else {
            throw CanonicalXLSXStringDecodingError.invalidLimit(
                kind: .decodedTextUTF8Bytes,
                actual: maximumDecodedUTF8ByteCount
            )
        }
        guard maximumRawUTF8ByteCount <= supportedMaximumRawTextNodeUTF8ByteCount else {
            throw CanonicalXLSXStringDecodingError.limitExceedsSupportedEnvelope(
                kind: .rawTextNodeUTF8Bytes,
                maximumSupported: supportedMaximumRawTextNodeUTF8ByteCount,
                actual: maximumRawUTF8ByteCount
            )
        }
        let supportedDecodedMaximum =
            CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount
        guard maximumDecodedUTF8ByteCount <= supportedDecodedMaximum else {
            throw CanonicalXLSXStringDecodingError.limitExceedsSupportedEnvelope(
                kind: .decodedTextUTF8Bytes,
                maximumSupported: supportedDecodedMaximum,
                actual: maximumDecodedUTF8ByteCount
            )
        }
    }

    private static func recognizedToken(
        in scalars: [Unicode.Scalar],
        at offset: Int
    ) -> UInt16? {
        guard offset <= scalars.count - 7,
              scalars[offset].value == 0x5F,
              scalars[offset + 1].value == 0x78,
              scalars[offset + 6].value == 0x5F else { return nil }
        var value: UInt16 = 0
        for index in (offset + 2)...(offset + 5) {
            guard let nibble = hexadecimalNibble(scalars[index]) else { return nil }
            value = (value << 4) | UInt16(nibble)
        }
        return value
    }

    private static func isProtectedLiteralTokenSuffix(
        in scalars: [Unicode.Scalar],
        at offset: Int
    ) -> Bool {
        guard offset <= scalars.count - 6,
              scalars[offset].value == 0x78,
              scalars[offset + 5].value == 0x5F else { return false }
        return ((offset + 1)...(offset + 4)).allSatisfy {
            hexadecimalNibble(scalars[$0]) != nil
        }
    }

    private static func hexadecimalNibble(_ scalar: Unicode.Scalar) -> UInt8? {
        switch scalar.value {
        case 0x30...0x39: UInt8(scalar.value - 0x30)
        case 0x41...0x46: UInt8(scalar.value - 0x41 + 10)
        case 0x61...0x66: UInt8(scalar.value - 0x61 + 10)
        default: nil
        }
    }

    private static func append(
        _ scalar: Unicode.Scalar,
        to result: inout String,
        decodedByteCount: inout Int,
        maximumDecodedUTF8ByteCount: Int
    ) throws {
        let scalarByteCount = scalar.utf8.count
        let (proposed, overflow) = decodedByteCount.addingReportingOverflow(scalarByteCount)
        guard !overflow else {
            throw CanonicalXLSXStringDecodingError.decodedTextByteCountOverflow(
                maximum: maximumDecodedUTF8ByteCount
            )
        }
        guard proposed <= maximumDecodedUTF8ByteCount else {
            throw CanonicalXLSXStringDecodingError.decodedTextLimitExceeded(
                maximum: maximumDecodedUTF8ByteCount,
                actual: proposed
            )
        }
        result.unicodeScalars.append(scalar)
        decodedByteCount = proposed
    }
}
