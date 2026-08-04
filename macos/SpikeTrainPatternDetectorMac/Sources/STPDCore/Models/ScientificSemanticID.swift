import Foundation

public enum ScientificSemanticIDError: Error, Equatable, Sendable {
    case sourceTooLong(maximumUTF8Bytes: Int)
    case canonicalValueTooLong(maximumUTF8Bytes: Int)
    case empty
    case blank
    case containsControlCharacter
}

/// A stable, case-sensitive semantic identifier stored in Unicode NFC.
///
/// This type never generates identifiers. Values must be installed from a confirmed or reused
/// manifest mapping. A UI may suggest label-derived text, but it must not become authoritative
/// without explicit confirmation; runtime UUIDs, source positions, and transient array indices
/// are never semantic identities. Assignment provenance is enforced by the later manifest validator.
public struct ScientificSemanticID: Hashable, Sendable {
    /// Bounds both untrusted source text and its stored NFC form.
    public static let maximumSourceUTF8ByteCount = 256
    public static let maximumCanonicalUTF8ByteCount = 256

    public let canonicalText: String

    public init(validating source: String) throws {
        guard !exceedsUTF8ByteLimit(
            source,
            maximum: Self.maximumSourceUTF8ByteCount
        ) else {
            throw ScientificSemanticIDError.sourceTooLong(
                maximumUTF8Bytes: Self.maximumSourceUTF8ByteCount
            )
        }
        let canonical = source.precomposedStringWithCanonicalMapping
        guard !exceedsUTF8ByteLimit(
            canonical,
            maximum: Self.maximumCanonicalUTF8ByteCount
        ) else {
            throw ScientificSemanticIDError.canonicalValueTooLong(
                maximumUTF8Bytes: Self.maximumCanonicalUTF8ByteCount
            )
        }
        guard !canonical.isEmpty else { throw ScientificSemanticIDError.empty }
        guard containsNonWhitespaceScalar(canonical) else {
            throw ScientificSemanticIDError.blank
        }
        guard !canonical.unicodeScalars.contains(where: {
            CharacterSet.controlCharacters.contains($0)
        }) else {
            throw ScientificSemanticIDError.containsControlCharacter
        }
        self.canonicalText = canonical
    }
}

public enum EventAttributeKeyError: Error, Equatable, Sendable {
    case sourceTooLong(maximumUTF8Bytes: Int)
    case canonicalValueTooLong(maximumUTF8Bytes: Int)
    case empty
    case blank
    case containsControlCharacter
    case containsMetadataSeparator
    case reservedUnitSuffix
}

/// A case-sensitive, NFC event-attribute key. ASCII `=` is the unescaped metadata separator and
/// cannot occur in a key. A key ending in `.unit` is reserved for an import-time unit suggestion
/// and cannot be installed as an independent scalar attribute.
public struct EventAttributeKey: Hashable, Sendable {
    /// Bounds both untrusted source text and its stored NFC form.
    public static let maximumSourceUTF8ByteCount = 256
    public static let maximumCanonicalUTF8ByteCount = 256

    public let canonicalText: String

    public init(validating source: String) throws {
        guard !exceedsUTF8ByteLimit(
            source,
            maximum: Self.maximumSourceUTF8ByteCount
        ) else {
            throw EventAttributeKeyError.sourceTooLong(
                maximumUTF8Bytes: Self.maximumSourceUTF8ByteCount
            )
        }
        let canonical = source.precomposedStringWithCanonicalMapping
        guard !exceedsUTF8ByteLimit(
            canonical,
            maximum: Self.maximumCanonicalUTF8ByteCount
        ) else {
            throw EventAttributeKeyError.canonicalValueTooLong(
                maximumUTF8Bytes: Self.maximumCanonicalUTF8ByteCount
            )
        }
        guard !canonical.isEmpty else { throw EventAttributeKeyError.empty }
        guard containsNonWhitespaceScalar(canonical) else {
            throw EventAttributeKeyError.blank
        }
        guard !canonical.unicodeScalars.contains(where: {
            CharacterSet.controlCharacters.contains($0)
        }) else {
            throw EventAttributeKeyError.containsControlCharacter
        }
        guard !canonical.contains("=") else {
            throw EventAttributeKeyError.containsMetadataSeparator
        }
        guard !canonical.hasSuffix(".unit") else {
            throw EventAttributeKeyError.reservedUnitSuffix
        }
        self.canonicalText = canonical
    }
}

private func exceedsUTF8ByteLimit(_ value: String, maximum: Int) -> Bool {
    value.utf8.prefix(maximum + 1).count > maximum
}

private func containsNonWhitespaceScalar(_ value: String) -> Bool {
    value.unicodeScalars.contains {
        !CharacterSet.whitespacesAndNewlines.contains($0)
    }
}
