import CryptoKit
import Darwin
import Foundation

enum STPDStableIdentifier {
    static func make(prefix: String, domain: String, components: [String]) -> String {
        var hasher = SHA256()
        append(domain, to: &hasher)
        append(components.count, to: &hasher)
        for component in components {
            append(component, to: &hasher)
        }
        return "\(prefix)_\(hasher.finalize().map { String(format: "%02x", $0) }.joined())"
    }

    static func digest(_ data: Data) -> String {
        SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
    }

    private static func append(_ value: String, to hasher: inout SHA256) {
        let bytes = Array(value.utf8)
        append(bytes.count, to: &hasher)
        hasher.update(data: Data(bytes))
    }

    private static func append(_ value: Int, to hasher: inout SHA256) {
        var bigEndian = Int64(value).bigEndian
        withUnsafeBytes(of: &bigEndian) { bytes in
            hasher.update(data: Data(bytes))
        }
    }
}

struct STPDParsedTimestamp: Sendable {
    let seconds: Double
    let precisionTolerance: Double
    let hasFractionalSeconds: Bool
}

/// One canonical timestamp codec shared by CSV import and result-package validation.
///
/// Fractional instants before 1970 must be reconstructed as one signed decimal value. Adding a
/// positive fractional `Double` to a negative whole second loses a bit for values such as `-0.1`.
enum STPDCanonicalTimestamp {
    private static let posixLocale = Locale(identifier: "en_US_POSIX")

    static func string(_ value: Date?) -> String {
        guard let value, value.timeIntervalSince1970.isFinite else {
            return ""
        }
        let exactSeconds = value.timeIntervalSince1970
        let wholeSeconds = floor(exactSeconds)
        guard wholeSeconds >= Double(Int64.min),
              wholeSeconds < Double(Int64.max) else {
            return ""
        }
        let wholeInteger = Int64(wholeSeconds)
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime]
        let base = formatter.string(
            from: Date(timeIntervalSince1970: Double(wholeInteger))
        )
        guard base.hasSuffix("Z"),
              let total = Decimal(
                string: canonicalDouble(exactSeconds),
                locale: posixLocale
              ) else {
            return ""
        }
        let fraction = total - Decimal(wholeInteger)
        guard fraction >= 0, fraction < 1 else {
            return ""
        }
        let encoded: String
        if fraction == 0 {
            encoded = base
        } else {
            let fractionText = NSDecimalNumber(decimal: fraction)
                .description(withLocale: posixLocale)
            guard fractionText.hasPrefix("0.") else {
                return ""
            }
            encoded =
                String(base.dropLast()) +
                String(fractionText.dropFirst()) +
                "Z"
        }
        guard let parsed = parse(encoded),
              parsed.seconds.bitPattern == exactSeconds.bitPattern else {
            return ""
        }
        return encoded
    }

    static func parse(_ value: String) -> STPDParsedTimestamp? {
        let wholeFormatter = ISO8601DateFormatter()
        wholeFormatter.formatOptions = [.withInternetDateTime]
        guard let timeSeparator = value.firstIndex(of: "T"),
              let decimalPoint = value[timeSeparator...].firstIndex(of: ".") else {
            guard let date = wholeFormatter.date(from: value) else {
                return nil
            }
            return STPDParsedTimestamp(
                seconds: date.timeIntervalSince1970,
                precisionTolerance: 0.500_001,
                hasFractionalSeconds: false
            )
        }

        let suffix = value[value.index(after: decimalPoint)...]
        guard let zoneStart = suffix.firstIndex(where: {
            $0 == "Z" || $0 == "+" || $0 == "-"
        }) else {
            return nil
        }
        let digits = suffix[..<zoneStart]
        guard !digits.isEmpty,
              digits.allSatisfy(\.isNumber),
              let fraction = Decimal(
                string: "0.\(digits)",
                locale: posixLocale
              ) else {
            return nil
        }
        let wholeValue =
            String(value[..<decimalPoint]) + String(suffix[zoneStart...])
        guard let wholeDate = wholeFormatter.date(from: wholeValue) else {
            return nil
        }
        let wholeSeconds = wholeDate.timeIntervalSince1970
        guard wholeSeconds.isFinite,
              wholeSeconds.rounded(.towardZero) == wholeSeconds,
              wholeSeconds >= Double(Int64.min),
              wholeSeconds < Double(Int64.max) else {
            return nil
        }
        let total = Decimal(Int64(wholeSeconds)) + fraction
        let totalText = NSDecimalNumber(decimal: total)
            .description(withLocale: posixLocale)
        guard let seconds = Double(totalText), seconds.isFinite else {
            return nil
        }
        return STPDParsedTimestamp(
            seconds: seconds,
            precisionTolerance: 0.5 * pow(10, -Double(digits.count)),
            hasFractionalSeconds: true
        )
    }

    private static func canonicalDouble(_ value: Double) -> String {
        if value == 0 {
            return "0"
        }
        return String(
            format: "%.17g",
            locale: posixLocale,
            value
        )
    }
}

enum STPDCanonicalValue {
    static func double(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return ""
        }
        if value == 0 {
            return "0"
        }
        return String(format: "%.17g", locale: Locale(identifier: "en_US_POSIX"), value)
    }

    static func double(_ value: Double) -> String {
        double(Optional(value))
    }

    static func int(_ value: Int?) -> String {
        value.map(String.init) ?? ""
    }

    static func bool(_ value: Bool?) -> String {
        value.map { $0 ? "true" : "false" } ?? ""
    }

    static func bool(_ value: Bool) -> String {
        value ? "true" : "false"
    }

    static func date(_ value: Date?) -> String {
        STPDCanonicalTimestamp.string(value)
    }

    static func stringList(_ values: [String]) -> String {
        let data = try! JSONEncoder().encode(values)
        return String(decoding: data, as: UTF8.self)
    }

    static func parseStringList(_ value: String) -> [String]? {
        guard let data = value.data(using: .utf8) else {
            return nil
        }
        return try? JSONDecoder().decode([String].self, from: data)
    }
}

enum STPDRFC4180 {
    static func data(headers: [String], rows: [[String]]) -> Data {
        let lines = ([headers] + rows).map { row in
            row.map(escape).joined(separator: ",")
        }
        return Data((lines.joined(separator: "\r\n") + "\r\n").utf8)
    }

    private static func escape(_ field: String) -> String {
        // Detect the special characters by Unicode SCALAR, not by Character/grapheme: a "\r\n" in a cell
        // forms a single grapheme cluster, so `String.contains("\r")` would miss it and emit the field
        // UNQUOTED, corrupting the record structure. Scalar-level detection quotes any field containing a
        // comma, quote, CR, or LF (including an embedded CRLF).
        let needsQuoting = field.unicodeScalars.contains { scalar in
            scalar == "," || scalar == "\"" || scalar == "\r" || scalar == "\n"
        }
        guard needsQuoting else {
            return field
        }
        return "\"\(field.replacingOccurrences(of: "\"", with: "\"\""))\""
    }
}

public struct STPDResultManifestTable: Codable, Hashable, Sendable {
    public let fileName: String
    public let grain: String
    public let primaryKey: [String]
    public let columns: [STPDResultColumnDefinition]
    public let rowCount: Int
    public let sha256: String

    enum CodingKeys: String, CodingKey {
        case fileName = "file_name"
        case grain
        case primaryKey = "primary_key"
        case columns
        case rowCount = "row_count"
        case sha256
    }
}

public struct STPDResultManifest: Codable, Hashable, Sendable {
    public let schemaVersion: String
    public let detectorVersion: String
    public let runID: String
    public let datasetDigest: String
    public let settingsDigest: String
    public let buildIdentifier: String
    public let buildIdentifierKind: String
    public let buildReproducibilityAttested: Bool
    public let sourceMode: String
    public let ownerName: String
    public let ownerEmail: String
    public let stringListEncoding: String
    public let tables: [STPDResultManifestTable]

    enum CodingKeys: String, CodingKey {
        case schemaVersion = "schema_version"
        case detectorVersion = "detector_version"
        case runID = "run_id"
        case datasetDigest = "dataset_digest"
        case settingsDigest = "settings_digest"
        case buildIdentifier = "build_identifier"
        case buildIdentifierKind = "build_identifier_kind"
        case buildReproducibilityAttested = "build_reproducibility_attested"
        case sourceMode = "source_mode"
        case ownerName = "owner_name"
        case ownerEmail = "owner_email"
        case stringListEncoding = "string_list_encoding"
        case tables
    }

    func encodedData() throws -> Data {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
        var data = try encoder.encode(self)
        data.append(0x0A)
        return data
    }
}

public enum STPDResultPackageWriter {
    enum Checkpoint: Hashable, Sendable {
        case stagedContentsDurable
        case stagingEntryDurable
        case willPublish
        case published
        case publicationDurable
    }

    /// Publishes a complete result directory with a same-volume directory rename.
    ///
    /// The destination must not already exist. This prevents a partial or ambiguous overwrite from
    /// replacing a prior scientific result package. Callers should use a run-specific destination.
    public static func write(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        fileManager: FileManager = .default
    ) throws {
        try write(
            package,
            to: destinationURL,
            fileManager: fileManager,
            checkpoint: { _ in }
        )
    }

    static func writeForTesting(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        fileManager: FileManager = .default,
        checkpoint: (Checkpoint) throws -> Void
    ) throws {
        try write(
            package,
            to: destinationURL,
            fileManager: fileManager,
            checkpoint: checkpoint
        )
    }

    private static func write(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        fileManager: FileManager,
        checkpoint: (Checkpoint) throws -> Void
    ) throws {
        if fileManager.fileExists(atPath: destinationURL.path) {
            throw STPDResultPackageError.destinationAlreadyExists(destinationURL.path)
        }

        let parent = destinationURL.deletingLastPathComponent()
        try requireExistingDirectory(parent)
        let temporaryURL = parent.appendingPathComponent(
            ".\(destinationURL.lastPathComponent).tmp.\(UUID().uuidString.lowercased())",
            isDirectory: true
        )
        var published = false

        do {
            try fileManager.createDirectory(at: temporaryURL, withIntermediateDirectories: false)
            for table in STPDResultTable.allCases {
                guard let data = package.tables[table] else {
                    throw STPDResultPackageError.missingTable(table.rawValue)
                }
                let tableURL = temporaryURL.appendingPathComponent(table.rawValue)
                try data.csvData.write(
                    to: tableURL,
                    options: .atomic
                )
                try syncRegularFile(tableURL)
            }
            let manifestURL = temporaryURL.appendingPathComponent(
                STPDResultSchema.manifestFileName
            )
            try package.manifest.encodedData().write(
                to: manifestURL,
                options: .atomic
            )
            try syncRegularFile(manifestURL)
            try syncDirectory(temporaryURL)
            try checkpoint(.stagedContentsDurable)
            try syncDirectory(parent)
            try checkpoint(.stagingEntryDurable)
            try checkpoint(.willPublish)
            try renameWithoutReplacing(temporaryURL, to: destinationURL)
            published = true
            try checkpoint(.published)
            try syncDirectory(parent)
            try checkpoint(.publicationDurable)
        } catch {
            if !published, fileManager.fileExists(atPath: temporaryURL.path) {
                try? fileManager.removeItem(at: temporaryURL)
                try? syncDirectory(parent)
            }
            throw error
        }
    }

    private static func syncRegularFile(_ url: URL) throws {
        let descriptor = open(url.path, O_RDONLY | O_NOFOLLOW)
        guard descriptor >= 0 else {
            throw posixError(path: url.path)
        }
        defer { close(descriptor) }
        var metadata = stat()
        guard fstat(descriptor, &metadata) == 0 else {
            throw posixError(path: url.path)
        }
        guard metadata.st_mode & S_IFMT == S_IFREG else {
            throw STPDResultPackageError.invalidInput(
                "staged result-package entry is not a regular file: \(url.path)"
            )
        }
        guard fsync(descriptor) == 0 else {
            throw posixError(path: url.path)
        }
    }

    private static func syncDirectory(_ url: URL) throws {
        let descriptor = open(url.path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW)
        guard descriptor >= 0 else {
            throw posixError(path: url.path)
        }
        defer { close(descriptor) }
        guard fsync(descriptor) == 0 else {
            throw posixError(path: url.path)
        }
    }

    private static func requireExistingDirectory(_ url: URL) throws {
        let descriptor = open(url.path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW)
        guard descriptor >= 0 else {
            let code = errno
            throw STPDResultPackageError.invalidInput(
                "result-package parent must already exist as a real directory: "
                    + "\(url.path) (\(String(cString: strerror(code))))"
            )
        }
        close(descriptor)
    }

    private static func renameWithoutReplacing(_ source: URL, to destination: URL) throws {
        let result = source.path.withCString { sourcePath in
            destination.path.withCString { destinationPath in
                renameatx_np(
                    AT_FDCWD,
                    sourcePath,
                    AT_FDCWD,
                    destinationPath,
                    UInt32(RENAME_EXCL)
                )
            }
        }
        guard result == 0 else {
            if errno == EEXIST {
                throw STPDResultPackageError.destinationAlreadyExists(destination.path)
            }
            throw posixError(path: "\(source.path) -> \(destination.path)")
        }
    }

    private static func posixError(path: String) -> NSError {
        let code = errno
        return NSError(
            domain: NSPOSIXErrorDomain,
            code: Int(code),
            userInfo: [
                NSFilePathErrorKey: path,
                NSLocalizedDescriptionKey: String(cString: strerror(code)),
            ]
        )
    }
}
