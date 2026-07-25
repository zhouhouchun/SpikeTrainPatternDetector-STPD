import CryptoKit
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
        guard let value else {
            return ""
        }
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter.string(from: value)
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
        guard field.contains(",") ||
                field.contains("\"") ||
                field.contains("\r") ||
                field.contains("\n") else {
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
    /// Publishes a complete result directory with a same-volume directory rename.
    ///
    /// The destination must not already exist. This prevents a partial or ambiguous overwrite from
    /// replacing a prior scientific result package. Callers should use a run-specific destination.
    public static func write(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        fileManager: FileManager = .default
    ) throws {
        if fileManager.fileExists(atPath: destinationURL.path) {
            throw STPDResultPackageError.destinationAlreadyExists(destinationURL.path)
        }

        let parent = destinationURL.deletingLastPathComponent()
        try fileManager.createDirectory(
            at: parent,
            withIntermediateDirectories: true
        )
        let temporaryURL = parent.appendingPathComponent(
            ".\(destinationURL.lastPathComponent).tmp.\(UUID().uuidString.lowercased())",
            isDirectory: true
        )

        do {
            try fileManager.createDirectory(at: temporaryURL, withIntermediateDirectories: false)
            for table in STPDResultTable.allCases {
                guard let data = package.tables[table] else {
                    throw STPDResultPackageError.missingTable(table.rawValue)
                }
                try data.csvData.write(
                    to: temporaryURL.appendingPathComponent(table.rawValue),
                    options: .atomic
                )
            }
            try package.manifest.encodedData().write(
                to: temporaryURL.appendingPathComponent(STPDResultSchema.manifestFileName),
                options: .atomic
            )
            try fileManager.moveItem(at: temporaryURL, to: destinationURL)
        } catch {
            try? fileManager.removeItem(at: temporaryURL)
            throw error
        }
    }
}
