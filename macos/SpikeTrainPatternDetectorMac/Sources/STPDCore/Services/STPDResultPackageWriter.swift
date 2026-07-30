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

/// Controls which existing parent directories may receive a result package.
public enum STPDResultPackageParentDirectoryPolicy: Sendable, Hashable {
    /// Require the parent to be owned by the current user, not group/world writable, and free of
    /// extended ACL entries that grant another principal authority over published packages. Deny-only
    /// ACLs commonly installed by macOS on user folders are permitted.
    /// This is the default for fail-closed local scientific-result publication.
    case privateCurrentUserOnly

    /// Permit an existing group/world-writable or differently-owned parent directory.
    ///
    /// Descriptor-relative staging, private package permissions, removal of inherited ACLs from newly
    /// created package entries, and no-replace publication still protect the write transaction itself.
    /// A collaborator or administrator with write authority
    /// over the parent may move, replace, or delete the completed directory after return, so this
    /// policy does not claim persistent authenticity in a shared directory. Use only when that
    /// collaboration model is intentional and independently governed.
    case allowShared
}

public enum STPDResultPackageWriter {
    /// These checkpoints mean the writer completed `fsync` (with EINTR retry) for the relevant file or
    /// directory descriptors. They describe kernel/filesystem synchronization order, not an absolute
    /// physical-media durability guarantee against storage hardware that falsely acknowledges flushes.
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
    /// The default parent policy rejects shared directories; callers must opt in explicitly when a
    /// laboratory-managed shared parent is required.
    @_disfavoredOverload
    public static func write(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        parentDirectoryPolicy: STPDResultPackageParentDirectoryPolicy =
            .privateCurrentUserOnly
    ) throws {
        try write(
            package,
            to: destinationURL,
            parentDirectoryPolicy: parentDirectoryPolicy,
            checkpoint: { _ in }
        )
    }

    /// Source-compatible entry point retained for callers compiled against the B2 writer API.
    ///
    /// Descriptor-relative I/O is intentionally authoritative; the supplied `FileManager` is not used
    /// to create, enumerate, replace, or publish package entries.
    @available(*, deprecated, message: "Use write(_:to:parentDirectoryPolicy:) instead.")
    public static func write(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        fileManager: FileManager
    ) throws {
        _ = fileManager
        try write(
            package,
            to: destinationURL,
            parentDirectoryPolicy: .privateCurrentUserOnly
        )
    }

    static func writeForTesting(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        parentDirectoryPolicy: STPDResultPackageParentDirectoryPolicy =
            .privateCurrentUserOnly,
        checkpoint: (Checkpoint) throws -> Void
    ) throws {
        try write(
            package,
            to: destinationURL,
            parentDirectoryPolicy: parentDirectoryPolicy,
            checkpoint: checkpoint
        )
    }

    private static func write(
        _ package: STPDResultPackage,
        to destinationURL: URL,
        parentDirectoryPolicy: STPDResultPackageParentDirectoryPolicy,
        checkpoint: (Checkpoint) throws -> Void
    ) throws {
        let parent = destinationURL.deletingLastPathComponent()
        let destinationName = destinationURL.lastPathComponent
        try requireBareEntryName(destinationName, path: destinationURL.path)

        let parentFD = open(
            parent.path,
            O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC
        )
        guard parentFD >= 0 else {
            let code = errno
            throw STPDResultPackageError.invalidInput(
                "result-package parent must already exist as a real directory: "
                    + "\(parent.path) (\(String(cString: strerror(code))))"
            )
        }
        defer { close(parentFD) }
        try requireTrustedParent(
            parentFD: parentFD,
            path: parent.path,
            policy: parentDirectoryPolicy
        )
        try requireDestinationAbsent(
            parentFD: parentFD,
            destinationName: destinationName,
            destinationPath: destinationURL.path
        )

        let workspaceName =
            ".\(destinationName).tmp.\(UUID().uuidString.lowercased())"
        guard mkdirat(parentFD, workspaceName, mode_t(S_IRWXU)) == 0 else {
            throw posixError(path: parent.appendingPathComponent(workspaceName).path)
        }
        let workspaceFD = openat(
            parentFD,
            workspaceName,
            O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC
        )
        guard workspaceFD >= 0 else {
            let openingError = posixError(
                path: parent.appendingPathComponent(workspaceName).path
            )
            _ = unlinkat(parentFD, workspaceName, AT_REMOVEDIR)
            throw openingError
        }
        defer { close(workspaceFD) }
        do {
            try removeExtendedACL(
                descriptor: workspaceFD,
                path: parent.appendingPathComponent(workspaceName).path
            )
        } catch {
            _ = unlinkat(parentFD, workspaceName, AT_REMOVEDIR)
            throw error
        }
        let workspaceIdentity = try descriptorIdentity(
            workspaceFD,
            path: parent.appendingPathComponent(workspaceName).path
        )

        let payloadName = "payload"
        guard mkdirat(workspaceFD, payloadName, mode_t(S_IRWXU)) == 0 else {
            let error = posixError(path: payloadName)
            _ = unlinkat(parentFD, workspaceName, AT_REMOVEDIR)
            throw error
        }
        let payloadFD = openat(
            workspaceFD,
            payloadName,
            O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC
        )
        guard payloadFD >= 0 else {
            let error = posixError(path: payloadName)
            _ = unlinkat(workspaceFD, payloadName, AT_REMOVEDIR)
            _ = unlinkat(parentFD, workspaceName, AT_REMOVEDIR)
            throw error
        }
        defer { close(payloadFD) }
        do {
            try removeExtendedACL(
                descriptor: payloadFD,
                path: parent.appendingPathComponent(workspaceName)
                    .appendingPathComponent(payloadName).path
            )
        } catch {
            _ = unlinkat(workspaceFD, payloadName, AT_REMOVEDIR)
            _ = unlinkat(parentFD, workspaceName, AT_REMOVEDIR)
            throw error
        }
        let payloadIdentity = try descriptorIdentity(
            payloadFD,
            path: payloadName
        )

        var published = false
        var stagedNames: [String] = []

        do {
            for table in STPDResultTable.allCases {
                guard let data = package.tables[table] else {
                    throw STPDResultPackageError.missingTable(table.rawValue)
                }
                try writeNewRegularFile(
                    data.csvData,
                    name: table.rawValue,
                    directoryFD: payloadFD,
                    stagedNames: &stagedNames
                )
            }
            try writeNewRegularFile(
                package.manifest.encodedData(),
                name: STPDResultSchema.manifestFileName,
                directoryFD: payloadFD,
                stagedNames: &stagedNames
            )
            try syncDescriptor(
                payloadFD,
                path: parent.appendingPathComponent(workspaceName)
                    .appendingPathComponent(payloadName).path
            )
            try checkpoint(.stagedContentsDurable)
            try syncDescriptor(
                workspaceFD,
                path: parent.appendingPathComponent(workspaceName).path
            )
            try syncDescriptor(parentFD, path: parent.path)
            try checkpoint(.stagingEntryDurable)
            try checkpoint(.willPublish)
            try requireOpenedEntryUnchanged(
                directoryFD: workspaceFD,
                name: payloadName,
                expected: payloadIdentity,
                path: parent.appendingPathComponent(workspaceName)
                    .appendingPathComponent(payloadName).path
            )
            try renameWithoutReplacing(
                sourceDirectoryFD: workspaceFD,
                sourceName: payloadName,
                destinationDirectoryFD: parentFD,
                destinationName: destinationName,
                destinationPath: destinationURL.path
            )
            published = true
            removeOpenedDirectoryEntryIfUnchanged(
                directoryFD: parentFD,
                name: workspaceName,
                expected: workspaceIdentity
            )
            try checkpoint(.published)
            try syncDescriptor(parentFD, path: parent.path)
            try checkpoint(.publicationDurable)
        } catch {
            if !published {
                for name in stagedNames.reversed() {
                    _ = unlinkat(payloadFD, name, 0)
                }
                removeOpenedDirectoryEntryIfUnchanged(
                    directoryFD: workspaceFD,
                    name: payloadName,
                    expected: payloadIdentity
                )
                removeOpenedDirectoryEntryIfUnchanged(
                    directoryFD: parentFD,
                    name: workspaceName,
                    expected: workspaceIdentity
                )
                try? syncDescriptor(parentFD, path: parent.path)
            }
            throw error
        }
    }

    private static func requireBareEntryName(_ name: String, path: String) throws {
        guard !name.isEmpty,
              name != ".",
              name != "..",
              !name.contains("/"),
              !name.contains("\0") else {
            throw STPDResultPackageError.invalidInput(
                "result-package destination must have a legal bare name: \(path)"
            )
        }
    }

    private static func requireDestinationAbsent(
        parentFD: Int32,
        destinationName: String,
        destinationPath: String
    ) throws {
        var metadata = stat()
        if fstatat(parentFD, destinationName, &metadata, AT_SYMLINK_NOFOLLOW) == 0 {
            throw STPDResultPackageError.destinationAlreadyExists(destinationPath)
        }
        guard errno == ENOENT else {
            throw posixError(path: destinationPath)
        }
    }

    private struct FileIdentity {
        let device: dev_t
        let inode: ino_t
    }

    private static func descriptorIdentity(
        _ descriptor: Int32,
        path: String
    ) throws -> FileIdentity {
        var metadata = stat()
        guard fstat(descriptor, &metadata) == 0 else {
            throw posixError(path: path)
        }
        return FileIdentity(device: metadata.st_dev, inode: metadata.st_ino)
    }

    private static func requireOpenedEntryUnchanged(
        directoryFD: Int32,
        name: String,
        expected: FileIdentity,
        path: String
    ) throws {
        var metadata = stat()
        guard fstatat(directoryFD, name, &metadata, AT_SYMLINK_NOFOLLOW) == 0,
              (metadata.st_mode & S_IFMT) == S_IFDIR,
              metadata.st_dev == expected.device,
              metadata.st_ino == expected.inode else {
            throw STPDResultPackageError.invalidInput(
                "result-package staging entry changed before publication: \(path)"
            )
        }
    }

    private static func removeOpenedDirectoryEntryIfUnchanged(
        directoryFD: Int32,
        name: String,
        expected: FileIdentity
    ) {
        var metadata = stat()
        guard fstatat(directoryFD, name, &metadata, AT_SYMLINK_NOFOLLOW) == 0,
              (metadata.st_mode & S_IFMT) == S_IFDIR,
              metadata.st_dev == expected.device,
              metadata.st_ino == expected.inode else {
            return
        }
        _ = unlinkat(directoryFD, name, AT_REMOVEDIR)
    }

    private static func requireTrustedParent(
        parentFD: Int32,
        path: String,
        policy: STPDResultPackageParentDirectoryPolicy
    ) throws {
        var metadata = stat()
        guard fstat(parentFD, &metadata) == 0 else {
            throw posixError(path: path)
        }
        guard policy == .privateCurrentUserOnly else {
            return
        }
        let unsafeWriteBits = mode_t(S_IWGRP | S_IWOTH)
        guard metadata.st_uid == geteuid(),
              metadata.st_mode & unsafeWriteBits == 0 else {
            throw STPDResultPackageError.invalidInput(
                "result-package parent must be owned by the current user and not group/world writable: "
                    + path
            )
        }
        try requireNoGrantingExtendedACL(parentFD: parentFD, path: path)
    }

    private static func requireNoGrantingExtendedACL(
        parentFD: Int32,
        path: String
    ) throws {
        errno = 0
        guard let acl = acl_get_fd_np(parentFD, ACL_TYPE_EXTENDED) else {
            if errno == ENOENT {
                return
            }
            throw STPDResultPackageError.invalidInput(
                "unable to inspect result-package parent extended ACL: \(path)"
            )
        }
        defer {
            _ = acl_free(UnsafeMutableRawPointer(acl))
        }

        var entryID = ACL_FIRST_ENTRY
        while true {
            var entry: acl_entry_t?
            errno = 0
            let result = acl_get_entry(
                acl,
                Int32(entryID.rawValue),
                &entry
            )
            if result == -1,
               errno == EINVAL,
               entryID == ACL_NEXT_ENTRY {
                break
            }
            guard result == 0, let entry else {
                throw STPDResultPackageError.invalidInput(
                    "unable to inspect result-package parent extended ACL entries: \(path)"
                )
            }
            var tag = ACL_UNDEFINED_TAG
            guard acl_get_tag_type(entry, &tag) == 0 else {
                throw STPDResultPackageError.invalidInput(
                    "unable to inspect result-package parent extended ACL tag: \(path)"
                )
            }
            guard tag != ACL_EXTENDED_ALLOW else {
                throw STPDResultPackageError.invalidInput(
                    "result-package parent extended ACL grants authority under privateCurrentUserOnly: "
                        + path
                )
            }
            guard tag == ACL_EXTENDED_DENY else {
                throw STPDResultPackageError.invalidInput(
                    "result-package parent carries an unsupported ACL entry under "
                        + "privateCurrentUserOnly: \(path)"
                )
            }
            entryID = ACL_NEXT_ENTRY
        }
    }

    /// Removes any ACL inherited by a newly-created staging entry. Mode bits remain the authoritative
    /// package permission surface (0700 directories, 0600 files), including under `allowShared`.
    private static func removeExtendedACL(
        descriptor: Int32,
        path: String
    ) throws {
        guard let emptyACL = acl_init(0) else {
            throw STPDResultPackageError.invalidInput(
                "unable to allocate empty ACL for result-package entry: \(path)"
            )
        }
        defer {
            _ = acl_free(UnsafeMutableRawPointer(emptyACL))
        }
        guard acl_set_fd_np(descriptor, emptyACL, ACL_TYPE_EXTENDED) == 0 else {
            throw STPDResultPackageError.invalidInput(
                "unable to remove inherited ACL from result-package entry: \(path)"
            )
        }
    }

    private static func writeNewRegularFile(
        _ data: Data,
        name: String,
        directoryFD: Int32,
        stagedNames: inout [String]
    ) throws {
        let descriptor = openat(
            directoryFD,
            name,
            O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC,
            mode_t(S_IRUSR | S_IWUSR)
        )
        guard descriptor >= 0 else {
            throw posixError(path: name)
        }
        stagedNames.append(name)
        defer { close(descriptor) }
        try removeExtendedACL(descriptor: descriptor, path: name)

        try data.withUnsafeBytes { bytes in
            guard let base = bytes.baseAddress else { return }
            var offset = 0
            while offset < bytes.count {
                let count = Darwin.write(
                    descriptor,
                    base.advanced(by: offset),
                    bytes.count - offset
                )
                if count < 0 {
                    if errno == EINTR {
                        continue
                    }
                    throw posixError(path: name)
                }
                guard count > 0 else {
                    throw STPDResultPackageError.invalidInput(
                        "zero-byte write while staging result-package entry: \(name)"
                    )
                }
                offset += count
            }
        }
        try syncDescriptor(descriptor, path: name)
    }

    private static func syncDescriptor(_ descriptor: Int32, path: String) throws {
        while fsync(descriptor) != 0 {
            if errno == EINTR {
                continue
            }
            throw posixError(path: path)
        }
    }

    private static func renameWithoutReplacing(
        sourceDirectoryFD: Int32,
        sourceName: String,
        destinationDirectoryFD: Int32,
        destinationName: String,
        destinationPath: String
    ) throws {
        let result = renameatx_np(
            sourceDirectoryFD,
            sourceName,
            destinationDirectoryFD,
            destinationName,
            publicationRenameFlags
        )
        guard result == 0 else {
            if errno == EEXIST {
                throw STPDResultPackageError.destinationAlreadyExists(destinationPath)
            }
            throw posixError(path: "\(sourceName) -> \(destinationPath)")
        }
    }

    /// `RENAME_RESOLVE_BENEATH` was added to the Darwin SDK after this package's
    /// macOS 14 deployment baseline. Keep the ABI bit local so older SDKs compile,
    /// and request it only on systems that implement it.
    private static let renameResolveBeneathFlag: UInt32 = 0x20

    static func publicationRenameFlagsForTesting(
        resolveBeneathAvailable: Bool
    ) -> UInt32 {
        var flags = UInt32(RENAME_EXCL | RENAME_NOFOLLOW_ANY)
        if resolveBeneathAvailable {
            flags |= renameResolveBeneathFlag
        }
        return flags
    }

    private static var publicationRenameFlags: UInt32 {
        if #available(macOS 26.0, *) {
            return publicationRenameFlagsForTesting(
                resolveBeneathAvailable: true
            )
        }
        return publicationRenameFlagsForTesting(
            resolveBeneathAvailable: false
        )
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
