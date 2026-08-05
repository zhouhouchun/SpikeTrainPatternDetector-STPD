import CryptoKit
import Darwin
import Foundation

enum ScientificSourceFormat: String, Equatable, Sendable {
    case csv
    case xlsx
}

struct BoundedScientificSource: Equatable, Sendable {
    let format: ScientificSourceFormat
    let snapshot: Data
    let displayName: String
    let byteCount: Int
    let sourceSHA256: String

    fileprivate init(format: ScientificSourceFormat, snapshot: Data, displayName: String) {
        let ownedSnapshot = Data(snapshot)
        self.format = format
        self.snapshot = ownedSnapshot
        self.displayName = displayName
        self.byteCount = ownedSnapshot.count
        self.sourceSHA256 = SHA256.hash(data: ownedSnapshot)
            .map { String(format: "%02x", $0) }
            .joined()
    }
}

enum BoundedScientificSourceReaderError: Error, Equatable, Sendable, LocalizedError {
    case notFileURL
    case unsupportedFileExtension
    case cannotInspectFile
    case notRegularFile
    case cannotOpenFile
    case cannotReadFile
    case sourceTooLarge(maximumByteCount: Int)

    var errorDescription: String? {
        switch self {
        case .notFileURL:
            "The selected source must be a local file."
        case .unsupportedFileExtension:
            "Only CSV and XLSX source files are supported."
        case .cannotInspectFile:
            "The selected source file could not be inspected."
        case .notRegularFile:
            "The selected source must be a regular file."
        case .cannotOpenFile:
            "The selected source file could not be opened for reading."
        case .cannotReadFile:
            "The selected source file could not be read."
        case let .sourceTooLarge(maximumByteCount):
            "The selected source exceeds the \(maximumByteCount)-byte size limit."
        }
    }
}

enum BoundedScientificSourceReader {
    static let maximumByteCount = 5 * 1_024 * 1_024

    private static let readChunkByteCount = 64 * 1_024

    static func read(from url: URL) throws -> BoundedScientificSource {
        guard url.isFileURL else {
            throw BoundedScientificSourceReaderError.notFileURL
        }

        let format = try format(for: url)
        try validateRegularFile(at: url)

        let handle: FileHandle
        do {
            handle = try FileHandle(forReadingFrom: url)
        } catch {
            throw BoundedScientificSourceReaderError.cannotOpenFile
        }
        defer { try? handle.close() }

        // Validate the object that was actually opened, not only the path inspected
        // above. This closes the path-replacement race before any bytes are accepted.
        var fileStatus = stat()
        guard Darwin.fstat(handle.fileDescriptor, &fileStatus) == 0 else {
            throw BoundedScientificSourceReaderError.cannotInspectFile
        }
        guard (fileStatus.st_mode & S_IFMT) == S_IFREG else {
            throw BoundedScientificSourceReaderError.notRegularFile
        }

        let snapshot = try readBoundedSnapshot(from: handle)
        return BoundedScientificSource(
            format: format,
            snapshot: snapshot,
            displayName: url.lastPathComponent
        )
    }

    private static func format(for url: URL) throws -> ScientificSourceFormat {
        switch url.pathExtension.lowercased() {
        case "csv":
            return .csv
        case "xlsx":
            return .xlsx
        default:
            throw BoundedScientificSourceReaderError.unsupportedFileExtension
        }
    }

    private static func validateRegularFile(at url: URL) throws {
        let resourceValues: URLResourceValues
        do {
            resourceValues = try url.resourceValues(forKeys: [.isRegularFileKey])
        } catch {
            throw BoundedScientificSourceReaderError.cannotInspectFile
        }

        guard resourceValues.isRegularFile == true else {
            throw BoundedScientificSourceReaderError.notRegularFile
        }
    }

    private static func readBoundedSnapshot(from handle: FileHandle) throws -> Data {
        var snapshot = Data()
        snapshot.reserveCapacity(maximumByteCount)

        do {
            while snapshot.count <= maximumByteCount {
                let remainingProbeByteCount = maximumByteCount + 1 - snapshot.count
                let requestedByteCount = min(readChunkByteCount, remainingProbeByteCount)
                guard requestedByteCount > 0 else {
                    break
                }

                guard let chunk = try handle.read(upToCount: requestedByteCount), !chunk.isEmpty else {
                    break
                }
                snapshot.append(chunk)

                if snapshot.count > maximumByteCount {
                    throw BoundedScientificSourceReaderError.sourceTooLarge(
                        maximumByteCount: maximumByteCount
                    )
                }
            }
        } catch let error as BoundedScientificSourceReaderError {
            throw error
        } catch {
            throw BoundedScientificSourceReaderError.cannotReadFile
        }

        return snapshot
    }
}
