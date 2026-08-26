import Foundation
@testable import SpikeTrainPatternDetectorMac
import Testing

@Suite("Bounded scientific source reader")
struct BoundedScientificSourceReaderTests {
    @Test("A source exactly at the five MiB boundary is accepted")
    func exactBoundaryIsAccepted() throws {
        try withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("boundary.csv")
            let original = Data(
                repeating: 0x61,
                count: BoundedScientificSourceReader.maximumByteCount
            )
            try original.write(to: url)

            let source = try BoundedScientificSourceReader.read(from: url)

            #expect(source.format == .csv)
            #expect(source.displayName == "boundary.csv")
            #expect(source.byteCount == BoundedScientificSourceReader.maximumByteCount)
            #expect(source.snapshot == original)
        }
    }

    @Test("A source one byte over the five MiB boundary is rejected")
    func oneByteOverBoundaryIsRejected() throws {
        try withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("over-limit.xlsx")
            let original = Data(
                repeating: 0x62,
                count: BoundedScientificSourceReader.maximumByteCount + 1
            )
            try original.write(to: url)

            #expect(throws: BoundedScientificSourceReaderError.sourceTooLarge(
                maximumByteCount: BoundedScientificSourceReader.maximumByteCount
            )) {
                try BoundedScientificSourceReader.read(from: url)
            }
        }
    }

    @Test("Unsupported extensions are rejected without format fallback")
    func unsupportedExtensionIsRejected() throws {
        try withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.txt")
            try Data("timestamp\n1.0\n".utf8).write(to: url)

            #expect(throws: BoundedScientificSourceReaderError.unsupportedFileExtension) {
                try BoundedScientificSourceReader.read(from: url)
            }
        }
    }

    @Test("A directory with a supported extension is not accepted as a source")
    func directoryIsRejected() throws {
        try withTemporaryDirectory { directory in
            let sourceDirectory = directory.appendingPathComponent("not-a-file.csv")
            try FileManager.default.createDirectory(
                at: sourceDirectory,
                withIntermediateDirectories: false
            )

            #expect(throws: BoundedScientificSourceReaderError.notRegularFile) {
                try BoundedScientificSourceReader.read(from: sourceDirectory)
            }
        }
    }

    @Test("Supported extensions are matched case-insensitively")
    func uppercaseExtensionIsAccepted() throws {
        try withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("WORKBOOK.XLSX")
            let original = Data("abc".utf8)
            try original.write(to: url)

            let source = try BoundedScientificSourceReader.read(from: url)

            #expect(source.format == .xlsx)
            #expect(source.displayName == "WORKBOOK.XLSX")
            #expect(source.byteCount == original.count)
            #expect(source.snapshot == original)
            #expect(source.sourceSHA256 == "ba7816bf8f01cfea414140de5dae2223"
                + "b00361a396177a9cb410ff61f20015ad")
        }
    }

    @Test("The returned snapshot is unaffected by later source-file changes")
    func snapshotSurvivesSourceMutation() throws {
        try withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("mutable.csv")
            let original = Data("unit_a\n1.000000\n".utf8)
            try original.write(to: url)

            let source = try BoundedScientificSourceReader.read(from: url)
            try Data("unit_a\n9.999999\n10.000000\n".utf8).write(to: url)

            #expect(source.snapshot == original)
            #expect(source.byteCount == original.count)
            #expect(source.sourceSHA256 == "97d9d1f61e0ba23f8b7f88ce9cac6355"
                + "cef3537b08cd0e89435f5c6887206413")
        }
    }

    @Test("NEX has an explicit bounded snapshot path without entering CSV or XLSX staging")
    func nexSnapshotIsAcceptedByItsDedicatedReader() throws {
        try withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("recording.NEX")
            let original = Data(repeating: 0x2A, count: 544)
            try original.write(to: url)

            let source = try BoundedScientificSourceReader.readNEX(from: url)

            #expect(source.displayName == "recording.NEX")
            #expect(source.snapshot == original)
            #expect(source.byteCount == 544)
            #expect(throws: BoundedScientificSourceReaderError.unsupportedFileExtension) {
                try BoundedScientificSourceReader.read(from: url)
            }
        }
    }

    private func withTemporaryDirectory<T>(_ body: (URL) throws -> T) throws -> T {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("BoundedScientificSourceReaderTests")
            .appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(
            at: directory,
            withIntermediateDirectories: true
        )
        defer { try? FileManager.default.removeItem(at: directory) }
        return try body(directory)
    }
}
