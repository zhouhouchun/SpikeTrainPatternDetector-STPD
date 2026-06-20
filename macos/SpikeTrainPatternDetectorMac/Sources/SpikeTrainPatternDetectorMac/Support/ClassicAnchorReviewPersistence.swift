import Foundation
import STPDCore

struct ClassicAnchorReviewPersistence {
    private struct StoreFile: Codable {
        var entries: [String: Entry] = [:]
    }

    private struct Entry: Codable {
        var updatedAt: Date
        var datasetName: String
        var sourceDescription: String
        var candidateCount: Int
        var statuses: [String: String]
    }

    private let fileManager: FileManager
    private let storeURL: URL

    init(fileManager: FileManager = .default) {
        self.fileManager = fileManager
        let directory = (try? fileManager.url(
            for: .applicationSupportDirectory,
            in: .userDomainMask,
            appropriateFor: nil,
            create: true
        )) ?? fileManager.homeDirectoryForCurrentUser.appendingPathComponent("Library/Application Support")

        self.storeURL = directory
            .appendingPathComponent("SpikeTrainPatternDetectorMac", isDirectory: true)
            .appendingPathComponent("classic-anchor-review-statuses.json")
    }

    func loadStatuses(runKey: String) throws -> [String: String] {
        let store = try loadStore()
        return store.entries[runKey]?.statuses ?? [:]
    }

    func saveStatuses(
        _ statuses: [String: String],
        runKey: String,
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun
    ) throws {
        var store = try loadStore()
        if statuses.isEmpty {
            store.entries.removeValue(forKey: runKey)
        } else {
            store.entries[runKey] = Entry(
                updatedAt: Date(),
                datasetName: dataset.name,
                sourceDescription: dataset.sourceDescription,
                candidateCount: run.candidateCount,
                statuses: statuses
            )
        }
        try saveStore(store)
    }

    private func loadStore() throws -> StoreFile {
        guard fileManager.fileExists(atPath: storeURL.path) else {
            return StoreFile()
        }

        let data = try Data(contentsOf: storeURL)
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        return try decoder.decode(StoreFile.self, from: data)
    }

    private func saveStore(_ store: StoreFile) throws {
        let directory = storeURL.deletingLastPathComponent()
        try fileManager.createDirectory(at: directory, withIntermediateDirectories: true)

        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        let data = try encoder.encode(store)
        try data.write(to: storeURL, options: .atomic)
    }
}
