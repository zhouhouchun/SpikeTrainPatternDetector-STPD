import Foundation
import STPDCore

/// Dataset-scoped, detector-independent persistence for locally authored manual annotations.
///
/// This store is deliberately separate from candidate-review persistence and from the canonical
/// confirmed-import receipt. It restores an editable local draft only; it never grants detector or
/// result-package authority. Geometry is revalidated against the active dataset on every load.
struct ManualAnnotationPersistence {
    static let schemaVersion = 1

    private struct StoreFile: Codable {
        var version = ManualAnnotationPersistence.schemaVersion
        var entries: [String: Entry] = [:]
    }

    private struct Entry: Codable {
        let updatedAt: Date
        let datasetName: String
        let sourceDescription: String
        let annotationsByTrain: [String: [ManualAnnotation]]
    }

    private let fileManager: FileManager
    private let storeURL: URL

    init(fileManager: FileManager = .default) {
        self.fileManager = fileManager
        let applicationSupport = (try? fileManager.url(
            for: .applicationSupportDirectory,
            in: .userDomainMask,
            appropriateFor: nil,
            create: true
        )) ?? fileManager.homeDirectoryForCurrentUser
            .appendingPathComponent("Library/Application Support", isDirectory: true)
        storeURL = applicationSupport
            .appendingPathComponent("SpikeTrainPatternDetectorMac", isDirectory: true)
            .appendingPathComponent("manual-annotations.json", isDirectory: false)
    }

    static func datasetKey(for dataset: SpikeDataset) -> String {
        ManualAnnotationDatasetKey.make(for: dataset)
    }

    func load(datasetKey: String) throws -> [String: [ManualAnnotation]] {
        try loadStore().entries[datasetKey]?.annotationsByTrain ?? [:]
    }

    func save(
        _ annotationsByTrain: [String: [ManualAnnotation]],
        datasetKey: String,
        dataset: SpikeDataset
    ) throws {
        var store = try loadStore()
        let nonempty = annotationsByTrain.filter { !$0.value.isEmpty }
        if nonempty.isEmpty {
            store.entries.removeValue(forKey: datasetKey)
        } else {
            store.entries[datasetKey] = Entry(
                updatedAt: Date(),
                datasetName: dataset.name,
                sourceDescription: dataset.sourceDescription,
                annotationsByTrain: nonempty
            )
        }
        try saveStore(store)
    }

    private func loadStore() throws -> StoreFile {
        guard fileManager.fileExists(atPath: storeURL.path) else {
            return StoreFile()
        }
        let attributes = try fileManager.attributesOfItem(atPath: storeURL.path)
        let byteCount = (attributes[.size] as? NSNumber)?.intValue ?? 0
        guard byteCount <= 16 * 1_024 * 1_024 else {
            throw CocoaError(.fileReadTooLarge)
        }
        let data = try Data(contentsOf: storeURL, options: [.mappedIfSafe])
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        let decoded = try decoder.decode(StoreFile.self, from: data)
        guard decoded.version == Self.schemaVersion else {
            throw CocoaError(.fileReadCorruptFile)
        }
        return decoded
    }

    private func saveStore(_ store: StoreFile) throws {
        let directory = storeURL.deletingLastPathComponent()
        try fileManager.createDirectory(
            at: directory,
            withIntermediateDirectories: true
        )
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        try encoder.encode(store).write(to: storeURL, options: [.atomic])
    }
}
