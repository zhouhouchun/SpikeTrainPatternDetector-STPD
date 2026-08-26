import Foundation
import STPDCore
import Testing
import ZIPFoundation
@testable import STPDTabularIO

@Test
func manualISIWorkbookCreatesOneWorksheetPerSpikeTrain() throws {
    let table = multiTrainTable()
    let data = try ManualISIResultXLSXExporter.data(table: table)
    let inspection = try CanonicalXLSXWorkbookInspector.inspect(
        data: data,
        limits: .supportedDatasetEnvelope
    )

    #expect(inspection.worksheets.map(\.name) == ["Unit_A", "Unit_B"])

    let first = try stagedWorksheet(inspection, at: 0)
    #expect(first.columns.map(\.header) == table.headers.map(Optional.some))
    #expect(first.columns[0].cells == [
        .text(rawText: "train-a"),
        .text(rawText: "train-a"),
    ])
    #expect(first.columns[2].cells == [
        .text(rawText: "9223372036854775807"),
        .text(rawText: "1000001"),
    ])
    #expect(first.columns[3].cells == [
        .text(rawText: "quoted \"value\" & text"),
        .text(rawText: "A second row"),
    ])

    let second = try stagedWorksheet(inspection, at: 1)
    #expect(second.columns[0].cells == [.text(rawText: "train-b")])
    #expect(second.columns[1].cells == [.text(rawText: "Unit_B")])
    #expect(second.columns[2].cells == [.text(rawText: "2000000")])
}

@Test
func manualISIWorkbookSanitizesAndUniquifiesWorksheetNames() throws {
    let repeatedLongName = "A/very:long*spike?train[name] exceeding thirty one chars"
    let table = ManualISIExportTable(
        headers: ["train_id", "train_name", "isi_index"],
        rows: [
            ["a", repeatedLongName, "1"],
            ["b", repeatedLongName, "1"],
        ]
    )
    let data = try ManualISIResultXLSXExporter.data(table: table)
    let inspection = try CanonicalXLSXWorkbookInspector.inspect(
        data: data,
        limits: .supportedDatasetEnvelope
    )
    let names = inspection.worksheets.map(\.name)

    #expect(names.count == 2)
    #expect(Set(names.map { $0.lowercased() }).count == 2)
    #expect(names.allSatisfy { $0.utf16.count <= 31 })
    #expect(names.allSatisfy {
        $0.rangeOfCharacter(from: CharacterSet(charactersIn: "[]:*?/\\")) == nil
    })
    #expect(names[1].hasSuffix(" (2)"))
}

@Test
func manualISICSVBundleCreatesOneCSVPerSpikeTrain() throws {
    let table = multiTrainTable()
    let data = try ManualISIResultCSVBundleExporter.data(table: table)
    let archive = try Archive(data: data, accessMode: .read)
    let entries = Array(archive).sorted { $0.path < $1.path }

    #expect(entries.map(\.path) == ["001_Unit_A.csv", "002_Unit_B.csv"])
    let first = try extract(entries[0], from: archive)
    let second = try extract(entries[1], from: archive)
    let firstText = try #require(String(data: first, encoding: .utf8))
    let secondText = try #require(String(data: second, encoding: .utf8))

    #expect(firstText.hasPrefix("train_id,train_name,right_us,note\r\n"))
    #expect(firstText.contains("train-a"))
    #expect(!firstText.contains("train-b"))
    #expect(secondText.contains("train-b"))
    #expect(!secondText.contains("train-a"))
}

@Test
func manualISIResultPartitioningRequiresTrainID() {
    let table = ManualISIExportTable(headers: ["isi_index"], rows: [["1"]])
    #expect(throws: ManualISIResultXLSXExporterError.self) {
        try ManualISIResultXLSXExporter.data(table: table)
    }
    #expect(throws: ManualISIResultXLSXExporterError.self) {
        try ManualISIResultCSVBundleExporter.data(table: table)
    }
}

@Test
func manualISIDraftWorkbookRoundTripsAcrossTrainSheets() throws {
    let headers = CanonicalManualISIDraftCSVExporter.headers
    let draftSchemaDigest = CanonicalManualISIDraftCSVExporter.schemaContractDigest
    let identity = [
        CanonicalManualISIDraftCSVExporter.schemaContractID,
        draftSchemaDigest,
        "canonical_schema",
        "canonical_schema_digest",
        "canonical_dataset_digest",
        "confirmed_import_record_digest",
    ]
    let table = ManualISIExportTable(
        headers: headers,
        rows: [
            identity + [
                "unit_a", "1", "0", "10000", "10000",
                "tonic", "high_frequency_burst", "", "manually_labeled",
                CanonicalManualISIDraftCSVExporter.authorityStatus,
            ],
            identity + [
                "unit_b", "1", "100", "300", "200",
                "", "", "other", "manually_labeled",
                CanonicalManualISIDraftCSVExporter.authorityStatus,
            ],
            identity + [
                "unit_a", "2", "10000", "25000", "15000",
                "", "", "", "unreviewed",
                CanonicalManualISIDraftCSVExporter.authorityStatus,
            ],
        ]
    )

    let workbook = try ManualISIResultXLSXExporter.data(table: table)
    let decoded = try ManualISIResultXLSXImporter.import(data: workbook)

    #expect(decoded.rowCount == 3)
    #expect(decoded.labeledISIIndexCount == 2)
    #expect(decoded.decisions.count == 3)
    #expect(decoded.decisions.contains {
        $0.trainID.semanticID.canonicalText == "unit_a"
            && $0.isiIndex == 1
            && $0.track == .state
            && $0.label == .tonic
    })
    #expect(decoded.decisions.contains {
        $0.trainID.semanticID.canonicalText == "unit_a"
            && $0.isiIndex == 1
            && $0.track == .event
            && $0.label == .highFrequencyBurst
    })
    #expect(decoded.decisions.contains {
        $0.trainID.semanticID.canonicalText == "unit_b"
            && $0.isiIndex == 1
            && $0.track == .other
            && $0.label == .other
    })
}


private func multiTrainTable() -> ManualISIExportTable {
    ManualISIExportTable(
        headers: ["train_id", "train_name", "right_us", "note"],
        rows: [
            ["train-a", "Unit_A", "9223372036854775807", "quoted \"value\" & text"],
            ["train-b", "Unit_B", "2000000", "B only"],
            ["train-a", "Unit_A", "1000001", "A second row"],
        ]
    )
}

private func stagedWorksheet(
    _ inspection: CanonicalXLSXWorkbookInspection,
    at index: Int
) throws -> StagedScientificImport {
    let worksheet = try #require(inspection.worksheets[safe: index])
    return try CanonicalXLSXWorksheetStagingReader.read(
        inspection: inspection,
        worksheet: worksheet,
        headerDecision: .firstRecordIsHeader,
        limits: .supportedDatasetEnvelope
    )
}

private func extract(_ entry: Entry, from archive: Archive) throws -> Data {
    var result = Data()
    _ = try archive.extract(entry) { result.append($0) }
    return result
}

private extension Collection {
    subscript(safe index: Index) -> Element? {
        indices.contains(index) ? self[index] : nil
    }
}
