import CryptoKit
import Foundation
import Testing
import ZIPFoundation
@testable import STPDTabularIO

private let xlsxLimits = CanonicalXLSXInspectionLimits.supportedDatasetEnvelope

private struct TestArchiveEntry {
    let path: String
    let data: Data
    let type: Entry.EntryType
    let compression: CompressionMethod
    let permissions: UInt16?

    init(
        _ path: String,
        _ text: String,
        type: Entry.EntryType = .file,
        compression: CompressionMethod = .none,
        permissions: UInt16? = nil
    ) {
        self.path = path
        self.data = Data(text.utf8)
        self.type = type
        self.compression = compression
        self.permissions = permissions
    }

    init(
        _ path: String,
        data: Data,
        type: Entry.EntryType = .file,
        compression: CompressionMethod = .none,
        permissions: UInt16? = nil
    ) {
        self.path = path
        self.data = data
        self.type = type
        self.compression = compression
        self.permissions = permissions
    }
}

private struct FixtureSheet {
    let name: String
    let id: String?
    let relationshipID: String?
    let state: String?
    let target: String
    let relationshipType: String
    let includePart: Bool
    let contentType: String

    init(
        name: String,
        id: String? = nil,
        relationshipID: String?,
        state: String? = nil,
        target: String,
        relationshipType: String =
            "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet",
        includePart: Bool = true,
        contentType: String =
            "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"
    ) {
        self.name = name
        self.id = id
        self.relationshipID = relationshipID
        self.state = state
        self.target = target
        self.relationshipType = relationshipType
        self.includePart = includePart
        self.contentType = contentType
    }
}

private let transitionalContentTypesNamespace =
    "http://schemas.openxmlformats.org/package/2006/content-types"
private let transitionalPackageRelationshipsNamespace =
    "http://schemas.openxmlformats.org/package/2006/relationships"
private let transitionalSpreadsheetNamespace =
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
private let transitionalDocumentRelationshipsNamespace =
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
private let officeDocumentRelationship = transitionalDocumentRelationshipsNamespace + "/officeDocument"
private let workbookContentType =
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"
private let relationshipsContentType =
    "application/vnd.openxmlformats-package.relationships+xml"

private func fixtureEntries(
    sheets: [FixtureSheet] = [
        FixtureSheet(name: "Unit A", relationshipID: "sheetA", target: "../sheets/a.xml"),
        FixtureSheet(
            name: "Unit B",
            relationshipID: "sheetB",
            state: "hidden",
            target: "../sheets/b.xml"
        ),
        FixtureSheet(
            name: "Events",
            relationshipID: "sheetEvents",
            state: "veryHidden",
            target: "../sheets/events.xml"
        ),
    ],
    workbookPath: String = "pkg/books/main.xml",
    workbookNamespace: String = transitionalSpreadsheetNamespace,
    relationshipAttributeNamespace: String = transitionalDocumentRelationshipsNamespace,
    rootRelationshipsNamespace: String = transitionalPackageRelationshipsNamespace,
    rootRelationshipType: String = officeDocumentRelationship,
    rootTarget: String? = nil,
    rootTargetMode: String? = nil,
    workbookContentTypeValue: String = workbookContentType,
    contentTypesNamespace: String = transitionalContentTypesNamespace,
    extraContentTypeXML: String = "",
    extraRootRelationshipXML: String = "",
    extraWorkbookRelationshipXML: String = "",
    workbookPrefixXML: String = "",
    workbookSuffixXML: String = ""
) -> [TestArchiveEntry] {
    let workbookDirectory = workbookPath.split(separator: "/").dropLast().joined(separator: "/")
    let workbookFileName = String(workbookPath.split(separator: "/").last!)
    let workbookRelsPath = workbookDirectory.isEmpty
        ? "_rels/\(workbookFileName).rels"
        : "\(workbookDirectory)/_rels/\(workbookFileName).rels"

    let sheetXML = sheets.enumerated().map { offset, sheet in
        let state = sheet.state.map { " state=\"\($0)\"" } ?? ""
        let relationship = sheet.relationshipID.map { " r:id=\"\($0)\"" } ?? ""
        return "<sheet name=\"\(xmlEscaped(sheet.name))\" sheetId=\"\(sheet.id ?? String(offset + 1))\"\(state)\(relationship)/>"
    }.joined()
    let workbookXML = """
    <?xml version="1.0" encoding="UTF-8"?>
    <workbook xmlns="\(workbookNamespace)" xmlns:r="\(relationshipAttributeNamespace)">
      \(workbookPrefixXML)<sheets>\(sheetXML)</sheets>\(workbookSuffixXML)
    </workbook>
    """

    let workbookRelationshipsXML = sheets.compactMap { sheet -> String? in
        guard let id = sheet.relationshipID else { return nil }
        return "<Relationship Id=\"\(id)\" Type=\"\(sheet.relationshipType)\" Target=\"\(sheet.target)\"/>"
    }.joined()
    let workbookRelsXML = """
    <?xml version="1.0" encoding="UTF-8"?>
    <Relationships xmlns="\(transitionalPackageRelationshipsNamespace)">
      \(workbookRelationshipsXML)\(extraWorkbookRelationshipXML)
    </Relationships>
    """

    let resolvedSheets = sheets.compactMap { sheet -> (FixtureSheet, String)? in
        guard let resolved = resolveForFixture(sheet.target, relativeTo: workbookPath) else { return nil }
        return (sheet, resolved)
    }
    let overrides = resolvedSheets.map { sheet, path in
        "<Override PartName=\"/\(path)\" ContentType=\"\(sheet.contentType)\"/>"
    }.joined()
    let contentTypesXML = """
    <?xml version="1.0" encoding="UTF-8"?>
    <Types xmlns="\(contentTypesNamespace)">
      <Default Extension="rels" ContentType="\(relationshipsContentType)"/>
      <Override PartName="/\(workbookPath)" ContentType="\(workbookContentTypeValue)"/>
      \(overrides)\(extraContentTypeXML)
    </Types>
    """

    let targetMode = rootTargetMode.map { " TargetMode=\"\($0)\"" } ?? ""
    let rootRelsXML = """
    <?xml version="1.0" encoding="UTF-8"?>
    <Relationships xmlns="\(rootRelationshipsNamespace)">
      <Relationship Id="rootDocument" Type="\(rootRelationshipType)" Target="\(rootTarget ?? "/" + workbookPath)"\(targetMode)/>
      \(extraRootRelationshipXML)
    </Relationships>
    """

    var entries = [
        TestArchiveEntry("[Content_Types].xml", contentTypesXML),
        TestArchiveEntry("_rels/.rels", rootRelsXML),
        TestArchiveEntry(workbookPath, workbookXML),
        TestArchiveEntry(workbookRelsPath, workbookRelsXML),
    ]
    for (_, path) in resolvedSheets where !entries.contains(where: { $0.path == path }) {
        guard sheets.first(where: { resolveForFixture($0.target, relativeTo: workbookPath) == path })?
            .includePart == true else { continue }
        entries.append(TestArchiveEntry(path, "<worksheet/>"))
    }
    return entries
}

private func makeArchive(_ entries: [TestArchiveEntry]) throws -> Data {
    let archive = try Archive(accessMode: .create)
    for entry in entries {
        try archive.addEntry(
            with: entry.path,
            type: entry.type,
            uncompressedSize: Int64(entry.data.count),
            permissions: entry.permissions,
            compressionMethod: entry.compression,
            bufferSize: 64 * 1_024
        ) { position, size in
            let lower = Int(position)
            let upper = min(lower + size, entry.data.count)
            guard lower <= upper else { return Data() }
            return entry.data.subdata(in: lower..<upper)
        }
    }
    return try #require(archive.data)
}

private func replacingEntryText(
    _ entries: [TestArchiveEntry],
    path: String,
    transform: (String) throws -> String
) throws -> [TestArchiveEntry] {
    var result = entries
    let index = try #require(result.firstIndex(where: { $0.path == path }))
    let original = try #require(String(data: result[index].data, encoding: .utf8))
    result[index] = TestArchiveEntry(path, try transform(original))
    return result
}

private func inspectError(
    _ data: Data,
    limits: CanonicalXLSXInspectionLimits = xlsxLimits
) -> CanonicalXLSXWorkbookInspectionError? {
    do {
        _ = try CanonicalXLSXWorkbookInspector.inspect(data: data, limits: limits)
        return nil
    } catch let error as CanonicalXLSXWorkbookInspectionError {
        return error
    } catch {
        Issue.record("Unexpected error type: \(error)")
        return nil
    }
}

private func customLimits(
    source: Int? = nil,
    entries: Int? = nil,
    path: Int? = nil,
    part: Int? = nil,
    total: Int? = nil,
    ratio: Int? = nil,
    worksheets: Int? = nil,
    contentTypes: Int? = nil,
    relationships: Int? = nil,
    depth: Int? = nil,
    attributes: Int? = nil,
    elements: Int? = nil,
    text: Int? = nil
) -> CanonicalXLSXInspectionLimits {
    CanonicalXLSXInspectionLimits(
        maximumSourceByteCount: source ?? xlsxLimits.maximumSourceByteCount,
        maximumZIPEntryCount: entries ?? xlsxLimits.maximumZIPEntryCount,
        maximumArchivePathUTF8ByteCount: path ?? xlsxLimits.maximumArchivePathUTF8ByteCount,
        maximumPartUncompressedByteCount: part ?? xlsxLimits.maximumPartUncompressedByteCount,
        maximumTotalUncompressedByteCount: total ?? xlsxLimits.maximumTotalUncompressedByteCount,
        maximumCompressionRatio: ratio ?? xlsxLimits.maximumCompressionRatio,
        maximumWorksheetCount: worksheets ?? xlsxLimits.maximumWorksheetCount,
        maximumContentTypeDeclarationCount:
            contentTypes ?? xlsxLimits.maximumContentTypeDeclarationCount,
        maximumRelationshipCountPerPart:
            relationships ?? xlsxLimits.maximumRelationshipCountPerPart,
        maximumXMLDepth: depth ?? xlsxLimits.maximumXMLDepth,
        maximumXMLAttributeCountPerElement:
            attributes ?? xlsxLimits.maximumXMLAttributeCountPerElement,
        maximumXMLElementCountPerPart: elements ?? xlsxLimits.maximumXMLElementCountPerPart,
        maximumXMLTextUTF8ByteCount: text ?? xlsxLimits.maximumXMLTextUTF8ByteCount
    )
}

private func xmlEscaped(_ value: String) -> String {
    value
        .replacingOccurrences(of: "&", with: "&amp;")
        .replacingOccurrences(of: "\"", with: "&quot;")
        .replacingOccurrences(of: "<", with: "&lt;")
        .replacingOccurrences(of: ">", with: "&gt;")
}

private func resolveForFixture(_ target: String, relativeTo source: String) -> String? {
    var components = target.hasPrefix("/")
        ? []
        : source.split(separator: "/").dropLast().map(String.init)
    for component in target.split(separator: "/") {
        if component == "." { continue }
        if component == ".." {
            guard !components.isEmpty else { return nil }
            components.removeLast()
        } else {
            components.append(String(component))
        }
    }
    return components.joined(separator: "/")
}

private func littleEndian16(_ data: Data, at offset: Int) -> UInt16 {
    UInt16(data[offset]) | (UInt16(data[offset + 1]) << 8)
}

private func littleEndian32(_ data: Data, at offset: Int) -> UInt32 {
    UInt32(data[offset])
        | (UInt32(data[offset + 1]) << 8)
        | (UInt32(data[offset + 2]) << 16)
        | (UInt32(data[offset + 3]) << 24)
}

private func centralDirectoryOffsets(_ data: Data) -> [Int] {
    var result: [Int] = []
    var offset = 0
    while offset + 4 <= data.count {
        if littleEndian32(data, at: offset) == 0x0201_4b50 { result.append(offset) }
        offset += 1
    }
    return result
}

private func localPayloadOffset(_ data: Data, entryPath: String) -> Int? {
    for central in centralDirectoryOffsets(data) {
        let nameLength = Int(littleEndian16(data, at: central + 28))
        let nameStart = central + 46
        guard nameStart + nameLength <= data.count,
              String(data: data[nameStart..<(nameStart + nameLength)], encoding: .utf8) == entryPath else {
            continue
        }
        let local = Int(littleEndian32(data, at: central + 42))
        guard local + 30 <= data.count else { return nil }
        return local + 30
            + Int(littleEndian16(data, at: local + 26))
            + Int(littleEndian16(data, at: local + 28))
    }
    return nil
}

private func centralDirectoryOffset(_ data: Data, entryPath: String) -> Int? {
    for central in centralDirectoryOffsets(data) {
        let nameLength = Int(littleEndian16(data, at: central + 28))
        let nameStart = central + 46
        guard nameStart + nameLength <= data.count else { continue }
        if String(data: data[nameStart..<(nameStart + nameLength)], encoding: .utf8) == entryPath {
            return central
        }
    }
    return nil
}

private func writeLittleEndian32(_ value: UInt32, to data: inout Data, at offset: Int) {
    data[offset] = UInt8(truncatingIfNeeded: value)
    data[offset + 1] = UInt8(truncatingIfNeeded: value >> 8)
    data[offset + 2] = UInt8(truncatingIfNeeded: value >> 16)
    data[offset + 3] = UInt8(truncatingIfNeeded: value >> 24)
}

private func writeLittleEndian16(_ value: UInt16, to data: inout Data, at offset: Int) {
    data[offset] = UInt8(truncatingIfNeeded: value)
    data[offset + 1] = UInt8(truncatingIfNeeded: value >> 8)
}

private func replaceDeclaredUncompressedSize(
    _ size: UInt32,
    in data: inout Data,
    entryPath: String
) throws {
    let central = try #require(centralDirectoryOffset(data, entryPath: entryPath))
    let local = Int(littleEndian32(data, at: central + 42))
    writeLittleEndian32(size, to: &data, at: central + 24)
    writeLittleEndian32(size, to: &data, at: local + 22)
}

private func totalCentralDirectoryUncompressedSize(_ data: Data) -> Int {
    centralDirectoryOffsets(data).reduce(0) { partial, offset in
        partial + Int(littleEndian32(data, at: offset + 24))
    }
}

@Test
func inspectionCatalogsRelocatedWorksheetsWithoutChoosingOne() throws {
    let data = try makeArchive(fixtureEntries())
    let inspection = try CanonicalXLSXWorkbookInspector.inspect(data: data, limits: xlsxLimits)
    let expectedDigest = SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()

    #expect(inspection.sourceSHA256 == expectedDigest)
    #expect(inspection.sourceByteCount == data.count)
    #expect(inspection.limits == xlsxLimits)
    #expect(inspection.sourceData == data)
    #expect(inspection.worksheets.map(\.name) == ["Unit A", "Unit B", "Events"])
    #expect(inspection.worksheets.map(\.sheetID) == [1, 2, 3])
    #expect(inspection.worksheets.map(\.relationshipID) == ["sheetA", "sheetB", "sheetEvents"])
    #expect(inspection.worksheets.map(\.normalizedPartPath) == [
        "pkg/sheets/a.xml", "pkg/sheets/b.xml", "pkg/sheets/events.xml",
    ])
    #expect(inspection.worksheets.map(\.visibility) == [.visible, .hidden, .veryHidden])
    #expect(inspection.worksheets.allSatisfy {
        $0.inspectionBinding == inspection.inspectionBinding
    })
}

@Test
func equalBytesHaveEqualDigestButIndependentUnforgeableBindings() throws {
    let data = try makeArchive(fixtureEntries())
    let first = try CanonicalXLSXWorkbookInspector.inspect(data: data, limits: xlsxLimits)
    let second = try CanonicalXLSXWorkbookInspector.inspect(data: data, limits: xlsxLimits)

    #expect(first.sourceSHA256 == second.sourceSHA256)
    #expect(first.sourceData == second.sourceData)
    #expect(first.inspectionBinding != second.inspectionBinding)
    #expect(first.worksheets[0].inspectionBinding == first.inspectionBinding)
    #expect(first.worksheets[0].inspectionBinding != second.inspectionBinding)
}

@Test
func sourceAndEntryLimitsAreExactAndFailAtOneLess() throws {
    let data = try makeArchive(fixtureEntries())
    _ = try CanonicalXLSXWorkbookInspector.inspect(
        data: data,
        limits: customLimits(source: data.count, entries: 7)
    )

    #expect(inspectError(data, limits: customLimits(source: data.count - 1)) ==
        .sourceByteLimitExceeded(maximum: data.count - 1, actual: data.count))
    #expect(inspectError(data, limits: customLimits(entries: 6)) ==
        .zipEntryLimitExceeded(maximum: 6, actual: 7))
}

@Test
func everyLimitMustBePositiveAndWithinTheSupportedEnvelope() {
    #expect(inspectError(Data(), limits: customLimits(depth: 0)) ==
        .invalidLimit(kind: .xmlDepth, actual: 0))
    #expect(inspectError(Data(), limits: customLimits(worksheets: 65)) ==
        .limitExceedsSupportedEnvelope(kind: .worksheets, maximumSupported: 64, actual: 65))
}

@Test
func everyLimitFieldIsValidatedBeforeAnyArchiveParsing() {
    let zeroCases: [(CanonicalXLSXInspectionLimitKind, CanonicalXLSXInspectionLimits)] = [
        (.sourceBytes, customLimits(source: 0)),
        (.zipEntries, customLimits(entries: 0)),
        (.archivePathUTF8Bytes, customLimits(path: 0)),
        (.partUncompressedBytes, customLimits(part: 0)),
        (.totalUncompressedBytes, customLimits(total: 0)),
        (.compressionRatio, customLimits(ratio: 0)),
        (.worksheets, customLimits(worksheets: 0)),
        (.contentTypeDeclarations, customLimits(contentTypes: 0)),
        (.relationshipsPerPart, customLimits(relationships: 0)),
        (.xmlDepth, customLimits(depth: 0)),
        (.xmlAttributesPerElement, customLimits(attributes: 0)),
        (.xmlElementsPerPart, customLimits(elements: 0)),
        (.xmlTextUTF8Bytes, customLimits(text: 0)),
    ]
    for (kind, limits) in zeroCases {
        #expect(inspectError(Data(), limits: limits) == .invalidLimit(kind: kind, actual: 0))
    }

    let supported = xlsxLimits
    let excessiveCases: [(CanonicalXLSXInspectionLimitKind, Int, CanonicalXLSXInspectionLimits)] = [
        (.sourceBytes, supported.maximumSourceByteCount, customLimits(source: supported.maximumSourceByteCount + 1)),
        (.zipEntries, supported.maximumZIPEntryCount, customLimits(entries: supported.maximumZIPEntryCount + 1)),
        (.archivePathUTF8Bytes, supported.maximumArchivePathUTF8ByteCount, customLimits(path: supported.maximumArchivePathUTF8ByteCount + 1)),
        (.partUncompressedBytes, supported.maximumPartUncompressedByteCount, customLimits(part: supported.maximumPartUncompressedByteCount + 1)),
        (.totalUncompressedBytes, supported.maximumTotalUncompressedByteCount, customLimits(total: supported.maximumTotalUncompressedByteCount + 1)),
        (.compressionRatio, supported.maximumCompressionRatio, customLimits(ratio: supported.maximumCompressionRatio + 1)),
        (.worksheets, supported.maximumWorksheetCount, customLimits(worksheets: supported.maximumWorksheetCount + 1)),
        (.contentTypeDeclarations, supported.maximumContentTypeDeclarationCount, customLimits(contentTypes: supported.maximumContentTypeDeclarationCount + 1)),
        (.relationshipsPerPart, supported.maximumRelationshipCountPerPart, customLimits(relationships: supported.maximumRelationshipCountPerPart + 1)),
        (.xmlDepth, supported.maximumXMLDepth, customLimits(depth: supported.maximumXMLDepth + 1)),
        (.xmlAttributesPerElement, supported.maximumXMLAttributeCountPerElement, customLimits(attributes: supported.maximumXMLAttributeCountPerElement + 1)),
        (.xmlElementsPerPart, supported.maximumXMLElementCountPerPart, customLimits(elements: supported.maximumXMLElementCountPerPart + 1)),
        (.xmlTextUTF8Bytes, supported.maximumXMLTextUTF8ByteCount, customLimits(text: supported.maximumXMLTextUTF8ByteCount + 1)),
    ]
    for (kind, maximum, limits) in excessiveCases {
        #expect(inspectError(Data(), limits: limits) == .limitExceedsSupportedEnvelope(
            kind: kind,
            maximumSupported: maximum,
            actual: maximum + 1
        ))
    }
}

@Test
func archivePathLimitAndUnsafePathsFailClosed() throws {
    let base = fixtureEntries()
    let longPath = "extra/" + String(repeating: "x", count: 40)
    let longArchive = try makeArchive(base + [TestArchiveEntry(longPath, "x")])
    _ = try CanonicalXLSXWorkbookInspector.inspect(
        data: longArchive,
        limits: customLimits(path: longPath.utf8.count)
    )
    #expect(inspectError(longArchive, limits: customLimits(path: longPath.utf8.count - 1)) ==
        .archivePathByteLimitExceeded(
            path: longPath,
            maximum: longPath.utf8.count - 1,
            actual: longPath.utf8.count
        ))

    for (path, expectedIssue) in [
        ("/absolute.xml", CanonicalXLSXArchivePathIssue.absolute),
        ("bad\\path.xml", .backslash),
        ("bad//path.xml", .emptySegment),
        ("bad/./path.xml", .dotSegment),
        ("bad/../path.xml", .parentSegment),
    ] {
        let data = try makeArchive(base + [TestArchiveEntry(path, "x")])
        #expect(inspectError(data) == .unsafeArchivePath(path: path, issue: expectedIssue))
    }
}

@Test
func duplicateAndCanonicalArchivePathsAreRejected() throws {
    let base = fixtureEntries()
    let exact = try makeArchive(base + [TestArchiveEntry("extra.xml", "a"), TestArchiveEntry("extra.xml", "b")])
    #expect(inspectError(exact) == .duplicateArchivePath(path: "extra.xml"))

    let caseCollision = try makeArchive(
        base + [TestArchiveEntry("Extra.xml", "a"), TestArchiveEntry("extra.XML", "b")]
    )
    guard case .canonicalArchivePathCollision = inspectError(caseCollision) else {
        Issue.record("Expected canonical archive path collision")
        return
    }

    let composed = "é.xml"
    let decomposed = "e\u{301}.xml"
    let unicodeCollision = try makeArchive(
        base + [TestArchiveEntry(composed, "a"), TestArchiveEntry(decomposed, "b")]
    )
    switch inspectError(unicodeCollision) {
    case .canonicalArchivePathCollision, .duplicateArchivePath:
        break // ZIPFoundation may NFC-normalize both spellings before exposing Entry.path.
    default:
        Issue.record("Expected safe rejection of Unicode-equivalent archive paths")
    }
}

@Test
func symlinksRejectButTrailingSlashFileClassificationIsTreatedAsDirectory() throws {
    let base = fixtureEntries()
    let symlink = try makeArchive(
        base + [TestArchiveEntry("link", "target", type: .symlink)]
    )
    #expect(inspectError(symlink) == .symbolicLinkEntry(path: "link"))

    let trailingSlash = try makeArchive(
        base + [TestArchiveEntry("odd/", "", type: .file, permissions: 0o644)]
    )
    let inspection = try CanonicalXLSXWorkbookInspector.inspect(
        data: trailingSlash,
        limits: xlsxLimits
    )
    #expect(inspection.worksheets.count == 3)
}

@Test
func corruptedPayloadFailsChecksumBeforeXMLInterpretation() throws {
    var data = try makeArchive(fixtureEntries())
    let payload = try #require(localPayloadOffset(data, entryPath: "[Content_Types].xml"))
    data[payload] ^= 0x01

    guard case .checksumMismatch(let path, _, _) = inspectError(data) else {
        Issue.record("Expected checksumMismatch")
        return
    }
    #expect(path == "[Content_Types].xml")
}

@Test
func actualExtractionBudgetsAndDeclaredActualEqualityAreIndependentOfZIPMetadata() throws {
    var entries = fixtureEntries(extraContentTypeXML: String(repeating: " ", count: 12_000))
    let contentTypesIndex = try #require(
        entries.firstIndex(where: { $0.path == "[Content_Types].xml" })
    )
    let contentTypes = entries[contentTypesIndex]
    entries[contentTypesIndex] = TestArchiveEntry(
        contentTypes.path,
        data: contentTypes.data,
        compression: .deflate
    )

    var understated = try makeArchive(entries)
    try replaceDeclaredUncompressedSize(100, in: &understated, entryPath: "[Content_Types].xml")
    let understatedTotal = totalCentralDirectoryUncompressedSize(understated)

    guard case .partActualByteLimitExceeded(let part, let maximum, let actual) = inspectError(
        understated,
        limits: customLimits(part: 2_000)
    ) else {
        Issue.record("Expected independently measured part-byte limit")
        return
    }
    #expect(part == "[Content_Types].xml")
    #expect(maximum == 2_000)
    #expect(actual > 2_000)

    guard case .totalActualByteLimitExceeded(let totalMaximum, let totalActual) = inspectError(
        understated,
        limits: customLimits(total: understatedTotal)
    ) else {
        Issue.record("Expected independently measured total-byte limit")
        return
    }
    #expect(totalMaximum == understatedTotal)
    #expect(totalActual > UInt64(understatedTotal))

    var overstated = try makeArchive(entries)
    let actualContentTypesSize = UInt32(contentTypes.data.count)
    try replaceDeclaredUncompressedSize(
        actualContentTypesSize + 1,
        in: &overstated,
        entryPath: "[Content_Types].xml"
    )
    #expect(inspectError(overstated) == .declaredActualByteCountMismatch(
        path: "[Content_Types].xml",
        declared: UInt64(actualContentTypesSize + 1),
        actual: UInt64(actualContentTypesSize)
    ))
}

@Test
func declaredPartAndTotalBudgetsUseAllSafeEntries() throws {
    let entries = fixtureEntries() + [TestArchiveEntry("unknown.bin", String(repeating: "z", count: 4_000))]
    let data = try makeArchive(entries)
    let maximumPart = entries.map(\.data.count).max()!
    let total = entries.reduce(0) { $0 + $1.data.count }

    _ = try CanonicalXLSXWorkbookInspector.inspect(
        data: data,
        limits: customLimits(part: maximumPart, total: total)
    )
    guard case .partDeclaredByteLimitExceeded(let path, let maximum, let actual) =
        inspectError(data, limits: customLimits(part: maximumPart - 1)) else {
        Issue.record("Expected declared part limit")
        return
    }
    #expect(path == "unknown.bin")
    #expect(maximum == maximumPart - 1)
    #expect(actual == UInt64(maximumPart))
    #expect(inspectError(data, limits: customLimits(total: total - 1)) ==
        .totalDeclaredByteLimitExceeded(maximum: total - 1, actual: UInt64(total)))
}

@Test
func compressionRatioLimitAppliesToUnknownEntries() throws {
    let entries = fixtureEntries() + [
        TestArchiveEntry(
            "unknown.bin",
            String(repeating: "A", count: 10_000),
            compression: .deflate
        ),
    ]
    let data = try makeArchive(entries)
    guard case .compressionRatioExceeded(let path, let maximum, _, _) =
        inspectError(data, limits: customLimits(ratio: 2)) else {
        Issue.record("Expected compression ratio limit")
        return
    }
    #expect(path == "unknown.bin")
    #expect(maximum == 2)
}

@Test
func missingDuplicateExternalAndUnsafeRootRelationshipsFail() throws {
    let missingOffice = try makeArchive(
        fixtureEntries(rootRelationshipType: officeDocumentRelationship + "/wrong")
    )
    guard case .invalidRelationships(_, .missingOfficeDocument, _, _) = inspectError(missingOffice) else {
        Issue.record("Expected missing office document")
        return
    }

    let duplicateOffice = try makeArchive(fixtureEntries(
        extraRootRelationshipXML:
            "<Relationship Id=\"second\" Type=\"\(officeDocumentRelationship)\" Target=\"/other.xml\"/>"
    ) + [TestArchiveEntry("other.xml", "<workbook/>")])
    guard case .invalidRelationships(_, .multipleOfficeDocuments, _, _) = inspectError(duplicateOffice) else {
        Issue.record("Expected multiple office documents")
        return
    }

    let external = try makeArchive(fixtureEntries(rootTargetMode: "External"))
    guard case .invalidRelationships(_, .externalTargetUnsupported, _, _) = inspectError(external) else {
        Issue.record("Expected external relationship rejection")
        return
    }

    let unsafe = try makeArchive(fixtureEntries(rootTarget: "../../escape.xml"))
    guard case .invalidRelationships(_, .invalidTarget, _, _) = inspectError(unsafe) else {
        Issue.record("Expected unsafe relationship rejection")
        return
    }


    let percentEncodedUnsafe = try makeArchive(
        fixtureEntries(rootTarget: "%2e%2e/%2e%2e/escape.xml")
    )
    guard case .invalidRelationships(_, .invalidTarget, _, _) =
        inspectError(percentEncodedUnsafe) else {
        Issue.record("Expected percent-decoded unsafe relationship rejection")
        return
    }

    let encodedQuery = try makeArchive(fixtureEntries(rootTarget: "pkg/books/main.xml%3Fother"))
    guard case .invalidRelationships(_, .invalidTarget, _, _) = inspectError(encodedQuery) else {
        Issue.record("Expected post-decode query rejection")
        return
    }

    let explicitInternal = try makeArchive(fixtureEntries(rootTargetMode: "Internal"))
    _ = try CanonicalXLSXWorkbookInspector.inspect(data: explicitInternal, limits: xlsxLimits)

    for directoryTarget in [".", "a/.."] {
        let directoryRelationship = try makeArchive(fixtureEntries(
            extraWorkbookRelationshipXML:
                "<Relationship Id=\"directoryTarget\" Type=\"urn:other\" Target=\"\(directoryTarget)\"/>"
        ))
        guard case .invalidRelationships(_, .invalidTarget, let id, let target) =
            inspectError(directoryRelationship) else {
            Issue.record("Expected final dot-segment relationship rejection")
            return
        }
        #expect(id == "directoryTarget")
        #expect(target == directoryTarget)
    }
}

@Test
func duplicateRelationshipIDsAndTargetsFail() throws {
    let duplicateID = try makeArchive(fixtureEntries(
        extraWorkbookRelationshipXML:
            "<Relationship Id=\"sheetA\" Type=\"urn:other\" Target=\"../other.xml\"/>"
    ) + [TestArchiveEntry("pkg/other.xml", "x")])
    guard case .invalidRelationships(_, .duplicateID, _, _) = inspectError(duplicateID) else {
        Issue.record("Expected duplicate relationship ID")
        return
    }

    let duplicateTarget = try makeArchive(fixtureEntries(
        extraWorkbookRelationshipXML:
            "<Relationship Id=\"other\" Type=\"urn:other\" Target=\"../sheets/a.xml\"/>"
    ))
    guard case .invalidRelationships(_, .duplicateTarget, _, _) = inspectError(duplicateTarget) else {
        Issue.record("Expected duplicate relationship target")
        return
    }
}

@Test
func missingRelationshipPartAndTypeMismatchFailWithContext() throws {
    let missingPart = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(
            name: "Missing",
            relationshipID: "missing",
            target: "../sheets/missing.xml",
            includePart: false
        ),
    ]))
    guard case .invalidContentTypes(_, .overrideTargetsMissingPart, let target) =
        inspectError(missingPart) else {
        Issue.record("Expected missing override target part")
        return
    }
    #expect(target == "pkg/sheets/missing.xml")

    let missingRelationshipTarget = try makeArchive(fixtureEntries(
        extraWorkbookRelationshipXML:
            "<Relationship Id=\"missingOther\" Type=\"urn:other\" Target=\"../missing.bin\"/>"
    ))
    guard case .invalidRelationships(_, .missingTargetPart, let id, let relationshipTarget) =
        inspectError(missingRelationshipTarget) else {
        Issue.record("Expected missing relationship target")
        return
    }
    #expect(id == "missingOther")
    #expect(relationshipTarget == "pkg/missing.bin")

    let wrongType = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(
            name: "Wrong",
            relationshipID: "wrong",
            target: "../sheets/wrong.xml",
            relationshipType: "urn:not-a-worksheet"
        ),
    ]))
    guard case .invalidRelationships(_, .relationshipTypeMismatch, let id, _) =
        inspectError(wrongType) else {
        Issue.record("Expected relationship type mismatch")
        return
    }
    #expect(id == "wrong")
}

@Test
func missingSheetRelationshipIDAndUnreferencedWorksheetRelationshipFail() throws {
    let missingID = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(name: "No ID", relationshipID: nil, target: "../sheets/a.xml"),
    ]))
    guard case .invalidWorkbook(_, .missingSheetAttribute, let name, _) = inspectError(missingID) else {
        Issue.record("Expected missing sheet r:id")
        return
    }
    #expect(name == "No ID")

    let unreferenced = try makeArchive(fixtureEntries(
        extraWorkbookRelationshipXML:
            "<Relationship Id=\"orphan\" Type=\"\(transitionalDocumentRelationshipsNamespace)/worksheet\" Target=\"../sheets/orphan.xml\"/>"
    ) + [TestArchiveEntry("pkg/sheets/orphan.xml", "<worksheet/>")])
    guard case .invalidRelationships(_, .unreferencedWorksheetRelationship, let id, _) =
        inspectError(unreferenced) else {
        Issue.record("Expected orphan worksheet relationship")
        return
    }
    #expect(id == "orphan")
}

@Test
func workbookAndWorksheetContentTypesAreFailClosed() throws {
    let macro = try makeArchive(fixtureEntries(
        workbookContentTypeValue: "application/vnd.ms-excel.sheet.macroEnabled.main+xml"
    ))
    guard case .invalidContentTypes(_, .macroEnabledWorkbookUnsupported, _) = inspectError(macro) else {
        Issue.record("Expected macro workbook rejection")
        return
    }

    let defaultVBA = try makeArchive(fixtureEntries(
        extraContentTypeXML:
            "<Default Extension=\"bin\" ContentType=\"application/vnd.ms-office.vbaProject\"/>"
    ))
    guard case .invalidContentTypes(_, .macroEnabledWorkbookUnsupported, let declaration) =
        inspectError(defaultVBA) else {
        Issue.record("Expected VBA Default content-type rejection")
        return
    }
    #expect(declaration == "*.bin")

    let defaultExcel4MacroSheet = try makeArchive(fixtureEntries(
        extraContentTypeXML:
            "<Default Extension=\"xlm\" ContentType=\"application/vnd.ms-excel.macrosheet+xml\"/>"
    ))
    guard case .invalidContentTypes(_, .macroEnabledWorkbookUnsupported, let xlmDefault) =
        inspectError(defaultExcel4MacroSheet) else {
        Issue.record("Expected Excel 4 macro-sheet Default content-type rejection")
        return
    }
    #expect(xlmDefault == "*.xlm")

    let overrideInternationalExcel4MacroSheet = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(
            name: "International XLM",
            relationshipID: "intlXLM",
            target: "../sheets/intl-xlm.xml",
            contentType: "application/vnd.ms-excel.intlmacrosheet+xml"
        ),
    ]))
    guard case .invalidContentTypes(_, .macroEnabledWorkbookUnsupported, let xlmOverride) =
        inspectError(overrideInternationalExcel4MacroSheet) else {
        Issue.record("Expected international Excel 4 macro-sheet Override rejection")
        return
    }
    #expect(xlmOverride == "pkg/sheets/intl-xlm.xml")

    let wrongWorkbook = try makeArchive(fixtureEntries(workbookContentTypeValue: "application/xml"))
    guard case .invalidContentTypes(_, .workbookContentTypeMismatch, _) = inspectError(wrongWorkbook) else {
        Issue.record("Expected workbook content-type mismatch")
        return
    }

    let wrongWorksheet = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(
            name: "Wrong Type",
            relationshipID: "wrong",
            target: "../sheets/wrong.xml",
            contentType: "application/xml"
        ),
    ]))
    guard case .invalidContentTypes(_, .worksheetContentTypeMismatch, _) = inspectError(wrongWorksheet) else {
        Issue.record("Expected worksheet content-type mismatch")
        return
    }
}

@Test
func Excel4MacroSheetWorkbookRelationshipsAreExplicitlyUnsupported() throws {
    let relationshipTypes = [
        "http://schemas.microsoft.com/office/2006/relationships/xlMacrosheet",
        "http://schemas.microsoft.com/office/2006/relationships/xlIntlMacrosheet",
    ]
    for (offset, relationshipType) in relationshipTypes.enumerated() {
        let relationshipID = "xlm\(offset + 1)"
        let target = "../sheets/xlm\(offset + 1).xml"
        let data = try makeArchive(fixtureEntries(sheets: [
            FixtureSheet(
                name: offset == 0 ? "XLM" : "International XLM",
                relationshipID: relationshipID,
                target: target,
                relationshipType: relationshipType
            ),
        ]))
        guard case .invalidRelationships(
            part: "pkg/books/_rels/main.xml.rels",
            issue: .macroSheetRelationshipUnsupported,
            relationshipID: let rejectedID,
            target: let rejectedTarget
        ) = inspectError(data) else {
            Issue.record("Expected Excel 4 macro-sheet relationship rejection")
            return
        }
        #expect(rejectedID == relationshipID)
        #expect(rejectedTarget == target)
    }
}

@Test
func strictNamespacesAndRelationshipTypesAreExplicitlyUnsupported() throws {
    let strictWorkbook = try makeArchive(fixtureEntries(
        workbookNamespace: "http://purl.oclc.org/ooxml/spreadsheetml/main"
    ))
    guard case .unsupportedConformance(let part, let value) = inspectError(strictWorkbook) else {
        Issue.record("Expected Strict workbook rejection")
        return
    }
    #expect(part == "pkg/books/main.xml")
    #expect(value.contains("purl.oclc.org/ooxml"))

    let strictRelationship = try makeArchive(fixtureEntries(
        rootRelationshipType:
            "http://purl.oclc.org/ooxml/officeDocument/relationships/officeDocument"
    ))
    guard case .unsupportedConformance(_, let type) = inspectError(strictRelationship) else {
        Issue.record("Expected Strict relationship rejection")
        return
    }
    #expect(type.contains("purl.oclc.org/ooxml"))
}

@Test
func documentTypeAndEntityDeclarationsAreRejectedBeforeParsing() throws {
    var entries = fixtureEntries()
    let workbookIndex = try #require(entries.firstIndex(where: { $0.path == "pkg/books/main.xml" }))
    let dtd = """
    <!DOCTYPE workbook [<!ENTITY x "value">]>
    <workbook xmlns="\(transitionalSpreadsheetNamespace)" xmlns:r="\(transitionalDocumentRelationshipsNamespace)">
      <sheets><sheet name="&x;" sheetId="1" r:id="sheetA"/></sheets>
    </workbook>
    """
    entries[workbookIndex] = TestArchiveEntry("pkg/books/main.xml", dtd)
    let data = try makeArchive(entries)

    #expect(inspectError(data) == .xmlSecurityViolation(
        part: "pkg/books/main.xml",
        issue: .documentTypeDeclaration
    ))
}

@Test
func XMLDepthAttributeElementAndTextLimitsAreEnforced() throws {
    let depthData = try makeArchive(fixtureEntries(
        workbookPrefixXML: "<a><b><c><d></d></c></b></a>"
    ))
    guard case .xmlLimitExceeded(_, .xmlDepth, 4, 5) =
        inspectError(depthData, limits: customLimits(depth: 4)) else {
        Issue.record("Expected XML depth limit")
        return
    }

    let attributeData = try makeArchive(fixtureEntries(
        workbookPrefixXML: "<x a=\"1\" b=\"2\" c=\"3\"/>"
    ))
    guard case .xmlLimitExceeded(_, .xmlAttributesPerElement, 2, 3) =
        inspectError(attributeData, limits: customLimits(attributes: 2)) else {
        Issue.record("Expected XML attribute limit")
        return
    }

    let elementData = try makeArchive(fixtureEntries())
    guard case .xmlLimitExceeded(_, .xmlElementsPerPart, 3, 4) =
        inspectError(elementData, limits: customLimits(elements: 3)) else {
        Issue.record("Expected XML element limit")
        return
    }

    let textData = try makeArchive(fixtureEntries(
        workbookPrefixXML: "<x>12345</x>"
    ))
    guard case .xmlLimitExceeded(_, .xmlTextUTF8Bytes, 4, _) =
        inspectError(textData, limits: customLimits(text: 4)) else {
        Issue.record("Expected XML text limit")
        return
    }
}

@Test
func elementNamesCommentsAndProcessingInstructionsObeyXMLTextLimit() throws {
    let oversized = String(repeating: "x", count: 101)
    let fragments = [
        "<\(oversized)/>",
        "<!--\(oversized)-->",
        "<?\(oversized)?>",
        "<?probe \(oversized)?>",
    ]
    for fragment in fragments {
        let data = try makeArchive(fixtureEntries(workbookPrefixXML: fragment))
        guard case .xmlLimitExceeded(
            part: "pkg/books/main.xml",
            kind: .xmlTextUTF8Bytes,
            maximum: 100,
            actual: let actual
        ) = inspectError(data, limits: customLimits(text: 100)) else {
            Issue.record("Expected XML callback text limit for fragment \(fragment.prefix(16))")
            return
        }
        #expect(actual > 100)
    }
}

@Test
func duplicateExactAndCanonicalWorksheetNamesAreRejected() throws {
    let exact = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(name: "Same", relationshipID: "a", target: "../sheets/a.xml"),
        FixtureSheet(name: "Same", relationshipID: "b", target: "../sheets/b.xml"),
    ]))
    guard case .invalidWorkbook(_, .duplicateSheetName, _, _) = inspectError(exact) else {
        Issue.record("Expected duplicate sheet name")
        return
    }

    let canonical = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(name: "Événement", relationshipID: "a", target: "../sheets/a.xml"),
        FixtureSheet(name: "e\u{301}VÉNEMENT", relationshipID: "b", target: "../sheets/b.xml"),
    ]))
    guard case .invalidWorkbook(_, .canonicalSheetNameCollision, _, _) = inspectError(canonical) else {
        Issue.record("Expected canonical sheet name collision")
        return
    }
}

@Test
func sheetIDsArePositiveUniqueASCIIUInt32Values() throws {
    let missingIDEntries = try replacingEntryText(
        fixtureEntries(),
        path: "pkg/books/main.xml"
    ) { source in
        source.replacingOccurrences(of: " sheetId=\"1\"", with: "", options: [], range: source.range(of: " sheetId=\"1\""))
    }
    guard case .invalidWorkbook(_, .invalidSheetID, let missingName, _) =
        inspectError(try makeArchive(missingIDEntries)) else {
        Issue.record("Expected missing sheetId rejection")
        return
    }
    #expect(missingName == "Unit A")

    for invalidID in ["0", "1.0", "4294967296", "-1"] {
        let data = try makeArchive(fixtureEntries(sheets: [
            FixtureSheet(
                name: "Invalid ID",
                id: invalidID,
                relationshipID: "invalid",
                target: "../sheets/invalid.xml"
            ),
        ]))
        guard case .invalidWorkbook(_, .invalidSheetID, let name, _) = inspectError(data) else {
            Issue.record("Expected invalid sheetId rejection for \(invalidID)")
            return
        }
        #expect(name == "Invalid ID")
    }

    let duplicate = try makeArchive(fixtureEntries(sheets: [
        FixtureSheet(name: "First", id: "1", relationshipID: "first", target: "../sheets/first.xml"),
        FixtureSheet(name: "Second", id: "01", relationshipID: "second", target: "../sheets/second.xml"),
    ]))
    guard case .invalidWorkbook(_, .duplicateSheetID, let duplicateName, _) =
        inspectError(duplicate) else {
        Issue.record("Expected numerically duplicate sheetId rejection")
        return
    }
    #expect(duplicateName == "Second")
}

@Test
func worksheetCountAndRelationshipCountLimitsFailAtPlusOne() throws {
    let data = try makeArchive(fixtureEntries())
    _ = try CanonicalXLSXWorkbookInspector.inspect(
        data: data,
        limits: customLimits(worksheets: 3, contentTypes: 5, relationships: 3)
    )
    guard case .invalidWorkbook(_, .worksheetLimitExceeded, _, _) =
        inspectError(data, limits: customLimits(worksheets: 2)) else {
        Issue.record("Expected worksheet count limit")
        return
    }
    guard case .xmlLimitExceeded(_, .relationshipsPerPart, 2, 3) =
        inspectError(data, limits: customLimits(relationships: 2)) else {
        Issue.record("Expected relationship count limit")
        return
    }
    guard case .xmlLimitExceeded(_, .contentTypeDeclarations, 4, 5) =
        inspectError(data, limits: customLimits(contentTypes: 4)) else {
        Issue.record("Expected content-type declaration count limit")
        return
    }
}

@Test
func namespaceDeclarationsShareAttributeAndTextBudgets() throws {
    let data = try makeArchive(fixtureEntries(
        workbookPrefixXML:
            "<x xmlns:a=\"urn:a\" xmlns:b=\"urn:b\" xmlns:c=\"urn:c\"/>"
    ))
    _ = try CanonicalXLSXWorkbookInspector.inspect(
        data: data,
        limits: customLimits(attributes: 4)
    )
    guard case .xmlLimitExceeded(_, .xmlAttributesPerElement, 3, 4) =
        inspectError(data, limits: customLimits(attributes: 3)) else {
        Issue.record("Expected namespace declarations to count against attribute cap")
        return
    }

    let hugeURI = String(repeating: "x", count: 200)
    let hugeNamespaceData = try makeArchive(fixtureEntries(
        workbookPrefixXML: "<x xmlns:a=\"\(hugeURI)\"/>"
    ))
    guard case .xmlLimitExceeded(_, .xmlTextUTF8Bytes, 100, 200) =
        inspectError(hugeNamespaceData, limits: customLimits(text: 100)) else {
        Issue.record("Expected namespace URI to obey XML text cap")
        return
    }
}

@Test
func malformedCentralDirectoryAndUnsupportedProfileDoNotReachZIPFoundationIterator() throws {
    var multiDisk = try makeArchive(fixtureEntries())
    let eocd = try #require(multiDisk.lastRange(of: Data([0x50, 0x4b, 0x05, 0x06])))
    multiDisk[eocd.lowerBound + 4] = 1
    #expect(inspectError(multiDisk) == .unsupportedArchiveProfile(issue: .multiDisk, detail: nil))

    var encrypted = try makeArchive(fixtureEntries())
    let firstCentral = try #require(centralDirectoryOffsets(encrypted).first)
    encrypted[firstCentral + 8] |= 0x01
    #expect(inspectError(encrypted) ==
        .unsupportedArchiveProfile(issue: .encryptedEntryUnsupported, detail: nil))

    var unsupportedCompression = try makeArchive(fixtureEntries())
    let compressionCentral = try #require(centralDirectoryOffsets(unsupportedCompression).first)
    unsupportedCompression[compressionCentral + 10] = 99
    unsupportedCompression[compressionCentral + 11] = 0
    #expect(inspectError(unsupportedCompression) ==
        .unsupportedArchiveProfile(issue: .compressionMethodUnsupported, detail: 99))

    let withUnusedFinalEntry = fixtureEntries() + [TestArchiveEntry("unused/final.bin", "unused")]
    var encryptedUnusedFinalEntry = try makeArchive(withUnusedFinalEntry)
    let unusedCentral = try #require(
        centralDirectoryOffset(encryptedUnusedFinalEntry, entryPath: "unused/final.bin")
    )
    let unusedLocal = Int(littleEndian32(encryptedUnusedFinalEntry, at: unusedCentral + 42))
    encryptedUnusedFinalEntry[unusedCentral + 8] |= 0x01
    encryptedUnusedFinalEntry[unusedLocal + 6] |= 0x01
    #expect(inspectError(encryptedUnusedFinalEntry) ==
        .unsupportedArchiveProfile(issue: .encryptedEntryUnsupported, detail: nil))

    var dataDescriptorUnusedFinalEntry = try makeArchive(withUnusedFinalEntry)
    let descriptorCentral = try #require(
        centralDirectoryOffset(dataDescriptorUnusedFinalEntry, entryPath: "unused/final.bin")
    )
    let descriptorLocal = Int(
        littleEndian32(dataDescriptorUnusedFinalEntry, at: descriptorCentral + 42)
    )
    dataDescriptorUnusedFinalEntry[descriptorCentral + 8] |= 0x08
    dataDescriptorUnusedFinalEntry[descriptorLocal + 6] |= 0x08
    #expect(inspectError(dataDescriptorUnusedFinalEntry) ==
        .unsupportedArchiveProfile(issue: .dataDescriptorUnsupported, detail: nil))
}

@Test
func centralDirectoryAndLocalHeaderMustAgreeBeforeArchiveEnumeration() throws {
    let original = try makeArchive(fixtureEntries())
    let central = try #require(centralDirectoryOffset(original, entryPath: "[Content_Types].xml"))
    let local = Int(littleEndian32(original, at: central + 42))

    var locallyEncrypted = original
    locallyEncrypted[local + 6] |= 0x01
    #expect(inspectError(locallyEncrypted) ==
        .unsupportedArchiveProfile(issue: .encryptedEntryUnsupported, detail: nil))

    var localMethod = original
    localMethod[local + 8] = 99
    localMethod[local + 9] = 0
    #expect(inspectError(localMethod) ==
        .unsupportedArchiveProfile(issue: .compressionMethodUnsupported, detail: 99))

    var mismatchedVersionNeeded = original
    let centralVersionNeeded = littleEndian16(original, at: central + 6)
    let differentSupportedVersion: UInt16 = centralVersionNeeded == 10 ? 20 : 10
    writeLittleEndian16(differentSupportedVersion, to: &mismatchedVersionNeeded, at: local + 4)
    #expect(inspectError(mismatchedVersionNeeded) ==
        .unsupportedArchiveProfile(issue: .malformedCentralDirectory, detail: nil))

    for encryptionBit: UInt16 in [0x0040, 0x2000] {
        var strongOrMaskedEncryption = original
        strongOrMaskedEncryption[central + 8] |= UInt8(truncatingIfNeeded: encryptionBit)
        strongOrMaskedEncryption[central + 9] |= UInt8(truncatingIfNeeded: encryptionBit >> 8)
        strongOrMaskedEncryption[local + 6] |= UInt8(truncatingIfNeeded: encryptionBit)
        strongOrMaskedEncryption[local + 7] |= UInt8(truncatingIfNeeded: encryptionBit >> 8)
        #expect(inspectError(strongOrMaskedEncryption) ==
            .unsupportedArchiveProfile(issue: .encryptedEntryUnsupported, detail: nil))
    }

    var unsupportedFlags = original
    unsupportedFlags[central + 8] |= 0x10
    unsupportedFlags[local + 6] |= 0x10
    #expect(inspectError(unsupportedFlags) == .unsupportedArchiveProfile(
        issue: .unsupportedGeneralPurposeFlags,
        detail: UInt64(littleEndian16(unsupportedFlags, at: central + 8))
    ))

    var mismatchedName = original
    let localNameStart = local + 30
    mismatchedName[localNameStart] ^= 0x01
    #expect(inspectError(mismatchedName) ==
        .unsupportedArchiveProfile(issue: .malformedCentralDirectory, detail: nil))

    var centralGap = original
    let eocd = try #require(centralGap.lastRange(of: Data([0x50, 0x4b, 0x05, 0x06])))
    centralGap.insert(0, at: eocd.lowerBound)
    #expect(inspectError(centralGap) ==
        .unsupportedArchiveProfile(issue: .malformedCentralDirectory, detail: nil))

    var zip64Locator = original
    let locatorOffset = try #require(
        zip64Locator.lastRange(of: Data([0x50, 0x4b, 0x05, 0x06]))?.lowerBound
    )
    var locator = Data(repeating: 0, count: 20)
    locator[0] = 0x50
    locator[1] = 0x4b
    locator[2] = 0x06
    locator[3] = 0x07
    zip64Locator.insert(contentsOf: locator, at: locatorOffset)
    #expect(inspectError(zip64Locator) ==
        .unsupportedArchiveProfile(issue: .zip64Unsupported, detail: nil))

    var zip64Version = original
    writeLittleEndian16(45, to: &zip64Version, at: central + 6)
    writeLittleEndian16(45, to: &zip64Version, at: local + 4)
    #expect(inspectError(zip64Version) ==
        .unsupportedArchiveProfile(issue: .zip64Unsupported, detail: 45))

    var zip64Extra = original
    let originalCentralNameLength = Int(littleEndian16(zip64Extra, at: central + 28))
    let originalLocalNameLength = Int(littleEndian16(zip64Extra, at: local + 26))
    #expect(originalCentralNameLength == originalLocalNameLength)
    let reducedNameLength = UInt16(originalCentralNameLength - 4)
    writeLittleEndian16(reducedNameLength, to: &zip64Extra, at: central + 28)
    writeLittleEndian16(4, to: &zip64Extra, at: central + 30)
    writeLittleEndian16(reducedNameLength, to: &zip64Extra, at: local + 26)
    writeLittleEndian16(4, to: &zip64Extra, at: local + 28)
    let centralExtra = central + 46 + Int(reducedNameLength)
    let localExtra = local + 30 + Int(reducedNameLength)
    for offset in [centralExtra, localExtra] {
        zip64Extra[offset] = 0x01
        zip64Extra[offset + 1] = 0x00
        zip64Extra[offset + 2] = 0x00
        zip64Extra[offset + 3] = 0x00
    }
    #expect(inspectError(zip64Extra) ==
        .unsupportedArchiveProfile(issue: .zip64Unsupported, detail: nil))
}

@Test
func relevantOOXMLElementsMustAppearAtTheirExactSchemaDepth() throws {
    let contentTypesEntries = try replacingEntryText(
        fixtureEntries(),
        path: "[Content_Types].xml"
    ) { source in
        source.replacingOccurrences(
            of: "<Override PartName=\"/pkg/books/main.xml\" ContentType=\"\(workbookContentType)\"/>",
            with: "<wrapper><Override PartName=\"/pkg/books/main.xml\" ContentType=\"\(workbookContentType)\"/></wrapper>"
        )
    }
    guard case .invalidContentTypes(_, .invalidElementStructure, _) =
        inspectError(try makeArchive(contentTypesEntries)) else {
        Issue.record("Expected nested content-type override rejection")
        return
    }

    let relationshipEntries = try replacingEntryText(
        fixtureEntries(),
        path: "pkg/books/_rels/main.xml.rels"
    ) { source in
        source.replacingOccurrences(
            of: "<Relationship Id=\"sheetA\"",
            with: "<wrapper><Relationship Id=\"sheetA\""
        ).replacingOccurrences(
            of: "Target=\"../sheets/a.xml\"/>",
            with: "Target=\"../sheets/a.xml\"/></wrapper>"
        )
    }
    guard case .invalidRelationships(_, .invalidElementStructure, _, _) =
        inspectError(try makeArchive(relationshipEntries)) else {
        Issue.record("Expected nested relationship rejection")
        return
    }

    let nestedSheet = try makeArchive(fixtureEntries(
        workbookPrefixXML:
            "<wrapper><sheet name=\"Spoof\" sheetId=\"9\" r:id=\"sheetA\"/></wrapper>"
    ))
    guard case .invalidWorkbook(_, .invalidSheetStructure, _, _) = inspectError(nestedSheet) else {
        Issue.record("Expected sheet outside workbook/sheets rejection")
        return
    }

    let foreignNamespaceSheet = try makeArchive(fixtureEntries(
        workbookPrefixXML:
            "<x:sheet xmlns:x=\"urn:extension\" name=\"Ignored\" x:id=\"ignored\"/>"
    ))
    let inspection = try CanonicalXLSXWorkbookInspector.inspect(
        data: foreignNamespaceSheet,
        limits: xlsxLimits
    )
    #expect(inspection.worksheets.map(\.name) == ["Unit A", "Unit B", "Events"])
}

@Test
func missingMandatoryPartsAndMissingReferencedRelationshipFailClosed() throws {
    let withoutContentTypes = try makeArchive(
        fixtureEntries().filter { $0.path != "[Content_Types].xml" }
    )
    #expect(inspectError(withoutContentTypes) ==
        .missingRequiredPart(path: "[Content_Types].xml"))

    let withoutRootRels = try makeArchive(
        fixtureEntries().filter { $0.path != "_rels/.rels" }
    )
    guard case .invalidContentTypes(_, .overrideTargetsMissingPart, _) =
        inspectError(withoutRootRels) else {
        // `[Content_Types].xml` uses a Default for .rels, so no override points at this missing part.
        guard inspectError(withoutRootRels) == .missingRequiredPart(path: "_rels/.rels") else {
            Issue.record("Expected missing package relationship part")
            return
        }
        return
    }

    let unknownIDEntries = try replacingEntryText(
        fixtureEntries(),
        path: "pkg/books/main.xml"
    ) { source in
        source.replacingOccurrences(of: "r:id=\"sheetA\"", with: "r:id=\"unknown\"")
    }
    guard case .invalidRelationships(_, .missingWorksheetRelationship, let id, _) =
        inspectError(try makeArchive(unknownIDEntries)) else {
        Issue.record("Expected missing referenced worksheet relationship")
        return
    }
    #expect(id == "unknown")
}
