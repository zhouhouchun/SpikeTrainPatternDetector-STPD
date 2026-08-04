import CryptoKit
import Foundation
import ZIPFoundation

public enum CanonicalXLSXWorksheetVisibility: String, Hashable, Sendable {
    case visible
    case hidden
    case veryHidden
}

/// A worksheet discovered in one exact workbook snapshot.
///
/// There is deliberately no public initializer. A descriptor is only a catalog entry; it is not
/// permission to read a different `Data` value that happens to contain the same worksheet name.
public struct CanonicalXLSXWorksheetDescriptor: Hashable, Sendable {
    public let name: String
    public let sheetID: UInt32
    public let relationshipID: String
    public let normalizedPartPath: String
    public let visibility: CanonicalXLSXWorksheetVisibility

    internal let inspectionBinding: UUID
}

/// Immutable inspection of one exact XLSX byte snapshot. Inspection never chooses a worksheet.
public struct CanonicalXLSXWorkbookInspection: Sendable {
    public let sourceSHA256: String
    public let sourceByteCount: Int
    public let limits: CanonicalXLSXInspectionLimits
    public let worksheets: [CanonicalXLSXWorksheetDescriptor]

    internal let sourceData: Data
    internal let inspectionBinding: UUID
}

public enum CanonicalXLSXInspectionLimitKind: String, Hashable, Sendable {
    case sourceBytes
    case zipEntries
    case archivePathUTF8Bytes
    case partUncompressedBytes
    case totalUncompressedBytes
    case compressionRatio
    case worksheets
    case contentTypeDeclarations
    case relationshipsPerPart
    case xmlDepth
    case xmlAttributesPerElement
    case xmlElementsPerPart
    case xmlTextUTF8Bytes
}

/// Positive, caller-selectable limits within one frozen supported envelope.
public struct CanonicalXLSXInspectionLimits: Hashable, Sendable {
    public let maximumSourceByteCount: Int
    public let maximumZIPEntryCount: Int
    public let maximumArchivePathUTF8ByteCount: Int
    public let maximumPartUncompressedByteCount: Int
    public let maximumTotalUncompressedByteCount: Int
    public let maximumCompressionRatio: Int
    public let maximumWorksheetCount: Int
    public let maximumContentTypeDeclarationCount: Int
    public let maximumRelationshipCountPerPart: Int
    public let maximumXMLDepth: Int
    public let maximumXMLAttributeCountPerElement: Int
    public let maximumXMLElementCountPerPart: Int
    public let maximumXMLTextUTF8ByteCount: Int

    public init(
        maximumSourceByteCount: Int,
        maximumZIPEntryCount: Int,
        maximumArchivePathUTF8ByteCount: Int,
        maximumPartUncompressedByteCount: Int,
        maximumTotalUncompressedByteCount: Int,
        maximumCompressionRatio: Int,
        maximumWorksheetCount: Int,
        maximumContentTypeDeclarationCount: Int,
        maximumRelationshipCountPerPart: Int,
        maximumXMLDepth: Int,
        maximumXMLAttributeCountPerElement: Int,
        maximumXMLElementCountPerPart: Int,
        maximumXMLTextUTF8ByteCount: Int
    ) {
        self.maximumSourceByteCount = maximumSourceByteCount
        self.maximumZIPEntryCount = maximumZIPEntryCount
        self.maximumArchivePathUTF8ByteCount = maximumArchivePathUTF8ByteCount
        self.maximumPartUncompressedByteCount = maximumPartUncompressedByteCount
        self.maximumTotalUncompressedByteCount = maximumTotalUncompressedByteCount
        self.maximumCompressionRatio = maximumCompressionRatio
        self.maximumWorksheetCount = maximumWorksheetCount
        self.maximumContentTypeDeclarationCount = maximumContentTypeDeclarationCount
        self.maximumRelationshipCountPerPart = maximumRelationshipCountPerPart
        self.maximumXMLDepth = maximumXMLDepth
        self.maximumXMLAttributeCountPerElement = maximumXMLAttributeCountPerElement
        self.maximumXMLElementCountPerPart = maximumXMLElementCountPerPart
        self.maximumXMLTextUTF8ByteCount = maximumXMLTextUTF8ByteCount
    }

    public static let supportedDatasetEnvelope = CanonicalXLSXInspectionLimits(
        maximumSourceByteCount: 5 * 1_024 * 1_024,
        maximumZIPEntryCount: 256,
        maximumArchivePathUTF8ByteCount: 1_024,
        maximumPartUncompressedByteCount: 64 * 1_024 * 1_024,
        maximumTotalUncompressedByteCount: 128 * 1_024 * 1_024,
        maximumCompressionRatio: 200,
        maximumWorksheetCount: 64,
        maximumContentTypeDeclarationCount: 4_096,
        maximumRelationshipCountPerPart: 4_096,
        maximumXMLDepth: 64,
        maximumXMLAttributeCountPerElement: 64,
        maximumXMLElementCountPerPart: 4_000_000,
        maximumXMLTextUTF8ByteCount: 65_794
    )
}

public enum CanonicalXLSXArchiveProfileIssue: String, Hashable, Sendable {
    case missingEndOfCentralDirectory
    case ambiguousEndOfCentralDirectory
    case multiDisk
    case zip64Unsupported
    case encryptedEntryUnsupported
    case dataDescriptorUnsupported
    case unsupportedGeneralPurposeFlags
    case compressionMethodUnsupported
    case malformedCentralDirectory
    case iteratorCountMismatch
}

public enum CanonicalXLSXArchivePathIssue: String, Hashable, Sendable {
    case empty
    case absolute
    case backslash
    case controlCharacter
    case emptySegment
    case dotSegment
    case parentSegment
}

public enum CanonicalXLSXXMLSecurityIssue: String, Hashable, Sendable {
    case documentTypeDeclaration
    case entityDeclaration
    case externalEntity
}

public enum CanonicalXLSXContentTypesIssue: String, Hashable, Sendable {
    case unexpectedRoot
    case invalidElementStructure
    case missingRequiredAttribute
    case invalidPartName
    case duplicateDefaultExtension
    case duplicateOverridePart
    case overrideTargetsMissingPart
    case missingPartContentType
    case workbookContentTypeMismatch
    case macroEnabledWorkbookUnsupported
    case worksheetContentTypeMismatch
    case relationshipsContentTypeMismatch
}

public enum CanonicalXLSXRelationshipIssue: String, Hashable, Sendable {
    case unexpectedRoot
    case invalidElementStructure
    case missingRequiredAttribute
    case duplicateID
    case duplicateTarget
    case externalTargetUnsupported
    case invalidTarget
    case missingTargetPart
    case missingOfficeDocument
    case multipleOfficeDocuments
    case missingWorksheetRelationship
    case relationshipTypeMismatch
    case macroSheetRelationshipUnsupported
    case duplicateWorksheetPart
    case unreferencedWorksheetRelationship
}

public enum CanonicalXLSXWorkbookIssue: String, Hashable, Sendable {
    case unexpectedRoot
    case invalidSheetStructure
    case missingSheetAttribute
    case invalidSheetID
    case duplicateSheetID
    case invalidSheetState
    case worksheetLimitExceeded
    case duplicateSheetName
    case canonicalSheetNameCollision
    case noWorksheets
}

public enum CanonicalXLSXWorkbookInspectionError: Error, Hashable, Sendable {
    case invalidLimit(kind: CanonicalXLSXInspectionLimitKind, actual: Int)
    case limitExceedsSupportedEnvelope(
        kind: CanonicalXLSXInspectionLimitKind,
        maximumSupported: Int,
        actual: Int
    )
    case sourceByteLimitExceeded(maximum: Int, actual: Int)
    case invalidArchive
    case unsupportedArchiveProfile(issue: CanonicalXLSXArchiveProfileIssue, detail: UInt64?)
    case zipEntryLimitExceeded(maximum: Int, actual: Int)
    case archivePathByteLimitExceeded(path: String, maximum: Int, actual: Int)
    case unsafeArchivePath(path: String, issue: CanonicalXLSXArchivePathIssue)
    case duplicateArchivePath(path: String)
    case canonicalArchivePathCollision(first: String, second: String)
    case symbolicLinkEntry(path: String)
    case partDeclaredByteLimitExceeded(path: String, maximum: Int, actual: UInt64)
    case totalDeclaredByteLimitExceeded(maximum: Int, actual: UInt64)
    case compressionRatioExceeded(
        path: String,
        maximum: Int,
        compressed: UInt64,
        uncompressed: UInt64
    )
    case missingRequiredPart(path: String)
    case archiveExtractionFailed(path: String)
    case partActualByteLimitExceeded(path: String, maximum: Int, actual: UInt64)
    case totalActualByteLimitExceeded(maximum: Int, actual: UInt64)
    case declaredActualByteCountMismatch(path: String, declared: UInt64, actual: UInt64)
    case checksumMismatch(path: String, expected: UInt32, actual: UInt32)
    case xmlSecurityViolation(part: String, issue: CanonicalXLSXXMLSecurityIssue)
    case xmlLimitExceeded(
        part: String,
        kind: CanonicalXLSXInspectionLimitKind,
        maximum: Int,
        actual: Int
    )
    case malformedXML(part: String, line: Int, column: Int)
    case unsupportedConformance(part: String, namespaceOrType: String)
    case invalidContentTypes(
        part: String,
        issue: CanonicalXLSXContentTypesIssue,
        value: String?
    )
    case invalidRelationships(
        part: String,
        issue: CanonicalXLSXRelationshipIssue,
        relationshipID: String?,
        target: String?
    )
    case invalidWorkbook(
        part: String,
        issue: CanonicalXLSXWorkbookIssue,
        sheetName: String?,
        relationshipID: String?
    )
}

/// Bounded, relationship-aware inspection of an ordinary Transitional XLSX workbook.
///
/// The Phase-0 ZIP profile fails closed on data-descriptor entries (general-purpose bit 3).
/// Descriptor-based local headers are not part of this supported subset.
public enum CanonicalXLSXWorkbookInspector {
    public static func inspect(
        data: Data,
        limits: CanonicalXLSXInspectionLimits
    ) throws -> CanonicalXLSXWorkbookInspection {
        try validate(limits: limits)
        guard data.count <= limits.maximumSourceByteCount else {
            throw CanonicalXLSXWorkbookInspectionError.sourceByteLimitExceeded(
                maximum: limits.maximumSourceByteCount,
                actual: data.count
            )
        }

        let preflight = try ZIPCentralDirectoryPreflight.inspect(data: data, limits: limits)
        let archive: Archive
        do {
            archive = try Archive(data: data, accessMode: .read)
        } catch {
            throw CanonicalXLSXWorkbookInspectionError.invalidArchive
        }

        var index: [String: IndexedArchiveEntry] = [:]
        var rawPaths = Set<String>()
        var canonicalPaths: [String: String] = [:]
        var totalDeclared: UInt64 = 0
        var iteratedCount = 0

        for entry in archive {
            iteratedCount += 1
            guard iteratedCount <= limits.maximumZIPEntryCount else {
                throw CanonicalXLSXWorkbookInspectionError.zipEntryLimitExceeded(
                    maximum: limits.maximumZIPEntryCount,
                    actual: iteratedCount
                )
            }

            if entry.type == .symlink {
                throw CanonicalXLSXWorkbookInspectionError.symbolicLinkEntry(path: entry.path)
            }
            let hasDirectorySuffix = entry.path.hasSuffix("/")
            let isDirectory = hasDirectorySuffix || entry.type == .directory
            let path = try normalizeArchiveEntryPath(
                entry.path,
                hasDirectorySuffix: hasDirectorySuffix,
                limits: limits
            )
            guard rawPaths.insert(entry.path).inserted else {
                throw CanonicalXLSXWorkbookInspectionError.duplicateArchivePath(path: entry.path)
            }
            let collisionKey = canonicalCollisionKey(path)
            if let first = canonicalPaths[collisionKey] {
                throw CanonicalXLSXWorkbookInspectionError.canonicalArchivePathCollision(
                    first: first,
                    second: entry.path
                )
            }
            canonicalPaths[collisionKey] = entry.path

            let declared = entry.uncompressedSize
            guard declared <= UInt64(limits.maximumPartUncompressedByteCount) else {
                throw CanonicalXLSXWorkbookInspectionError.partDeclaredByteLimitExceeded(
                    path: entry.path,
                    maximum: limits.maximumPartUncompressedByteCount,
                    actual: declared
                )
            }
            let (proposedTotal, overflow) = totalDeclared.addingReportingOverflow(declared)
            guard !overflow, proposedTotal <= UInt64(limits.maximumTotalUncompressedByteCount) else {
                throw CanonicalXLSXWorkbookInspectionError.totalDeclaredByteLimitExceeded(
                    maximum: limits.maximumTotalUncompressedByteCount,
                    actual: overflow ? UInt64.max : proposedTotal
                )
            }
            totalDeclared = proposedTotal

            if declared > 0 {
                let compressed = entry.compressedSize
                let (allowed, multiplicationOverflow) = compressed.multipliedReportingOverflow(
                    by: UInt64(limits.maximumCompressionRatio)
                )
                if compressed == 0 || (!multiplicationOverflow && declared > allowed) {
                    throw CanonicalXLSXWorkbookInspectionError.compressionRatioExceeded(
                        path: entry.path,
                        maximum: limits.maximumCompressionRatio,
                        compressed: compressed,
                        uncompressed: declared
                    )
                }
            }

            if !isDirectory {
                index[path] = IndexedArchiveEntry(entry: entry, originalPath: entry.path)
            }
        }

        guard iteratedCount == preflight.entryCount else {
            throw CanonicalXLSXWorkbookInspectionError.unsupportedArchiveProfile(
                issue: .iteratorCountMismatch,
                detail: UInt64(iteratedCount)
            )
        }

        var extractor = BoundedArchiveExtractor(
            archive: archive,
            index: index,
            limits: limits
        )

        let contentTypesPath = "[Content_Types].xml"
        let contentTypesData = try extractor.extractRequired(path: contentTypesPath)
        let contentTypesDelegate = ContentTypesXMLDelegate(part: contentTypesPath, limits: limits)
        try parseXML(
            data: contentTypesData,
            part: contentTypesPath,
            limits: limits,
            delegate: contentTypesDelegate
        )
        let contentTypes = contentTypesDelegate.catalog
        for overridePath in contentTypes.overrides.keys where index[overridePath] == nil {
            throw contentTypesError(
                contentTypesPath,
                .overrideTargetsMissingPart,
                overridePath
            )
        }
        if let macroDeclaration = contentTypes.firstMacroEnabledDeclaration() {
            throw contentTypesError(
                contentTypesPath,
                .macroEnabledWorkbookUnsupported,
                macroDeclaration
            )
        }

        let rootRelationshipsPath = "_rels/.rels"
        try requireContentType(
            for: rootRelationshipsPath,
            expected: OOXML.relationshipsContentType,
            mismatch: .relationshipsContentTypeMismatch,
            catalog: contentTypes,
            part: contentTypesPath
        )
        let rootRelationshipsData = try extractor.extractRequired(path: rootRelationshipsPath)
        let rootRelationshipsDelegate = RelationshipsXMLDelegate(
            part: rootRelationshipsPath,
            sourcePart: nil,
            limits: limits
        )
        try parseXML(
            data: rootRelationshipsData,
            part: rootRelationshipsPath,
            limits: limits,
            delegate: rootRelationshipsDelegate
        )
        let rootRelationships = rootRelationshipsDelegate.relationships
        try validateRelationshipTargetsExist(
            rootRelationships,
            part: rootRelationshipsPath,
            index: index
        )

        let officeDocuments = rootRelationships.filter { $0.type == OOXML.officeDocumentRelationship }
        guard !officeDocuments.isEmpty else {
            throw relationshipError(rootRelationshipsPath, .missingOfficeDocument)
        }
        guard officeDocuments.count == 1 else {
            throw relationshipError(rootRelationshipsPath, .multipleOfficeDocuments)
        }
        let workbookPath = officeDocuments[0].targetPart

        guard let workbookContentType = contentTypes.contentType(for: workbookPath) else {
            throw contentTypesError(contentTypesPath, .missingPartContentType, workbookPath)
        }
        if OOXML.macroWorkbookContentTypes.contains(workbookContentType) {
            throw contentTypesError(contentTypesPath, .macroEnabledWorkbookUnsupported, workbookPath)
        }
        guard workbookContentType == OOXML.workbookContentType else {
            throw contentTypesError(contentTypesPath, .workbookContentTypeMismatch, workbookPath)
        }

        let workbookRelationshipsPath = relationshipPartPath(for: workbookPath)
        try requireContentType(
            for: workbookRelationshipsPath,
            expected: OOXML.relationshipsContentType,
            mismatch: .relationshipsContentTypeMismatch,
            catalog: contentTypes,
            part: contentTypesPath
        )
        let workbookData = try extractor.extractRequired(path: workbookPath)
        let workbookRelationshipsData = try extractor.extractRequired(path: workbookRelationshipsPath)

        let workbookDelegate = WorkbookXMLDelegate(part: workbookPath, limits: limits)
        try parseXML(
            data: workbookData,
            part: workbookPath,
            limits: limits,
            delegate: workbookDelegate
        )
        let workbookSheets = workbookDelegate.sheets

        let workbookRelationshipsDelegate = RelationshipsXMLDelegate(
            part: workbookRelationshipsPath,
            sourcePart: workbookPath,
            limits: limits
        )
        try parseXML(
            data: workbookRelationshipsData,
            part: workbookRelationshipsPath,
            limits: limits,
            delegate: workbookRelationshipsDelegate
        )
        let workbookRelationships = workbookRelationshipsDelegate.relationships
        try validateRelationshipTargetsExist(
            workbookRelationships,
            part: workbookRelationshipsPath,
            index: index
        )

        let relationshipsByID = Dictionary(uniqueKeysWithValues: workbookRelationships.map { ($0.id, $0) })
        let binding = UUID()
        var descriptors: [CanonicalXLSXWorksheetDescriptor] = []
        var usedWorksheetParts = Set<String>()
        var usedWorksheetRelationshipIDs = Set<String>()

        for sheet in workbookSheets {
            guard let relationship = relationshipsByID[sheet.relationshipID] else {
                throw relationshipError(
                    workbookRelationshipsPath,
                    .missingWorksheetRelationship,
                    id: sheet.relationshipID
                )
            }
            guard relationship.type == OOXML.worksheetRelationship else {
                throw relationshipError(
                    workbookRelationshipsPath,
                    .relationshipTypeMismatch,
                    id: sheet.relationshipID,
                    target: relationship.targetPart
                )
            }
            guard usedWorksheetParts.insert(relationship.targetPart).inserted else {
                throw relationshipError(
                    workbookRelationshipsPath,
                    .duplicateWorksheetPart,
                    id: sheet.relationshipID,
                    target: relationship.targetPart
                )
            }
            let worksheetContentType = contentTypes.contentType(for: relationship.targetPart)
            guard worksheetContentType == OOXML.worksheetContentType else {
                throw contentTypesError(
                    contentTypesPath,
                    worksheetContentType == nil ? .missingPartContentType : .worksheetContentTypeMismatch,
                    relationship.targetPart
                )
            }
            usedWorksheetRelationshipIDs.insert(sheet.relationshipID)
            descriptors.append(
                CanonicalXLSXWorksheetDescriptor(
                    name: sheet.name,
                    sheetID: sheet.sheetID,
                    relationshipID: sheet.relationshipID,
                    normalizedPartPath: relationship.targetPart,
                    visibility: sheet.visibility,
                    inspectionBinding: binding
                )
            )
        }

        if let unreferenced = workbookRelationships.first(where: {
            $0.type == OOXML.worksheetRelationship && !usedWorksheetRelationshipIDs.contains($0.id)
        }) {
            throw relationshipError(
                workbookRelationshipsPath,
                .unreferencedWorksheetRelationship,
                id: unreferenced.id,
                target: unreferenced.targetPart
            )
        }

        let digest = SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
        return CanonicalXLSXWorkbookInspection(
            sourceSHA256: digest,
            sourceByteCount: data.count,
            limits: limits,
            worksheets: descriptors,
            sourceData: data,
            inspectionBinding: binding
        )
    }
}

private enum OOXML {
    static let contentTypesNamespace =
        "http://schemas.openxmlformats.org/package/2006/content-types"
    static let packageRelationshipsNamespace =
        "http://schemas.openxmlformats.org/package/2006/relationships"
    static let spreadsheetNamespace =
        "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
    static let documentRelationshipsNamespace =
        "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    static let officeDocumentRelationship =
        documentRelationshipsNamespace + "/officeDocument"
    static let worksheetRelationship = documentRelationshipsNamespace + "/worksheet"
    static let macroSheetRelationshipTypes: Set<String> = [
        "http://schemas.microsoft.com/office/2006/relationships/xlMacrosheet",
        "http://schemas.microsoft.com/office/2006/relationships/xlIntlMacrosheet",
    ]
    static let relationshipsContentType =
        "application/vnd.openxmlformats-package.relationships+xml"
    static let workbookContentType =
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"
    static let worksheetContentType =
        "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"
    static let macroWorkbookContentTypes: Set<String> = [
        "application/vnd.ms-excel.sheet.macroEnabled.main+xml",
        "application/vnd.ms-excel.template.macroEnabled.main+xml",
        "application/vnd.ms-excel.addin.macroEnabled.main+xml",
    ]
    static let macroSheetContentTypes: Set<String> = [
        "application/vnd.ms-excel.macrosheet+xml",
        "application/vnd.ms-excel.intlmacrosheet+xml",
    ]

    static func isStrict(_ value: String) -> Bool {
        value.contains("purl.oclc.org/ooxml")
    }

    static func isMacroEnabledContentType(_ value: String) -> Bool {
        let folded = value.lowercased()
        return folded.contains("macroenabled")
            || folded.contains("vbaproject")
            || macroSheetContentTypes.contains(folded)
    }

    static func isMacroSheetRelationshipType(_ value: String) -> Bool {
        macroSheetRelationshipTypes.contains(value)
    }
}

private struct IndexedArchiveEntry {
    let entry: Entry
    let originalPath: String
}

private struct ZIPCentralDirectoryPreflight {
    let entryCount: Int

    static func inspect(
        data: Data,
        limits: CanonicalXLSXInspectionLimits
    ) throws -> ZIPCentralDirectoryPreflight {
        let bytes = [UInt8](data)
        let minimumEOCDSize = 22
        guard bytes.count >= minimumEOCDSize else {
            throw profileError(.missingEndOfCentralDirectory)
        }
        let firstCandidate = max(0, bytes.count - minimumEOCDSize - 65_535)
        let lastCandidate = bytes.count - minimumEOCDSize
        var validEOCDOffsets: [Int] = []
        if firstCandidate <= lastCandidate {
            for offset in stride(from: lastCandidate, through: firstCandidate, by: -1) {
                guard littleEndianUInt32(bytes, offset) == 0x0605_4b50 else { continue }
                let commentLength = Int(littleEndianUInt16(bytes, offset + 20))
                if offset + minimumEOCDSize + commentLength == bytes.count {
                    validEOCDOffsets.append(offset)
                }
            }
        }
        guard !validEOCDOffsets.isEmpty else {
            throw profileError(.missingEndOfCentralDirectory)
        }
        guard validEOCDOffsets.count == 1, let eocdOffset = validEOCDOffsets.first else {
            throw profileError(.ambiguousEndOfCentralDirectory)
        }

        let diskNumber = littleEndianUInt16(bytes, eocdOffset + 4)
        let centralDirectoryDisk = littleEndianUInt16(bytes, eocdOffset + 6)
        let entriesOnDisk = littleEndianUInt16(bytes, eocdOffset + 8)
        let totalEntries = littleEndianUInt16(bytes, eocdOffset + 10)
        guard diskNumber == 0, centralDirectoryDisk == 0, entriesOnDisk == totalEntries else {
            throw profileError(.multiDisk)
        }
        let centralDirectorySize = littleEndianUInt32(bytes, eocdOffset + 12)
        let centralDirectoryOffset = littleEndianUInt32(bytes, eocdOffset + 16)
        guard totalEntries != UInt16.max,
              centralDirectorySize != UInt32.max,
              centralDirectoryOffset != UInt32.max else {
            throw profileError(.zip64Unsupported)
        }
        let entryCount = Int(totalEntries)
        guard entryCount <= limits.maximumZIPEntryCount else {
            throw CanonicalXLSXWorkbookInspectionError.zipEntryLimitExceeded(
                maximum: limits.maximumZIPEntryCount,
                actual: entryCount
            )
        }

        let centralStart = Int(centralDirectoryOffset)
        let centralSize = Int(centralDirectorySize)
        let (centralEnd, rangeOverflow) = centralStart.addingReportingOverflow(centralSize)
        if eocdOffset >= 20,
           littleEndianUInt32(bytes, eocdOffset - 20) == 0x0706_4b50 {
            throw profileError(.zip64Unsupported)
        }
        guard !rangeOverflow, centralStart >= 0, centralEnd == eocdOffset else {
            throw profileError(.malformedCentralDirectory)
        }

        var cursor = centralStart
        var localRanges: [(start: Int, end: Int)] = []
        for _ in 0..<entryCount {
            guard cursor >= 0, cursor + 46 <= centralEnd,
                  littleEndianUInt32(bytes, cursor) == 0x0201_4b50 else {
                throw profileError(.malformedCentralDirectory)
            }
            let centralVersionNeeded = littleEndianUInt16(bytes, cursor + 6)
            guard centralVersionNeeded < 45 else {
                throw profileError(.zip64Unsupported, UInt64(centralVersionNeeded))
            }
            let flags = littleEndianUInt16(bytes, cursor + 8)
            let encryptionFlags: UInt16 = 0x0001 | 0x0040 | 0x2000
            if flags & encryptionFlags != 0 {
                throw profileError(.encryptedEntryUnsupported)
            }
            if flags & 0x0008 != 0 {
                throw profileError(.dataDescriptorUnsupported)
            }
            let compressionMethod = littleEndianUInt16(bytes, cursor + 10)
            guard compressionMethod == 0 || compressionMethod == 8 else {
                throw profileError(.compressionMethodUnsupported, UInt64(compressionMethod))
            }
            let universallyAllowedFlags: UInt16 = 0x0800
            let deflateOptionFlags: UInt16 = compressionMethod == 8 ? 0x0006 : 0
            let allowedFlags = universallyAllowedFlags | deflateOptionFlags
            guard flags & ~allowedFlags == 0 else {
                throw profileError(.unsupportedGeneralPurposeFlags, UInt64(flags))
            }
            let compressedSize = littleEndianUInt32(bytes, cursor + 20)
            let uncompressedSize = littleEndianUInt32(bytes, cursor + 24)
            let localOffset = littleEndianUInt32(bytes, cursor + 42)
            guard compressedSize != UInt32.max,
                  uncompressedSize != UInt32.max,
                  localOffset != UInt32.max else {
                throw profileError(.zip64Unsupported)
            }
            guard littleEndianUInt16(bytes, cursor + 34) == 0 else {
                throw profileError(.multiDisk)
            }
            let fileNameLength = Int(littleEndianUInt16(bytes, cursor + 28))
            let centralExtraLength = Int(littleEndianUInt16(bytes, cursor + 30))
            let variableLength = fileNameLength
                + centralExtraLength
                + Int(littleEndianUInt16(bytes, cursor + 32))
            let (nextCursor, overflow) = cursor.addingReportingOverflow(46 + variableLength)
            guard !overflow, nextCursor <= centralEnd else {
                throw profileError(.malformedCentralDirectory)
            }

            let localStart = Int(localOffset)
            guard localStart >= 0,
                  localStart + 30 <= centralStart,
                  littleEndianUInt32(bytes, localStart) == 0x0403_4b50 else {
                throw profileError(.malformedCentralDirectory)
            }
            let localVersionNeeded = littleEndianUInt16(bytes, localStart + 4)
            guard localVersionNeeded < 45 else {
                throw profileError(.zip64Unsupported, UInt64(localVersionNeeded))
            }
            guard localVersionNeeded == centralVersionNeeded else {
                throw profileError(.malformedCentralDirectory)
            }
            let localFlags = littleEndianUInt16(bytes, localStart + 6)
            if localFlags & encryptionFlags != 0 {
                throw profileError(.encryptedEntryUnsupported)
            }
            if localFlags & 0x0008 != 0 {
                throw profileError(.dataDescriptorUnsupported)
            }
            let localCompressionMethod = littleEndianUInt16(bytes, localStart + 8)
            guard localCompressionMethod == 0 || localCompressionMethod == 8 else {
                throw profileError(
                    .compressionMethodUnsupported,
                    UInt64(localCompressionMethod)
                )
            }
            guard localFlags == flags, localCompressionMethod == compressionMethod else {
                throw profileError(.malformedCentralDirectory)
            }
            guard localFlags & ~allowedFlags == 0 else {
                throw profileError(.unsupportedGeneralPurposeFlags, UInt64(localFlags))
            }
            let localFileNameLength = Int(littleEndianUInt16(bytes, localStart + 26))
            let localExtraLength = Int(littleEndianUInt16(bytes, localStart + 28))
            let localNameStart = localStart + 30
            let centralNameStart = cursor + 46
            guard localFileNameLength == fileNameLength,
                  localNameStart + localFileNameLength <= centralStart,
                  centralNameStart + fileNameLength <= centralEnd,
                  bytes[localNameStart..<(localNameStart + localFileNameLength)]
                    .elementsEqual(bytes[centralNameStart..<(centralNameStart + fileNameLength)]) else {
                throw profileError(.malformedCentralDirectory)
            }
            let centralExtraStart = centralNameStart + fileNameLength
            if try extraFieldContainsZIP64(
                bytes,
                start: centralExtraStart,
                length: centralExtraLength
            ) {
                throw profileError(.zip64Unsupported)
            }
            let localExtraStart = localNameStart + localFileNameLength
            if try extraFieldContainsZIP64(
                bytes,
                start: localExtraStart,
                length: localExtraLength
            ) {
                throw profileError(.zip64Unsupported)
            }
            let centralCRC = littleEndianUInt32(bytes, cursor + 16)
            let localCRC = littleEndianUInt32(bytes, localStart + 14)
            let localCompressedSize = littleEndianUInt32(bytes, localStart + 18)
            let localUncompressedSize = littleEndianUInt32(bytes, localStart + 22)
            guard localCRC == centralCRC,
                  localCompressedSize == compressedSize,
                  localUncompressedSize == uncompressedSize else {
                throw profileError(.malformedCentralDirectory)
            }
            let (payloadStart, headerOverflow) = localNameStart
                .addingReportingOverflow(localFileNameLength + localExtraLength)
            let (payloadEnd, payloadOverflow) = payloadStart
                .addingReportingOverflow(Int(compressedSize))
            guard !headerOverflow, !payloadOverflow,
                  payloadStart <= centralStart,
                  payloadEnd <= centralStart else {
                throw profileError(.malformedCentralDirectory)
            }
            localRanges.append((start: localStart, end: payloadEnd))
            cursor = nextCursor
        }
        guard cursor == centralEnd else {
            throw profileError(.malformedCentralDirectory)
        }
        let sortedLocalRanges = localRanges.sorted { lhs, rhs in lhs.start < rhs.start }
        for index in sortedLocalRanges.indices.dropFirst() {
            guard sortedLocalRanges[index - 1].end <= sortedLocalRanges[index].start else {
                throw profileError(.malformedCentralDirectory)
            }
        }
        return ZIPCentralDirectoryPreflight(entryCount: entryCount)
    }
}

private struct BoundedArchiveExtractor {
    let archive: Archive
    let index: [String: IndexedArchiveEntry]
    let limits: CanonicalXLSXInspectionLimits
    var totalActualExtracted: UInt64 = 0

    mutating func extractRequired(path: String) throws -> Data {
        guard let indexed = index[path] else {
            throw CanonicalXLSXWorkbookInspectionError.missingRequiredPart(path: path)
        }
        var result = Data()
        var actual: UInt64 = 0
        let checksum: CRC32
        do {
            checksum = try archive.extract(
                indexed.entry,
                bufferSize: 64 * 1_024,
                skipCRC32: false
            ) { chunk in
                let chunkCount = UInt64(chunk.count)
                let (proposedActual, partOverflow) = actual.addingReportingOverflow(chunkCount)
                guard !partOverflow,
                      proposedActual <= UInt64(limits.maximumPartUncompressedByteCount) else {
                    throw ExtractionAbort.part(overflow: partOverflow, actual: proposedActual)
                }
                let (proposedTotal, totalOverflow) = totalActualExtracted.addingReportingOverflow(chunkCount)
                guard !totalOverflow,
                      proposedTotal <= UInt64(limits.maximumTotalUncompressedByteCount) else {
                    throw ExtractionAbort.total(overflow: totalOverflow, actual: proposedTotal)
                }
                actual = proposedActual
                totalActualExtracted = proposedTotal
                result.append(chunk)
            }
        } catch let abort as ExtractionAbort {
            switch abort {
            case .part(let overflow, let count):
                throw CanonicalXLSXWorkbookInspectionError.partActualByteLimitExceeded(
                    path: path,
                    maximum: limits.maximumPartUncompressedByteCount,
                    actual: overflow ? UInt64.max : count
                )
            case .total(let overflow, let count):
                throw CanonicalXLSXWorkbookInspectionError.totalActualByteLimitExceeded(
                    maximum: limits.maximumTotalUncompressedByteCount,
                    actual: overflow ? UInt64.max : count
                )
            }
        } catch {
            throw CanonicalXLSXWorkbookInspectionError.archiveExtractionFailed(path: path)
        }
        guard actual == indexed.entry.uncompressedSize else {
            throw CanonicalXLSXWorkbookInspectionError.declaredActualByteCountMismatch(
                path: path,
                declared: indexed.entry.uncompressedSize,
                actual: actual
            )
        }
        guard checksum == indexed.entry.checksum else {
            throw CanonicalXLSXWorkbookInspectionError.checksumMismatch(
                path: path,
                expected: indexed.entry.checksum,
                actual: checksum
            )
        }
        return result
    }

    private enum ExtractionAbort: Error {
        case part(overflow: Bool, actual: UInt64)
        case total(overflow: Bool, actual: UInt64)
    }
}

private struct ContentTypeCatalog {
    var defaults: [String: String] = [:]
    var overrides: [String: String] = [:]

    func contentType(for path: String) -> String? {
        if let override = overrides[path] { return override }
        guard let finalComponent = path.split(separator: "/").last,
              let dot = finalComponent.lastIndex(of: ".") else { return nil }
        return defaults[String(finalComponent[finalComponent.index(after: dot)...]).lowercased()]
    }

    func firstMacroEnabledDeclaration() -> String? {
        if let extensionValue = defaults.keys.sorted().first(where: {
            defaults[$0].map(OOXML.isMacroEnabledContentType) == true
        }) {
            return "*.\(extensionValue)"
        }
        return overrides.keys.sorted().first(where: {
            overrides[$0].map(OOXML.isMacroEnabledContentType) == true
        })
    }
}

private final class ContentTypesXMLDelegate: BoundedXMLDelegate {
    var catalog = ContentTypeCatalog()
    private var sawRoot = false
    private var overrideCanonicalPaths: [String: String] = [:]
    private var declarationCount = 0

    override func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        if OOXML.isStrict(namespaceURI ?? "") {
            throw unsupportedConformance(part, namespaceURI ?? "")
        }
        if depth == 1 {
            guard elementName == "Types", namespaceURI == OOXML.contentTypesNamespace else {
                throw contentTypesError(part, .unexpectedRoot, namespaceURI)
            }
            sawRoot = true
            return
        }
        guard sawRoot, namespaceURI == OOXML.contentTypesNamespace else { return }
        if elementName == "Default" || elementName == "Override" {
            guard depth == 2 else {
                throw contentTypesError(part, .invalidElementStructure, elementName)
            }
            let proposedCount = declarationCount + 1
            guard proposedCount <= limits.maximumContentTypeDeclarationCount else {
                throw xmlLimitError(
                    part,
                    .contentTypeDeclarations,
                    limits.maximumContentTypeDeclarationCount,
                    proposedCount
                )
            }
            declarationCount = proposedCount
        }
        switch elementName {
        case "Default":
            guard let extensionValue = attributes["Extension"], !extensionValue.isEmpty,
                  let contentType = attributes["ContentType"], !contentType.isEmpty else {
                throw contentTypesError(part, .missingRequiredAttribute)
            }
            let key = extensionValue.lowercased()
            guard catalog.defaults[key] == nil else {
                throw contentTypesError(part, .duplicateDefaultExtension, extensionValue)
            }
            catalog.defaults[key] = contentType
        case "Override":
            guard let rawPartName = attributes["PartName"],
                  let contentType = attributes["ContentType"], !contentType.isEmpty else {
                throw contentTypesError(part, .missingRequiredAttribute)
            }
            let normalized: String
            do {
                normalized = try normalizeContentTypePartName(rawPartName)
            } catch {
                throw contentTypesError(part, .invalidPartName, rawPartName)
            }
            guard catalog.overrides[normalized] == nil else {
                throw contentTypesError(part, .duplicateOverridePart, normalized)
            }
            let collisionKey = canonicalCollisionKey(normalized)
            if overrideCanonicalPaths[collisionKey] != nil {
                throw contentTypesError(part, .duplicateOverridePart, normalized)
            }
            overrideCanonicalPaths[collisionKey] = normalized
            catalog.overrides[normalized] = contentType
        default:
            break
        }
    }

    override func validateDocumentComplete() throws {
        guard sawRoot else { throw contentTypesError(part, .unexpectedRoot) }
    }
}

private struct ParsedRelationship {
    let id: String
    let type: String
    let targetPart: String
}

private final class RelationshipsXMLDelegate: BoundedXMLDelegate {
    let sourcePart: String?
    var relationships: [ParsedRelationship] = []
    private var IDs = Set<String>()
    private var targets = Set<String>()
    private var sawRoot = false

    init(part: String, sourcePart: String?, limits: CanonicalXLSXInspectionLimits) {
        self.sourcePart = sourcePart
        super.init(part: part, limits: limits)
    }

    override func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        if OOXML.isStrict(namespaceURI ?? "") {
            throw unsupportedConformance(part, namespaceURI ?? "")
        }
        if depth == 1 {
            guard elementName == "Relationships",
                  namespaceURI == OOXML.packageRelationshipsNamespace else {
                throw relationshipError(part, .unexpectedRoot)
            }
            sawRoot = true
            return
        }
        guard sawRoot,
              namespaceURI == OOXML.packageRelationshipsNamespace,
              elementName == "Relationship" else { return }
        guard depth == 2 else {
            throw relationshipError(part, .invalidElementStructure)
        }
        guard relationships.count < limits.maximumRelationshipCountPerPart else {
            throw xmlLimitError(
                part,
                .relationshipsPerPart,
                limits.maximumRelationshipCountPerPart,
                relationships.count + 1
            )
        }
        guard let id = attributes["Id"], !id.isEmpty,
              let type = attributes["Type"], !type.isEmpty,
              let target = attributes["Target"], !target.isEmpty else {
            throw relationshipError(part, .missingRequiredAttribute)
        }
        if OOXML.isStrict(type) {
            throw unsupportedConformance(part, type)
        }
        if OOXML.isMacroSheetRelationshipType(type) {
            throw relationshipError(
                part,
                .macroSheetRelationshipUnsupported,
                id: id,
                target: target
            )
        }
        if attributes["TargetMode"] == "External" {
            throw relationshipError(part, .externalTargetUnsupported, id: id, target: target)
        }
        guard attributes["TargetMode"] == nil || attributes["TargetMode"] == "Internal" else {
            throw relationshipError(part, .invalidTarget, id: id, target: target)
        }
        guard IDs.insert(id).inserted else {
            throw relationshipError(part, .duplicateID, id: id, target: target)
        }
        let normalizedTarget: String
        do {
            normalizedTarget = try resolveRelationshipTarget(target, sourcePart: sourcePart)
        } catch {
            throw relationshipError(part, .invalidTarget, id: id, target: target)
        }
        guard targets.insert(canonicalCollisionKey(normalizedTarget)).inserted else {
            throw relationshipError(part, .duplicateTarget, id: id, target: normalizedTarget)
        }
        relationships.append(
            ParsedRelationship(id: id, type: type, targetPart: normalizedTarget)
        )
    }

    override func validateDocumentComplete() throws {
        guard sawRoot else { throw relationshipError(part, .unexpectedRoot) }
    }
}

private struct ParsedWorkbookSheet {
    let name: String
    let sheetID: UInt32
    let relationshipID: String
    let visibility: CanonicalXLSXWorksheetVisibility
}

private final class WorkbookXMLDelegate: BoundedXMLDelegate {
    var sheets: [ParsedWorkbookSheet] = []
    private var sawRoot = false
    private var exactNames = Set<String>()
    private var canonicalNames: [String: String] = [:]
    private var sheetIDs = Set<UInt32>()
    private var insideSheets = false
    private var sawSheets = false

    override func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        if OOXML.isStrict(namespaceURI ?? "") {
            throw unsupportedConformance(part, namespaceURI ?? "")
        }
        if depth == 1 {
            guard elementName == "workbook", namespaceURI == OOXML.spreadsheetNamespace else {
                throw workbookError(part, .unexpectedRoot)
            }
            sawRoot = true
            return
        }
        guard sawRoot, namespaceURI == OOXML.spreadsheetNamespace else { return }
        if elementName == "sheets" {
            guard depth == 2, !sawSheets else {
                throw workbookError(part, .invalidSheetStructure)
            }
            sawSheets = true
            insideSheets = true
            return
        }
        guard elementName == "sheet" else { return }
        guard depth == 3, insideSheets else {
            throw workbookError(part, .invalidSheetStructure)
        }
        guard sheets.count < limits.maximumWorksheetCount else {
            throw workbookError(part, .worksheetLimitExceeded)
        }
        guard let name = attributes["name"], !name.isEmpty else {
            throw workbookError(part, .missingSheetAttribute)
        }
        guard exactNames.insert(name).inserted else {
            throw workbookError(part, .duplicateSheetName, name: name)
        }
        let nameCollisionKey = canonicalCollisionKey(name.precomposedStringWithCanonicalMapping)
        if let first = canonicalNames[nameCollisionKey] {
            throw workbookError(
                part,
                .canonicalSheetNameCollision,
                name: "\(first) | \(name)"
            )
        }
        canonicalNames[nameCollisionKey] = name

        guard let rawSheetID = attributes["sheetId"],
              !rawSheetID.isEmpty,
              rawSheetID.utf8.allSatisfy({ (48...57).contains($0) }),
              let sheetID = UInt32(rawSheetID),
              sheetID > 0 else {
            throw workbookError(part, .invalidSheetID, name: name)
        }
        guard sheetIDs.insert(sheetID).inserted else {
            throw workbookError(part, .duplicateSheetID, name: name)
        }

        let relationshipID = try relationshipIDAttribute(
            attributes,
            namespaceLookup: { self.namespaceURI(forPrefix: $0) },
            part: part,
            sheetName: name
        )
        let visibility: CanonicalXLSXWorksheetVisibility
        switch attributes["state"] {
        case nil, "visible": visibility = .visible
        case "hidden": visibility = .hidden
        case "veryHidden": visibility = .veryHidden
        default:
            throw workbookError(
                part,
                .invalidSheetState,
                name: name,
                id: relationshipID
            )
        }
        sheets.append(
            ParsedWorkbookSheet(
                name: name,
                sheetID: sheetID,
                relationshipID: relationshipID,
                visibility: visibility
            )
        )
    }

    override func handleEnd(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?
    ) throws {
        if elementName == "sheets", namespaceURI == OOXML.spreadsheetNamespace, depth == 2 {
            insideSheets = false
        }
    }

    override func validateDocumentComplete() throws {
        guard sawRoot else { throw workbookError(part, .unexpectedRoot) }
        guard !sheets.isEmpty else { throw workbookError(part, .noWorksheets) }
    }
}

private class BoundedXMLDelegate: NSObject, XMLParserDelegate {
    let part: String
    let limits: CanonicalXLSXInspectionLimits
    private(set) var failure: CanonicalXLSXWorkbookInspectionError?
    private(set) var depth = 0
    private var elementCount = 0
    private var textByteCounts: [Int] = []
    private var namespaceBindings: [String: [String]] = [:]
    private var pendingNamespaceDeclarationCount = 0

    init(part: String, limits: CanonicalXLSXInspectionLimits) {
        self.part = part
        self.limits = limits
    }

    final func parser(
        _ parser: XMLParser,
        didStartMappingPrefix prefix: String,
        toURI namespaceURI: String
    ) {
        guard failure == nil else { return }
        pendingNamespaceDeclarationCount += 1
        guard pendingNamespaceDeclarationCount <= limits.maximumXMLAttributeCountPerElement else {
            failure = xmlLimitError(
                part,
                .xmlAttributesPerElement,
                limits.maximumXMLAttributeCountPerElement,
                pendingNamespaceDeclarationCount
            )
            parser.abortParsing()
            return
        }
        guard prefix.utf8.count <= limits.maximumXMLTextUTF8ByteCount,
              namespaceURI.utf8.count <= limits.maximumXMLTextUTF8ByteCount else {
            failure = xmlLimitError(
                part,
                .xmlTextUTF8Bytes,
                limits.maximumXMLTextUTF8ByteCount,
                max(prefix.utf8.count, namespaceURI.utf8.count)
            )
            parser.abortParsing()
            return
        }
        namespaceBindings[prefix, default: []].append(namespaceURI)
    }

    final func parser(_ parser: XMLParser, didEndMappingPrefix prefix: String) {
        namespaceBindings[prefix]?.removeLast()
        if namespaceBindings[prefix]?.isEmpty == true { namespaceBindings[prefix] = nil }
    }

    final func parser(
        _ parser: XMLParser,
        didStartElement elementName: String,
        namespaceURI: String?,
        qualifiedName qName: String?,
        attributes attributeDict: [String: String] = [:]
    ) {
        guard failure == nil else { return }
        do {
            let proposedDepth = depth + 1
            guard proposedDepth <= limits.maximumXMLDepth else {
                throw xmlLimitError(part, .xmlDepth, limits.maximumXMLDepth, proposedDepth)
            }
            let proposedElementCount = elementCount + 1
            guard proposedElementCount <= limits.maximumXMLElementCountPerPart else {
                throw xmlLimitError(
                    part,
                    .xmlElementsPerPart,
                    limits.maximumXMLElementCountPerPart,
                    proposedElementCount
                )
            }
            let (totalAttributeCount, attributeOverflow) = attributeDict.count
                .addingReportingOverflow(pendingNamespaceDeclarationCount)
            guard !attributeOverflow,
                  totalAttributeCount <= limits.maximumXMLAttributeCountPerElement else {
                throw xmlLimitError(
                    part,
                    .xmlAttributesPerElement,
                    limits.maximumXMLAttributeCountPerElement,
                    attributeOverflow ? Int.max : totalAttributeCount
                )
            }
            pendingNamespaceDeclarationCount = 0
            try validateXMLText(elementName)
            if let namespaceURI { try validateXMLText(namespaceURI) }
            if let qName { try validateXMLText(qName) }
            for (key, value) in attributeDict {
                try validateXMLText(key)
                try validateXMLText(value)
            }
            depth = proposedDepth
            elementCount = proposedElementCount
            textByteCounts.append(0)
            try handleStart(
                elementName: elementName,
                namespaceURI: namespaceURI,
                qualifiedName: qName,
                attributes: attributeDict
            )
        } catch let error as CanonicalXLSXWorkbookInspectionError {
            failure = error
            parser.abortParsing()
        } catch {
            failure = .malformedXML(part: part, line: parser.lineNumber, column: parser.columnNumber)
            parser.abortParsing()
        }
    }

    final func parser(
        _ parser: XMLParser,
        didEndElement elementName: String,
        namespaceURI: String?,
        qualifiedName qName: String?
    ) {
        guard failure == nil else { return }
        do {
            try handleEnd(
                elementName: elementName,
                namespaceURI: namespaceURI,
                qualifiedName: qName
            )
            _ = textByteCounts.popLast()
            depth -= 1
        } catch let error as CanonicalXLSXWorkbookInspectionError {
            failure = error
            parser.abortParsing()
        } catch {
            failure = .malformedXML(part: part, line: parser.lineNumber, column: parser.columnNumber)
            parser.abortParsing()
        }
    }

    final func parser(_ parser: XMLParser, foundCharacters string: String) {
        recordTextByteCount(string.utf8.count, parser: parser)
    }

    final func parser(_ parser: XMLParser, foundIgnorableWhitespace whitespaceString: String) {
        recordTextByteCount(whitespaceString.utf8.count, parser: parser)
    }

    final func parser(_ parser: XMLParser, foundCDATA CDATABlock: Data) {
        recordTextByteCount(CDATABlock.count, parser: parser)
    }

    final func parser(_ parser: XMLParser, foundComment comment: String) {
        failIfDetachedXMLTextExceedsLimit(comment.utf8.count, parser: parser)
    }

    final func parser(
        _ parser: XMLParser,
        foundProcessingInstructionWithTarget target: String,
        data: String?
    ) {
        let targetCount = target.utf8.count
        let dataCount = data?.utf8.count ?? 0
        let (combinedCount, overflow) = targetCount.addingReportingOverflow(dataCount)
        failIfDetachedXMLTextExceedsLimit(
            overflow ? Int.max : combinedCount,
            parser: parser
        )
    }

    private func recordTextByteCount(_ byteCount: Int, parser: XMLParser) {
        guard failure == nil, !textByteCounts.isEmpty else { return }
        let (proposed, overflow) = textByteCounts[textByteCounts.count - 1]
            .addingReportingOverflow(byteCount)
        guard !overflow, proposed <= limits.maximumXMLTextUTF8ByteCount else {
            failure = xmlLimitError(
                part,
                .xmlTextUTF8Bytes,
                limits.maximumXMLTextUTF8ByteCount,
                overflow ? Int.max : proposed
            )
            parser.abortParsing()
            return
        }
        textByteCounts[textByteCounts.count - 1] = proposed
    }

    private func failIfDetachedXMLTextExceedsLimit(_ byteCount: Int, parser: XMLParser) {
        guard failure == nil else { return }
        guard byteCount <= limits.maximumXMLTextUTF8ByteCount else {
            failure = xmlLimitError(
                part,
                .xmlTextUTF8Bytes,
                limits.maximumXMLTextUTF8ByteCount,
                byteCount
            )
            parser.abortParsing()
            return
        }
    }

    final func parser(
        _ parser: XMLParser,
        resolveExternalEntityName name: String,
        systemID: String?
    ) -> Data? {
        failure = .xmlSecurityViolation(part: part, issue: .externalEntity)
        parser.abortParsing()
        return nil
    }

    func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {}

    func handleEnd(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?
    ) throws {}

    func validateDocumentComplete() throws {}

    func namespaceURI(forPrefix prefix: String) -> String? {
        namespaceBindings[prefix]?.last
    }

    private func validateXMLText(_ text: String) throws {
        let bytes = text.utf8.count
        guard bytes <= limits.maximumXMLTextUTF8ByteCount else {
            throw xmlLimitError(
                part,
                .xmlTextUTF8Bytes,
                limits.maximumXMLTextUTF8ByteCount,
                bytes
            )
        }
    }
}

private func parseXML(
    data: Data,
    part: String,
    limits: CanonicalXLSXInspectionLimits,
    delegate: BoundedXMLDelegate
) throws {
    if containsXMLMarkup(data, needle: "<!DOCTYPE") {
        throw CanonicalXLSXWorkbookInspectionError.xmlSecurityViolation(
            part: part,
            issue: .documentTypeDeclaration
        )
    }
    if containsXMLMarkup(data, needle: "<!ENTITY") {
        throw CanonicalXLSXWorkbookInspectionError.xmlSecurityViolation(
            part: part,
            issue: .entityDeclaration
        )
    }
    let parser = XMLParser(data: data)
    parser.shouldProcessNamespaces = true
    parser.shouldReportNamespacePrefixes = true
    parser.shouldResolveExternalEntities = false
    parser.delegate = delegate
    let success = parser.parse()
    if let failure = delegate.failure { throw failure }
    guard success else {
        throw CanonicalXLSXWorkbookInspectionError.malformedXML(
            part: part,
            line: parser.lineNumber,
            column: parser.columnNumber
        )
    }
    try delegate.validateDocumentComplete()
}

private func validate(limits: CanonicalXLSXInspectionLimits) throws {
    let supported = CanonicalXLSXInspectionLimits.supportedDatasetEnvelope
    let values: [(CanonicalXLSXInspectionLimitKind, Int, Int)] = [
        (.sourceBytes, limits.maximumSourceByteCount, supported.maximumSourceByteCount),
        (.zipEntries, limits.maximumZIPEntryCount, supported.maximumZIPEntryCount),
        (
            .archivePathUTF8Bytes,
            limits.maximumArchivePathUTF8ByteCount,
            supported.maximumArchivePathUTF8ByteCount
        ),
        (
            .partUncompressedBytes,
            limits.maximumPartUncompressedByteCount,
            supported.maximumPartUncompressedByteCount
        ),
        (
            .totalUncompressedBytes,
            limits.maximumTotalUncompressedByteCount,
            supported.maximumTotalUncompressedByteCount
        ),
        (.compressionRatio, limits.maximumCompressionRatio, supported.maximumCompressionRatio),
        (.worksheets, limits.maximumWorksheetCount, supported.maximumWorksheetCount),
        (
            .contentTypeDeclarations,
            limits.maximumContentTypeDeclarationCount,
            supported.maximumContentTypeDeclarationCount
        ),
        (
            .relationshipsPerPart,
            limits.maximumRelationshipCountPerPart,
            supported.maximumRelationshipCountPerPart
        ),
        (.xmlDepth, limits.maximumXMLDepth, supported.maximumXMLDepth),
        (
            .xmlAttributesPerElement,
            limits.maximumXMLAttributeCountPerElement,
            supported.maximumXMLAttributeCountPerElement
        ),
        (
            .xmlElementsPerPart,
            limits.maximumXMLElementCountPerPart,
            supported.maximumXMLElementCountPerPart
        ),
        (
            .xmlTextUTF8Bytes,
            limits.maximumXMLTextUTF8ByteCount,
            supported.maximumXMLTextUTF8ByteCount
        ),
    ]
    for (kind, actual, maximum) in values {
        guard actual > 0 else {
            throw CanonicalXLSXWorkbookInspectionError.invalidLimit(kind: kind, actual: actual)
        }
        guard actual <= maximum else {
            throw CanonicalXLSXWorkbookInspectionError.limitExceedsSupportedEnvelope(
                kind: kind,
                maximumSupported: maximum,
                actual: actual
            )
        }
    }
}

private func normalizeArchiveEntryPath(
    _ rawPath: String,
    hasDirectorySuffix: Bool,
    limits: CanonicalXLSXInspectionLimits
) throws -> String {
    let actualBytes = rawPath.utf8.count
    guard actualBytes <= limits.maximumArchivePathUTF8ByteCount else {
        throw CanonicalXLSXWorkbookInspectionError.archivePathByteLimitExceeded(
            path: rawPath,
            maximum: limits.maximumArchivePathUTF8ByteCount,
            actual: actualBytes
        )
    }
    let candidate = hasDirectorySuffix ? String(rawPath.dropLast()) : rawPath
    guard !candidate.isEmpty else { throw unsafePath(rawPath, .empty) }
    guard !candidate.hasPrefix("/") else { throw unsafePath(rawPath, .absolute) }
    guard !candidate.contains("\\") else { throw unsafePath(rawPath, .backslash) }
    guard !candidate.unicodeScalars.contains(where: { $0.properties.generalCategory == .control }) else {
        throw unsafePath(rawPath, .controlCharacter)
    }
    let segments = candidate.split(separator: "/", omittingEmptySubsequences: false)
    for segment in segments {
        if segment.isEmpty { throw unsafePath(rawPath, .emptySegment) }
        if segment == "." { throw unsafePath(rawPath, .dotSegment) }
        if segment == ".." { throw unsafePath(rawPath, .parentSegment) }
    }
    return candidate.precomposedStringWithCanonicalMapping
}

private func normalizeContentTypePartName(_ rawPartName: String) throws -> String {
    guard rawPartName.hasPrefix("/"), !rawPartName.hasPrefix("//") else {
        throw PathNormalizationFailure.invalid
    }
    return try normalizePackagePath(String(rawPartName.dropFirst()))
}

private func resolveRelationshipTarget(_ rawTarget: String, sourcePart: String?) throws -> String {
    guard !rawTarget.isEmpty,
          !rawTarget.contains("\\"),
          !rawTarget.contains("?"),
          !rawTarget.contains("#"),
          !rawTarget.unicodeScalars.contains(where: {
              $0.properties.generalCategory == .control
          }),
          let decoded = rawTarget.removingPercentEncoding else {
        throw PathNormalizationFailure.invalid
    }
    guard !decoded.isEmpty,
          !decoded.contains("\\"),
          !decoded.contains("?"),
          !decoded.contains("#"),
          !decoded.contains(":"),
          !decoded.unicodeScalars.contains(where: {
              $0.properties.generalCategory == .control
          }),
          !decoded.hasSuffix("/") else {
        throw PathNormalizationFailure.invalid
    }
    let pathBody = decoded.hasPrefix("/") ? String(decoded.dropFirst()) : decoded
    guard !pathBody.isEmpty, !pathBody.hasPrefix("/") else {
        throw PathNormalizationFailure.invalid
    }
    let decodedSegments = pathBody.split(separator: "/", omittingEmptySubsequences: false)
    guard let finalDecodedSegment = decodedSegments.last,
          finalDecodedSegment != ".",
          finalDecodedSegment != ".." else {
        throw PathNormalizationFailure.invalid
    }
    let initialSegments: [String]
    if decoded.hasPrefix("/") {
        initialSegments = []
    } else if let sourcePart {
        initialSegments = sourcePart.split(separator: "/").dropLast().map(String.init)
    } else {
        initialSegments = []
    }
    var resolved = initialSegments
    for segment in decodedSegments {
        let value = String(segment)
        if value.isEmpty { throw PathNormalizationFailure.invalid }
        switch value {
        case ".": continue
        case "..":
            guard !resolved.isEmpty else { throw PathNormalizationFailure.invalid }
            resolved.removeLast()
        default: resolved.append(value)
        }
    }
    guard !resolved.isEmpty else { throw PathNormalizationFailure.invalid }
    return try normalizePackagePath(resolved.joined(separator: "/"))
}

private func normalizePackagePath(_ path: String) throws -> String {
    guard !path.isEmpty,
          !path.hasPrefix("/"),
          !path.contains("\\"),
          !path.contains("?"),
          !path.contains("#"),
          !path.contains(":") else {
        throw PathNormalizationFailure.invalid
    }
    guard !path.unicodeScalars.contains(where: { $0.properties.generalCategory == .control }) else {
        throw PathNormalizationFailure.invalid
    }
    for segment in path.split(separator: "/", omittingEmptySubsequences: false) {
        guard !segment.isEmpty, segment != ".", segment != ".." else {
            throw PathNormalizationFailure.invalid
        }
    }
    return path.precomposedStringWithCanonicalMapping
}

private enum PathNormalizationFailure: Error { case invalid }

private func relationshipPartPath(for sourcePart: String) -> String {
    let components = sourcePart.split(separator: "/").map(String.init)
    let fileName = components.last ?? sourcePart
    let directory = components.dropLast().joined(separator: "/")
    return directory.isEmpty
        ? "_rels/\(fileName).rels"
        : "\(directory)/_rels/\(fileName).rels"
}

private func relationshipIDAttribute(
    _ attributes: [String: String],
    namespaceLookup: (String) -> String?,
    part: String,
    sheetName: String
) throws -> String {
    let candidates = attributes.compactMap { key, value -> (String, String)? in
        guard let colon = key.firstIndex(of: ":") else { return nil }
        let prefix = String(key[..<colon])
        let localName = String(key[key.index(after: colon)...])
        return localName == "id" ? (prefix, value) : nil
    }
    let resolved = candidates.compactMap { prefix, value -> (String, String)? in
        namespaceLookup(prefix).map { ($0, value) }
    }
    if let strict = resolved.first(where: { OOXML.isStrict($0.0) }) {
        throw unsupportedConformance(part, strict.0)
    }
    let transitional = resolved.filter {
        $0.0 == OOXML.documentRelationshipsNamespace && !$0.1.isEmpty
    }
    guard transitional.count == 1, let value = transitional.first?.1 else {
        throw workbookError(part, .missingSheetAttribute, name: sheetName)
    }
    return value
}

private func validateRelationshipTargetsExist(
    _ relationships: [ParsedRelationship],
    part: String,
    index: [String: IndexedArchiveEntry]
) throws {
    for relationship in relationships where index[relationship.targetPart] == nil {
        throw relationshipError(
            part,
            .missingTargetPart,
            id: relationship.id,
            target: relationship.targetPart
        )
    }
}

private func requireContentType(
    for path: String,
    expected: String,
    mismatch: CanonicalXLSXContentTypesIssue,
    catalog: ContentTypeCatalog,
    part: String
) throws {
    guard let actual = catalog.contentType(for: path) else {
        throw contentTypesError(part, .missingPartContentType, path)
    }
    guard actual == expected else { throw contentTypesError(part, mismatch, path) }
}

private func canonicalCollisionKey(_ value: String) -> String {
    value.precomposedStringWithCanonicalMapping.folding(
        options: [.caseInsensitive],
        locale: Locale(identifier: "en_US_POSIX")
    )
}

private func containsASCIIInsensitive(_ data: Data, needle: String) -> Bool {
    let bytes = [UInt8](data)
    let target = Array(needle.utf8).map(asciiLowercased)
    guard bytes.count >= target.count else { return false }
    for start in 0...(bytes.count - target.count) {
        var matches = true
        for offset in target.indices where asciiLowercased(bytes[start + offset]) != target[offset] {
            matches = false
            break
        }
        if matches { return true }
    }
    return false
}

private func containsXMLMarkup(_ data: Data, needle: String) -> Bool {
    if containsASCIIInsensitive(data, needle: needle) { return true }
    for encoding: String.Encoding in [
        .utf16LittleEndian,
        .utf16BigEndian,
        .utf32LittleEndian,
        .utf32BigEndian,
    ] {
        if let encoded = needle.data(using: encoding), data.range(of: encoded) != nil {
            return true
        }
    }
    return false
}

private func asciiLowercased(_ byte: UInt8) -> UInt8 {
    (65...90).contains(byte) ? byte + 32 : byte
}

private func littleEndianUInt16(_ bytes: [UInt8], _ offset: Int) -> UInt16 {
    UInt16(bytes[offset]) | (UInt16(bytes[offset + 1]) << 8)
}

private func littleEndianUInt32(_ bytes: [UInt8], _ offset: Int) -> UInt32 {
    UInt32(bytes[offset])
        | (UInt32(bytes[offset + 1]) << 8)
        | (UInt32(bytes[offset + 2]) << 16)
        | (UInt32(bytes[offset + 3]) << 24)
}

private func extraFieldContainsZIP64(
    _ bytes: [UInt8],
    start: Int,
    length: Int
) throws -> Bool {
    let (end, overflow) = start.addingReportingOverflow(length)
    guard !overflow, start >= 0, end <= bytes.count else {
        throw profileError(.malformedCentralDirectory)
    }
    var cursor = start
    while cursor < end {
        guard cursor + 4 <= end else {
            throw profileError(.malformedCentralDirectory)
        }
        let identifier = littleEndianUInt16(bytes, cursor)
        let payloadLength = Int(littleEndianUInt16(bytes, cursor + 2))
        let (next, fieldOverflow) = cursor.addingReportingOverflow(4 + payloadLength)
        guard !fieldOverflow, next <= end else {
            throw profileError(.malformedCentralDirectory)
        }
        if identifier == 0x0001 { return true }
        cursor = next
    }
    return false
}

private func profileError(
    _ issue: CanonicalXLSXArchiveProfileIssue,
    _ detail: UInt64? = nil
) -> CanonicalXLSXWorkbookInspectionError {
    .unsupportedArchiveProfile(issue: issue, detail: detail)
}

private func unsafePath(
    _ path: String,
    _ issue: CanonicalXLSXArchivePathIssue
) -> CanonicalXLSXWorkbookInspectionError {
    .unsafeArchivePath(path: path, issue: issue)
}

private func xmlLimitError(
    _ part: String,
    _ kind: CanonicalXLSXInspectionLimitKind,
    _ maximum: Int,
    _ actual: Int
) -> CanonicalXLSXWorkbookInspectionError {
    .xmlLimitExceeded(part: part, kind: kind, maximum: maximum, actual: actual)
}

private func unsupportedConformance(
    _ part: String,
    _ value: String
) -> CanonicalXLSXWorkbookInspectionError {
    .unsupportedConformance(part: part, namespaceOrType: value)
}

private func contentTypesError(
    _ part: String,
    _ issue: CanonicalXLSXContentTypesIssue,
    _ value: String? = nil
) -> CanonicalXLSXWorkbookInspectionError {
    .invalidContentTypes(part: part, issue: issue, value: value)
}

private func relationshipError(
    _ part: String,
    _ issue: CanonicalXLSXRelationshipIssue,
    id: String? = nil,
    target: String? = nil
) -> CanonicalXLSXWorkbookInspectionError {
    .invalidRelationships(
        part: part,
        issue: issue,
        relationshipID: id,
        target: target
    )
}

private func workbookError(
    _ part: String,
    _ issue: CanonicalXLSXWorkbookIssue,
    name: String? = nil,
    id: String? = nil
) -> CanonicalXLSXWorkbookInspectionError {
    .invalidWorkbook(part: part, issue: issue, sheetName: name, relationshipID: id)
}
