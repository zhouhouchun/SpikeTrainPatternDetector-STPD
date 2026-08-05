import Foundation

enum CanonicalXLSXNumberFormatDisposition: String, Hashable, Sendable {
    case provenNonDate
    case dateOrTime
    case unsupported
}

struct CanonicalXLSXCellFormat: Hashable, Sendable {
    let numberFormatID: UInt16
    let disposition: CanonicalXLSXNumberFormatDisposition
}

struct CanonicalXLSXStyleCatalog: Hashable, Sendable {
    let cellFormats: [CanonicalXLSXCellFormat]
}

enum CanonicalXLSXStylesAvailability: Hashable, Sendable {
    case absent
    case present(CanonicalXLSXStyleCatalog)

    /// Proves only that the selected style is not an Excel date/time format.
    ///
    /// The worksheet reader must still require a numeric cell and parse its exact `<v>` text.
    /// Number-format display effects such as percent, fractions, or trailing-comma scaling never
    /// transform the scientific value and never enter canonical dataset identity.
    func requireTimestampSafeCellFormat(
        explicitStyleIndex: UInt32?
    ) throws -> CanonicalXLSXCellFormat {
        let selectedIndex: Int
        let format: CanonicalXLSXCellFormat

        switch self {
        case .absent:
            if let explicitStyleIndex {
                throw CanonicalXLSXStyleResolutionError.styleIndexRequiresStylesPart(
                    index: explicitStyleIndex
                )
            }
            selectedIndex = 0
            format = CanonicalXLSXCellFormat(
                numberFormatID: 0,
                disposition: .provenNonDate
            )

        case .present(let catalog):
            let requestedIndex = explicitStyleIndex ?? 0
            selectedIndex = Int(requestedIndex)
            guard catalog.cellFormats.indices.contains(selectedIndex) else {
                throw CanonicalXLSXStyleResolutionError.cellStyleIndexOutOfBounds(
                    index: requestedIndex,
                    count: catalog.cellFormats.count
                )
            }
            format = catalog.cellFormats[selectedIndex]
        }

        switch format.disposition {
        case .provenNonDate:
            return format
        case .dateOrTime:
            throw CanonicalXLSXStyleResolutionError.dateOrTimeNumberFormat(
                styleIndex: selectedIndex,
                numberFormatID: format.numberFormatID
            )
        case .unsupported:
            throw CanonicalXLSXStyleResolutionError.unprovenNumberFormat(
                styleIndex: selectedIndex,
                numberFormatID: format.numberFormatID
            )
        }
    }
}

enum CanonicalXLSXStyleResolutionError: Error, Hashable, Sendable {
    case styleIndexRequiresStylesPart(index: UInt32)
    case cellStyleIndexOutOfBounds(index: UInt32, count: Int)
    case dateOrTimeNumberFormat(styleIndex: Int, numberFormatID: UInt16)
    case unprovenNumberFormat(styleIndex: Int, numberFormatID: UInt16)
}

enum CanonicalXLSXStyleIssue: String, Hashable, Sendable {
    case unexpectedRoot
    case duplicateSection
    case invalidSectionOrder
    case invalidElementStructure
    case missingRequiredAttribute
    case invalidUnsignedInteger
    case invalidBoolean
    case declaredCountMismatch
    case formatRecordLimitExceeded
    case duplicateNumberFormatID
    case reservedNumberFormatRedefined
    case formatCode16Unsupported
    case invalidFormatCode
    case formatCodeTooLong
    case unsupportedExtensionList
    case missingCellFormats
    case styleParentReferenceOutOfRange
    case styleParentMustNotReferenceParent
    case incoherentNumberFormatApplication
    case unexpectedText
    case invalidCDATA
}

enum CanonicalXLSXStyleParsingError: Error, Hashable, Sendable {
    case invalidStyles(part: String, issue: CanonicalXLSXStyleIssue, value: String?)
}

enum CanonicalXLSXStyleCatalogParser {
    static let maximumCustomNumberFormatCount = 206
    static let maximumCombinedXFCount = 65_430
    static let maximumFormatCodeUTF16CodeUnitCount = 254

    static func parse(
        data: Data,
        part: String,
        limits: CanonicalXLSXInspectionLimits
    ) throws -> CanonicalXLSXStyleCatalog {
        let delegate = StylesXMLDelegate(part: part, limits: limits)
        try parseXML(data: data, delegate: delegate)
        return try delegate.makeCatalog()
    }
}

private let spreadsheetNamespace =
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
private let markupCompatibilityNamespace =
    "http://schemas.openxmlformats.org/markup-compatibility/2006"
private let slicerStylesNamespace =
    "http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
private let timelineStylesNamespace =
    "http://schemas.microsoft.com/office/spreadsheetml/2010/11/main"

private enum AllowedPresentationStyleExtension: Int {
    case slicerStyles
    case timelineStyles

    var uri: String {
        switch self {
        case .slicerStyles:
            return "{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}"
        case .timelineStyles:
            return "{9260A510-F301-46a8-8635-F512D64BE5F5}"
        }
    }

    var elementName: String {
        switch self {
        case .slicerStyles: return "slicerStyles"
        case .timelineStyles: return "timelineStyles"
        }
    }

    var namespace: String {
        switch self {
        case .slicerStyles: return slicerStylesNamespace
        case .timelineStyles: return timelineStylesNamespace
        }
    }

    var allowedAttribute: String {
        switch self {
        case .slicerStyles: return "defaultSlicerStyle"
        case .timelineStyles: return "defaultTimelineStyle"
        }
    }

    init?(uri: String) {
        if uri == AllowedPresentationStyleExtension.slicerStyles.uri {
            self = .slicerStyles
        } else if uri == AllowedPresentationStyleExtension.timelineStyles.uri {
            self = .timelineStyles
        } else {
            return nil
        }
    }
}

private let knownNonDateNumberFormatIDs: Set<UInt16> = {
    var result = Set<UInt16>()
    result.formUnion((0...4).map(UInt16.init))
    result.formUnion((9...13).map(UInt16.init))
    result.formUnion((37...40).map(UInt16.init))
    result.formUnion([48, 49])
    return result
}()

private let knownDateOrTimeNumberFormatIDs: Set<UInt16> = {
    var result = Set<UInt16>()
    result.formUnion((14...22).map(UInt16.init))
    result.formUnion((27...36).map(UInt16.init))
    result.formUnion((45...47).map(UInt16.init))
    result.formUnion((50...58).map(UInt16.init))
    result.formUnion((71...81).map(UInt16.init))
    return result
}()

private struct ParsedStyleXF {
    let numberFormatID: UInt16
    let parentStyleIndex: Int?
    let applyNumberFormat: Bool?
}

private enum StyleSection: String {
    case numberFormats = "numFmts"
    case cellStyleFormats = "cellStyleXfs"
    case cellFormats = "cellXfs"
}

private let topLevelStyleElementOrder: [String: Int] = [
    "numFmts": 0,
    "fonts": 1,
    "fills": 2,
    "borders": 3,
    "cellStyleXfs": 4,
    "cellXfs": 5,
    "cellStyles": 6,
    "dxfs": 7,
    "tableStyles": 8,
    "colors": 9,
    "extLst": 10,
]

private let allowedXFAttributes: Set<String> = [
    "numFmtId", "fontId", "fillId", "borderId", "xfId",
    "applyNumberFormat", "applyFont", "applyFill", "applyBorder",
    "applyAlignment", "applyProtection", "pivotButton", "quotePrefix",
]

private let booleanXFAttributes: Set<String> = [
    "applyNumberFormat", "applyFont", "applyFill", "applyBorder",
    "applyAlignment", "applyProtection", "pivotButton", "quotePrefix",
]

private let unsignedIntegerXFAttributes: Set<String> = [
    "fontId", "fillId", "borderId",
]

private let allowedAlignmentAttributes: Set<String> = [
    "horizontal", "vertical", "textRotation", "wrapText", "shrinkToFit", "indent",
    "relativeIndent", "justifyLastLine", "readingOrder",
]

private let allowedProtectionAttributes: Set<String> = ["locked", "hidden"]

private final class StylesXMLDelegate: BoundedXMLDelegate {
    private var sawRoot = false
    private var seenTopLevelElements = Set<String>()
    private var activeSection: StyleSection?
    private var declaredCounts: [StyleSection: Int] = [:]
    private var customNumberFormats: [UInt16: String] = [:]
    private var cellStyleFormats: [ParsedStyleXF] = []
    private var cellFormats: [ParsedStyleXF] = []
    private var totalXFCount = 0
    private var lastTopLevelOrder = -1
    private var insideXF = false
    private var seenXFChildren = Set<String>()
    private var lastXFChildOrder = -1
    private var insidePresentationExtensionList = false
    private var activePresentationExtension: AllowedPresentationStyleExtension?
    private var activePresentationExtensionSawPayload = false
    private var seenPresentationExtensions = Set<Int>()
    private var lastPresentationExtensionOrder = -1

    override func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        if attributes.keys.contains(where: { attributeLocalName($0) == "formatCode16" }) {
            throw styleError(.formatCode16Unsupported, elementName)
        }
        if elementName == "extLst", depth != 2 {
            throw styleError(.unsupportedExtensionList, elementName)
        }

        switch depth {
        case 1:
            guard elementName == "styleSheet", namespaceURI == spreadsheetNamespace else {
                throw styleError(.unexpectedRoot, namespaceURI)
            }
            try validateRootAttributes(attributes)
            sawRoot = true

        case 2:
            guard sawRoot,
                  namespaceURI == spreadsheetNamespace,
                  let order = topLevelStyleElementOrder[elementName] else {
                throw styleError(.invalidElementStructure, elementName)
            }
            guard seenTopLevelElements.insert(elementName).inserted else {
                throw styleError(.duplicateSection, elementName)
            }
            guard order > lastTopLevelOrder else {
                throw styleError(.invalidSectionOrder, elementName)
            }
            lastTopLevelOrder = order
            if elementName == "extLst" {
                guard attributes.isEmpty else {
                    throw styleError(
                        .unsupportedExtensionList,
                        attributes.keys.sorted().first
                    )
                }
                insidePresentationExtensionList = true
                return
            }
            guard let section = StyleSection(rawValue: elementName) else { return }
            try validateSectionAttributes(attributes, section: section)
            activeSection = section

        case 3:
            if insidePresentationExtensionList {
                guard elementName == "ext", namespaceURI == spreadsheetNamespace,
                      attributes.count == 1, let uri = attributes["uri"],
                      let extensionKind = AllowedPresentationStyleExtension(uri: uri),
                      seenPresentationExtensions.insert(extensionKind.rawValue).inserted,
                      extensionKind.rawValue > lastPresentationExtensionOrder else {
                    throw styleError(
                        .unsupportedExtensionList,
                        attributes["uri"] ?? qualifiedName ?? elementName
                    )
                }
                lastPresentationExtensionOrder = extensionKind.rawValue
                activePresentationExtension = extensionKind
                activePresentationExtensionSawPayload = false
                return
            }
            guard let activeSection else { return }
            guard namespaceURI == spreadsheetNamespace else {
                throw styleError(.invalidElementStructure, qualifiedName ?? elementName)
            }
            switch activeSection {
            case .numberFormats:
                guard elementName == "numFmt" else {
                    throw styleError(.invalidElementStructure, elementName)
                }
                try parseNumberFormat(attributes)
            case .cellStyleFormats, .cellFormats:
                guard elementName == "xf" else {
                    throw styleError(.invalidElementStructure, elementName)
                }
                try appendXF(attributes, to: activeSection)
                insideXF = true
                seenXFChildren.removeAll(keepingCapacity: true)
                lastXFChildOrder = -1
            }

        case 4:
            if let extensionKind = activePresentationExtension {
                guard !activePresentationExtensionSawPayload,
                      elementName == extensionKind.elementName,
                      namespaceURI == extensionKind.namespace,
                      attributes.keys.allSatisfy({ $0 == extensionKind.allowedAttribute }),
                      let styleName = attributes[extensionKind.allowedAttribute],
                      !styleName.isEmpty,
                      // XML Schema length facets count Unicode code points, not Swift graphemes
                      // or UTF-8 storage bytes. Freeze that metric at this compatibility edge.
                      styleName.unicodeScalars.count <= 255 else {
                    throw styleError(
                        .unsupportedExtensionList,
                        qualifiedName ?? elementName
                    )
                }
                activePresentationExtensionSawPayload = true
                return
            }
            guard let activeSection else { return }
            guard activeSection != .numberFormats,
                  insideXF,
                  namespaceURI == spreadsheetNamespace else {
                throw styleError(.invalidElementStructure, qualifiedName ?? elementName)
            }
            let childOrder: Int
            switch elementName {
            case "alignment": childOrder = 0
            case "protection": childOrder = 1
            default: throw styleError(.invalidElementStructure, elementName)
            }
            guard seenXFChildren.insert(elementName).inserted,
                  childOrder > lastXFChildOrder else {
                throw styleError(.invalidElementStructure, elementName)
            }
            lastXFChildOrder = childOrder
            let allowedAttributes = elementName == "alignment"
                ? allowedAlignmentAttributes
                : allowedProtectionAttributes
            if let unexpected = attributes.keys.sorted().first(
                where: { !allowedAttributes.contains($0) }
            ) {
                throw styleError(.invalidElementStructure, unexpected)
            }

        default:
            if insidePresentationExtensionList {
                throw styleError(
                    .unsupportedExtensionList,
                    qualifiedName ?? elementName
                )
            }
            if activeSection != nil {
                throw styleError(.invalidElementStructure, qualifiedName ?? elementName)
            }
        }
    }

    override func handleEnd(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?
    ) throws {
        if depth == 3, elementName == "xf", namespaceURI == spreadsheetNamespace {
            insideXF = false
            seenXFChildren.removeAll(keepingCapacity: true)
            lastXFChildOrder = -1
        }
        if depth == 3, elementName == "ext", namespaceURI == spreadsheetNamespace,
           insidePresentationExtensionList {
            guard activePresentationExtension != nil,
                  activePresentationExtensionSawPayload else {
                throw styleError(.unsupportedExtensionList, elementName)
            }
            activePresentationExtension = nil
            activePresentationExtensionSawPayload = false
        }
        if depth == 2, namespaceURI == spreadsheetNamespace,
           activeSection?.rawValue == elementName {
            activeSection = nil
        }
        if depth == 2, elementName == "extLst", namespaceURI == spreadsheetNamespace {
            guard insidePresentationExtensionList,
                  activePresentationExtension == nil,
                  !seenPresentationExtensions.isEmpty else {
                throw styleError(.unsupportedExtensionList, elementName)
            }
            insidePresentationExtensionList = false
        }
    }

    override func handleCharacters(_ string: String) throws {
        guard string.unicodeScalars.allSatisfy(isXMLWhitespace) else {
            throw styleError(.unexpectedText, string)
        }
    }

    override func handleCDATA(_ data: Data) throws {
        throw styleError(.invalidCDATA)
    }

    override func validateDocumentComplete() throws {
        guard sawRoot else { throw styleError(.unexpectedRoot) }
    }

    func makeCatalog() throws -> CanonicalXLSXStyleCatalog {
        try validateDeclaredCount(.numberFormats, actual: customNumberFormats.count)
        try validateDeclaredCount(.cellStyleFormats, actual: cellStyleFormats.count)
        try validateDeclaredCount(.cellFormats, actual: cellFormats.count)
        guard !cellFormats.isEmpty else { throw styleError(.missingCellFormats) }

        for (index, format) in cellFormats.enumerated() {
            guard let parentIndex = format.parentStyleIndex else { continue }
            guard cellStyleFormats.indices.contains(parentIndex) else {
                throw styleError(
                    .styleParentReferenceOutOfRange,
                    "cellXfs[\(index)].xfId=\(parentIndex)"
                )
            }
            let parent = cellStyleFormats[parentIndex]
            if format.numberFormatID != parent.numberFormatID,
               format.applyNumberFormat != true {
                throw styleError(
                    .incoherentNumberFormatApplication,
                    "cellXfs[\(index)]"
                )
            }
        }

        return CanonicalXLSXStyleCatalog(
            cellFormats: cellFormats.map { format in
                CanonicalXLSXCellFormat(
                    numberFormatID: format.numberFormatID,
                    disposition: classify(
                        numberFormatID: format.numberFormatID,
                        customNumberFormats: customNumberFormats
                    )
                )
            }
        )
    }

    private func validateRootAttributes(_ attributes: [String: String]) throws {
        var sawIgnorable = false
        for key in attributes.keys.sorted() {
            guard let separator = key.firstIndex(of: ":"),
                  key[key.index(after: separator)...] == "Ignorable",
                  namespaceURI(forPrefix: String(key[..<separator]))
                    == markupCompatibilityNamespace,
                  !sawIgnorable,
                  let value = attributes[key] else {
                throw styleError(.invalidElementStructure, key)
            }
            sawIgnorable = true
            let prefixes = value.split(whereSeparator: {
                $0 == " " || $0 == "\t" || $0 == "\n" || $0 == "\r"
            })
            guard !prefixes.isEmpty,
                  prefixes.allSatisfy({ namespaceURI(forPrefix: String($0)) != nil }) else {
                throw styleError(.invalidElementStructure, value)
            }
        }
    }

    private func validateSectionAttributes(
        _ attributes: [String: String],
        section: StyleSection
    ) throws {
        if let unexpected = attributes.keys.sorted().first(where: { $0 != "count" }) {
            throw styleError(.invalidElementStructure, unexpected)
        }
        guard let rawCount = attributes["count"] else { return }
        let maximum = section == .numberFormats
            ? CanonicalXLSXStyleCatalogParser.maximumCustomNumberFormatCount
            : CanonicalXLSXStyleCatalogParser.maximumCombinedXFCount
        declaredCounts[section] = try parseBoundedCount(rawCount, maximum: maximum)
    }

    private func parseNumberFormat(_ attributes: [String: String]) throws {
        let allowedAttributes: Set<String> = ["numFmtId", "formatCode"]
        if let unexpected = attributes.keys.sorted().first(
            where: { !allowedAttributes.contains($0) }
        ) {
            throw styleError(.invalidElementStructure, unexpected)
        }
        guard let rawID = attributes["numFmtId"],
              let rawCode = attributes["formatCode"] else {
            throw styleError(.missingRequiredAttribute, "numFmt")
        }
        let id = try parseNumberFormatID(rawID)
        if knownNonDateNumberFormatIDs.contains(id)
            || knownDateOrTimeNumberFormatIDs.contains(id) {
            throw styleError(.reservedNumberFormatRedefined, rawID)
        }
        guard customNumberFormats[id] == nil else {
            throw styleError(.duplicateNumberFormatID, rawID)
        }
        guard customNumberFormats.count
                < CanonicalXLSXStyleCatalogParser.maximumCustomNumberFormatCount else {
            throw styleError(.formatRecordLimitExceeded, "numFmt")
        }
        guard rawCode.utf16.count
                <= CanonicalXLSXStyleCatalogParser.maximumFormatCodeUTF16CodeUnitCount else {
            throw styleError(.formatCodeTooLong, rawID)
        }

        let code: String
        do {
            code = try CanonicalXLSXStringDecoder.decodeTextNode(rawCode)
        } catch {
            throw styleError(.invalidFormatCode, rawID)
        }
        guard code.utf16.count
                <= CanonicalXLSXStyleCatalogParser.maximumFormatCodeUTF16CodeUnitCount else {
            throw styleError(.formatCodeTooLong, rawID)
        }
        customNumberFormats[id] = code
    }

    private func appendXF(
        _ attributes: [String: String],
        to section: StyleSection
    ) throws {
        guard totalXFCount < CanonicalXLSXStyleCatalogParser.maximumCombinedXFCount else {
            throw styleError(.formatRecordLimitExceeded, "xf")
        }
        let parsed = try parseXF(
            attributes,
            permitsParent: section == .cellFormats
        )
        totalXFCount += 1
        if section == .cellFormats {
            cellFormats.append(parsed)
        } else {
            cellStyleFormats.append(parsed)
        }
    }

    private func parseXF(
        _ attributes: [String: String],
        permitsParent: Bool
    ) throws -> ParsedStyleXF {
        if let unexpected = attributes.keys.sorted().first(
            where: { !allowedXFAttributes.contains($0) }
        ) {
            throw styleError(.invalidElementStructure, unexpected)
        }
        guard let rawNumberFormatID = attributes["numFmtId"] else {
            throw styleError(.missingRequiredAttribute, "xf.numFmtId")
        }
        let numberFormatID = try parseNumberFormatID(rawNumberFormatID)

        for key in unsignedIntegerXFAttributes.sorted() {
            if let rawValue = attributes[key] {
                _ = try parseUInt32(rawValue)
            }
        }
        for key in booleanXFAttributes.sorted() {
            if let rawValue = attributes[key] {
                _ = try parseBoolean(rawValue)
            }
        }

        let parentStyleIndex: Int?
        if let rawParent = attributes["xfId"] {
            guard permitsParent else {
                throw styleError(.styleParentMustNotReferenceParent, rawParent)
            }
            parentStyleIndex = Int(try parseUInt32(rawParent))
        } else {
            parentStyleIndex = nil
        }

        let applyNumberFormat = try attributes["applyNumberFormat"].map(parseBoolean)
        return ParsedStyleXF(
            numberFormatID: numberFormatID,
            parentStyleIndex: parentStyleIndex,
            applyNumberFormat: applyNumberFormat
        )
    }

    private func parseBoundedCount(_ raw: String, maximum: Int) throws -> Int {
        let value = try parseUnsignedInt(raw)
        guard value <= maximum else {
            throw styleError(.formatRecordLimitExceeded, raw)
        }
        return value
    }

    private func parseUnsignedInt(_ raw: String) throws -> Int {
        guard !raw.isEmpty,
              raw.utf8.allSatisfy({ (48...57).contains($0) }),
              let value = Int(raw) else {
            throw styleError(.invalidUnsignedInteger, raw)
        }
        return value
    }

    private func parseUInt32(_ raw: String) throws -> UInt32 {
        let value = try parseUnsignedInt(raw)
        guard value <= Int(UInt32.max) else {
            throw styleError(.invalidUnsignedInteger, raw)
        }
        return UInt32(value)
    }

    private func parseNumberFormatID(_ raw: String) throws -> UInt16 {
        let value = try parseUnsignedInt(raw)
        guard value <= Int(UInt16.max) else {
            throw styleError(.invalidUnsignedInteger, raw)
        }
        return UInt16(value)
    }

    private func parseBoolean(_ raw: String) throws -> Bool {
        switch raw {
        case "1", "true": return true
        case "0", "false": return false
        default: throw styleError(.invalidBoolean, raw)
        }
    }

    private func validateDeclaredCount(_ section: StyleSection, actual: Int) throws {
        guard let declared = declaredCounts[section] else { return }
        guard declared == actual else {
            throw styleError(
                .declaredCountMismatch,
                "\(section.rawValue):\(declared):\(actual)"
            )
        }
    }

    private func styleError(
        _ issue: CanonicalXLSXStyleIssue,
        _ value: String? = nil
    ) -> CanonicalXLSXStyleParsingError {
        .invalidStyles(part: part, issue: issue, value: value)
    }
}

private func classify(
    numberFormatID: UInt16,
    customNumberFormats: [UInt16: String]
) -> CanonicalXLSXNumberFormatDisposition {
    if knownNonDateNumberFormatIDs.contains(numberFormatID) { return .provenNonDate }
    if knownDateOrTimeNumberFormatIDs.contains(numberFormatID) { return .dateOrTime }
    guard let custom = customNumberFormats[numberFormatID] else { return .unsupported }
    return classifyCustomNumberFormat(custom)
}

private func classifyCustomNumberFormat(_ code: String) -> CanonicalXLSXNumberFormatDisposition {
    let scalars = Array(code.unicodeScalars)
    var offset = 0
    var sectionCount = 1
    var fillCountInSection = 0
    var sawPlaceholder = false

    while offset < scalars.count {
        let scalar = scalars[offset]
        switch scalar.value {
        case 0x22: // Quoted literal; the first following quote ends it.
            offset += 1
            var closed = false
            while offset < scalars.count {
                guard isPermittedLiteralScalar(scalars[offset]) else { return .unsupported }
                if scalars[offset].value == 0x22 {
                    closed = true
                    offset += 1
                    break
                }
                offset += 1
            }
            if !closed { return .unsupported }
            continue

        case 0x5C, 0x5F, 0x2A: // Escape, spacing, or fill consumes one literal scalar.
            if scalar.value == 0x2A {
                fillCountInSection += 1
                if fillCountInSection > 1 { return .unsupported }
            }
            guard offset + 1 < scalars.count,
                  isPermittedLiteralScalar(scalars[offset + 1]) else {
                return .unsupported
            }
            offset += 2
            continue

        case 0x3B: // Semicolon.
            sectionCount += 1
            if sectionCount > 4 { return .unsupported }
            fillCountInSection = 0
            offset += 1
            continue

        case 0x5B: // Only a narrow, non-date color/condition subset is accepted.
            guard let close = scalars[(offset + 1)...].firstIndex(
                where: { $0.value == 0x5D }
            ) else {
                return .unsupported
            }
            let content = String(String.UnicodeScalarView(scalars[(offset + 1)..<close]))
            if isElapsedTimeBracket(content) { return .dateOrTime }
            guard isSupportedColorBracket(content) || isSupportedConditionBracket(content) else {
                return .unsupported
            }
            offset = close + 1
            continue

        default:
            break
        }

        if matchesASCIICaseInsensitive("AM/PM", in: scalars, at: offset)
            || matchesASCIICaseInsensitive("A/P", in: scalars, at: offset) {
            return .dateOrTime
        }
        if isActiveDateOrTimeLetter(scalar) { return .dateOrTime }
        if isASCIIAlphabetic(scalar) || scalar.value > 0x7F { return .unsupported }

        if [UInt32(0x30), 0x23, 0x3F, 0x40].contains(scalar.value) {
            sawPlaceholder = true
        } else {
            let allowedASCII = "123456789.,%@$-+/():!^&'~{}<>= "
            guard allowedASCII.unicodeScalars.contains(scalar) else { return .unsupported }
        }
        offset += 1
    }

    return sawPlaceholder ? .provenNonDate : .unsupported
}

private func isSupportedColorBracket(_ content: String) -> Bool {
    let lowercased = content.lowercased()
    let namedColors: Set<String> = [
        "black", "blue", "cyan", "green", "magenta", "red", "white", "yellow",
    ]
    if namedColors.contains(lowercased) { return true }
    guard lowercased.hasPrefix("color"),
          let value = Int(lowercased.dropFirst(5)),
          (1...56).contains(value) else { return false }
    return lowercased == "color\(value)"
}

private func isSupportedConditionBracket(_ content: String) -> Bool {
    let operators = ["<=", ">=", "<>", "<", ">", "="]
    guard let operation = operators.first(where: { content.hasPrefix($0) }) else {
        return false
    }
    var number = content.dropFirst(operation.count)
    if number.first == "+" || number.first == "-" { number = number.dropFirst() }
    guard !number.isEmpty else { return false }

    let pieces = number.split(separator: ".", omittingEmptySubsequences: false)
    guard pieces.count <= 2,
          pieces.contains(where: { !$0.isEmpty }),
          pieces.allSatisfy({ part in
              part.utf8.allSatisfy({ (48...57).contains($0) })
          }) else { return false }
    return true
}

private func isElapsedTimeBracket(_ content: String) -> Bool {
    guard !content.isEmpty else { return false }
    let lowercased = content.lowercased()
    return lowercased.allSatisfy({ $0 == "h" || $0 == "m" || $0 == "s" })
        && Set(lowercased).count == 1
}

private func isASCIIAlphabetic(_ scalar: Unicode.Scalar) -> Bool {
    (0x41...0x5A).contains(scalar.value) || (0x61...0x7A).contains(scalar.value)
}

private func isActiveDateOrTimeLetter(_ scalar: Unicode.Scalar) -> Bool {
    guard isASCIIAlphabetic(scalar) else { return false }
    let lower = scalar.value | 0x20
    return [UInt32(0x79), 0x6D, 0x64, 0x68, 0x73]
        .contains(lower)
}

private func matchesASCIICaseInsensitive(
    _ token: String,
    in scalars: [Unicode.Scalar],
    at offset: Int
) -> Bool {
    let tokenScalars = Array(token.unicodeScalars)
    guard offset <= scalars.count - tokenScalars.count else { return false }
    for tokenOffset in tokenScalars.indices {
        let actual = scalars[offset + tokenOffset].value
        let expected = tokenScalars[tokenOffset].value
        let foldedActual = (0x41...0x5A).contains(actual) ? actual | 0x20 : actual
        let foldedExpected = (0x41...0x5A).contains(expected) ? expected | 0x20 : expected
        guard foldedActual == foldedExpected else { return false }
    }
    return true
}

private func isPermittedLiteralScalar(_ scalar: Unicode.Scalar) -> Bool {
    !(scalar.value < 0x20 || (0x7F...0x9F).contains(scalar.value))
}

private func isXMLWhitespace(_ scalar: Unicode.Scalar) -> Bool {
    scalar.value == 0x09 || scalar.value == 0x0A
        || scalar.value == 0x0D || scalar.value == 0x20
}

private func attributeLocalName(_ qualifiedName: String) -> Substring {
    guard let separator = qualifiedName.lastIndex(of: ":") else {
        return qualifiedName[...]
    }
    return qualifiedName[qualifiedName.index(after: separator)...]
}
