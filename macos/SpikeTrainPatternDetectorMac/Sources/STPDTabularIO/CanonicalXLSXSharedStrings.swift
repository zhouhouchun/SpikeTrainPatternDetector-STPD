import Foundation

enum CanonicalXLSXStringParsingLimitKind: String, Hashable, Sendable {
    case sharedStringItems
    case decodedStringUTF8Bytes
    case totalDecodedSharedStringUTF8Bytes
}

struct CanonicalXLSXStringParsingLimits: Hashable, Sendable {
    let maximumSharedStringItemCount: Int
    let maximumDecodedStringUTF8ByteCount: Int
    let maximumTotalDecodedSharedStringUTF8ByteCount: Int

    static let supportedDatasetEnvelope = CanonicalXLSXStringParsingLimits(
        maximumSharedStringItemCount:
            CanonicalTabularWorkloadLimits.supportedDatasetEnvelope
                .maximumMaterializedDataCellCount,
        maximumDecodedStringUTF8ByteCount:
            CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount,
        maximumTotalDecodedSharedStringUTF8ByteCount:
            CanonicalTabularWorkloadLimits.supportedDatasetEnvelope
                .maximumTotalMaterializedTextUTF8ByteCount
    )
}

struct CanonicalXLSXSharedStringTable: Hashable, Sendable {
    let values: [String]
    let declaredCount: UInt32?
    let declaredUniqueCount: UInt32?

    func value(at zeroBasedIndex: UInt32) throws -> String {
        let index = Int(zeroBasedIndex)
        guard values.indices.contains(index) else {
            throw CanonicalXLSXSharedStringResolutionError.indexOutOfBounds(
                index: zeroBasedIndex,
                count: values.count
            )
        }
        return values[index]
    }
}

enum CanonicalXLSXSharedStringResolutionError: Error, Hashable, Sendable {
    case indexOutOfBounds(index: UInt32, count: Int)
}

enum CanonicalXLSXSharedStringIssue: String, Hashable, Sendable {
    case unexpectedRoot
    case invalidElementStructure
    case unexpectedText
    case invalidCDATA
    case unsupportedExtensionList
    case pairedCountsRequired
    case invalidUnsignedInteger
    case countLessThanUniqueCount
    case declaredUniqueCountMismatch
    case itemLimitExceeded
    case totalDecodedLimitExceeded
    case incompleteStringItem
}

enum CanonicalXLSXStringItemIssue: String, Hashable, Sendable {
    case invalidElementStructure
    case mixedPlainAndRich
    case missingBaseText
    case missingRunText
    case invalidXMLSpace
    case invalidUnsignedInteger
    case invalidPhoneticRun
    case invalidPhoneticProperties
    case decodedItemLimitExceeded
    case unexpectedText
    case invalidCDATA
}

enum CanonicalXLSXStringItemContext: Hashable, Sendable {
    case sharedString(index: Int)
    case inlineString(cellReference: String)
}

enum CanonicalXLSXSharedStringParsingError: Error, Hashable, Sendable {
    case invalidLimit(kind: CanonicalXLSXStringParsingLimitKind, actual: Int)
    case limitExceedsSupportedEnvelope(
        kind: CanonicalXLSXStringParsingLimitKind,
        maximumSupported: Int,
        actual: Int
    )
    case invalidSharedStrings(
        part: String,
        issue: CanonicalXLSXSharedStringIssue,
        value: String?
    )
    case invalidStringItem(
        part: String,
        context: CanonicalXLSXStringItemContext,
        issue: CanonicalXLSXStringItemIssue,
        value: String?
    )
    case invalidStringText(
        part: String,
        context: CanonicalXLSXStringItemContext,
        textNodeIndex: Int,
        error: CanonicalXLSXStringDecodingError
    )
}

enum CanonicalXLSXSharedStringTableParser {
    static func parse(
        data: Data,
        part: String,
        xmlLimits: CanonicalXLSXInspectionLimits,
        stringLimits: CanonicalXLSXStringParsingLimits = .supportedDatasetEnvelope
    ) throws -> CanonicalXLSXSharedStringTable {
        try validate(stringLimits)
        let (maximumRawTextNodeUTF8ByteCount, overflow) = 7.multipliedReportingOverflow(
            by: stringLimits.maximumDecodedStringUTF8ByteCount
        )
        guard !overflow else {
            throw CanonicalXLSXSharedStringParsingError.invalidLimit(
                kind: .decodedStringUTF8Bytes,
                actual: stringLimits.maximumDecodedStringUTF8ByteCount
            )
        }
        let delegate = SharedStringsXMLDelegate(
            part: part,
            limits: xmlLimits,
            stringLimits: stringLimits,
            maximumRawTextNodeUTF8ByteCount: maximumRawTextNodeUTF8ByteCount
        )
        try parseXML(data: data, delegate: delegate)
        return try delegate.makeTable()
    }

    private static func validate(_ limits: CanonicalXLSXStringParsingLimits) throws {
        let supported = CanonicalXLSXStringParsingLimits.supportedDatasetEnvelope
        let values: [(CanonicalXLSXStringParsingLimitKind, Int, Int)] = [
            (
                .sharedStringItems,
                limits.maximumSharedStringItemCount,
                supported.maximumSharedStringItemCount
            ),
            (
                .decodedStringUTF8Bytes,
                limits.maximumDecodedStringUTF8ByteCount,
                supported.maximumDecodedStringUTF8ByteCount
            ),
            (
                .totalDecodedSharedStringUTF8Bytes,
                limits.maximumTotalDecodedSharedStringUTF8ByteCount,
                supported.maximumTotalDecodedSharedStringUTF8ByteCount
            ),
        ]
        for (kind, actual, maximumSupported) in values {
            guard actual > 0 else {
                throw CanonicalXLSXSharedStringParsingError.invalidLimit(
                    kind: kind,
                    actual: actual
                )
            }
            guard actual <= maximumSupported else {
                throw CanonicalXLSXSharedStringParsingError.limitExceedsSupportedEnvelope(
                    kind: kind,
                    maximumSupported: maximumSupported,
                    actual: actual
                )
            }
        }
    }
}

private let sharedStringSpreadsheetNamespace =
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
private let sharedStringMarkupCompatibilityNamespace =
    "http://schemas.openxmlformats.org/markup-compatibility/2006"

private final class SharedStringsXMLDelegate: BoundedXMLDelegate {
    private let stringLimits: CanonicalXLSXStringParsingLimits
    private let maximumRawTextNodeUTF8ByteCount: Int
    private var sawRoot = false
    private var declaredCount: UInt32?
    private var declaredUniqueCount: UInt32?
    private var values: [String] = []
    private var totalDecodedUTF8ByteCount = 0
    private var activeItem: CanonicalXLSXStringItemAssembler?

    init(
        part: String,
        limits: CanonicalXLSXInspectionLimits,
        stringLimits: CanonicalXLSXStringParsingLimits,
        maximumRawTextNodeUTF8ByteCount: Int
    ) {
        self.stringLimits = stringLimits
        self.maximumRawTextNodeUTF8ByteCount = maximumRawTextNodeUTF8ByteCount
        super.init(part: part, limits: limits)
    }

    override var maximumElementCharacterDataUTF8ByteCount: Int {
        activeItem?.isInsideTextElement == true
            ? maximumRawTextNodeUTF8ByteCount
            : limits.maximumXMLTextUTF8ByteCount
    }

    override func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        if depth == 1 {
            guard elementName == "sst",
                  namespaceURI == sharedStringSpreadsheetNamespace else {
                throw sharedError(.unexpectedRoot, namespaceURI)
            }
            try parseRootAttributes(attributes)
            sawRoot = true
            return
        }
        guard sawRoot else { return }

        if let activeItem {
            try activeItem.handleStart(
                elementName: elementName,
                namespaceURI: namespaceURI,
                qualifiedName: qualifiedName,
                attributes: attributes
            )
            return
        }

        guard depth == 2, namespaceURI == sharedStringSpreadsheetNamespace else {
            throw sharedError(.invalidElementStructure, qualifiedName ?? elementName)
        }
        if elementName == "extLst" {
            throw sharedError(.unsupportedExtensionList, elementName)
        }
        guard elementName == "si", attributes.isEmpty else {
            let value = attributes.keys.sorted().first ?? elementName
            throw sharedError(.invalidElementStructure, value)
        }
        guard values.count < stringLimits.maximumSharedStringItemCount else {
            throw sharedError(
                .itemLimitExceeded,
                String(stringLimits.maximumSharedStringItemCount)
            )
        }
        self.activeItem = CanonicalXLSXStringItemAssembler(
            part: part,
            context: .sharedString(index: values.count),
            maximumRawTextNodeUTF8ByteCount: maximumRawTextNodeUTF8ByteCount,
            maximumDecodedStringUTF8ByteCount:
                stringLimits.maximumDecodedStringUTF8ByteCount
        )
    }

    override func handleEnd(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?
    ) throws {
        guard let activeItem else { return }
        if depth == 2 {
            guard elementName == "si", namespaceURI == sharedStringSpreadsheetNamespace else {
                throw sharedError(.incompleteStringItem, qualifiedName ?? elementName)
            }
            let value = try activeItem.finish()
            let (proposed, overflow) = totalDecodedUTF8ByteCount.addingReportingOverflow(
                value.utf8.count
            )
            guard !overflow,
                  proposed <= stringLimits.maximumTotalDecodedSharedStringUTF8ByteCount else {
                throw sharedError(
                    .totalDecodedLimitExceeded,
                    "\(stringLimits.maximumTotalDecodedSharedStringUTF8ByteCount):\(overflow ? Int.max : proposed)"
                )
            }
            values.append(value)
            totalDecodedUTF8ByteCount = proposed
            self.activeItem = nil
            return
        }
        try activeItem.handleEnd(
            elementName: elementName,
            namespaceURI: namespaceURI,
            qualifiedName: qualifiedName
        )
    }

    override func handleCharacters(_ string: String) throws {
        if let activeItem {
            try activeItem.handleCharacters(string)
            return
        }
        guard string.unicodeScalars.allSatisfy(isSharedStringXMLWhitespace) else {
            throw sharedError(.unexpectedText)
        }
    }

    override func handleCDATA(_ data: Data) throws {
        guard let activeItem else {
            throw sharedError(.invalidCDATA)
        }
        try activeItem.handleCDATA(data)
    }

    override func validateDocumentComplete() throws {
        guard sawRoot else { throw sharedError(.unexpectedRoot) }
        guard activeItem == nil else { throw sharedError(.incompleteStringItem) }
        if let declaredUniqueCount {
            guard UInt64(declaredUniqueCount) == UInt64(values.count) else {
                throw sharedError(
                    .declaredUniqueCountMismatch,
                    "\(declaredUniqueCount):\(values.count)"
                )
            }
        }
    }

    func makeTable() throws -> CanonicalXLSXSharedStringTable {
        guard activeItem == nil else { throw sharedError(.incompleteStringItem) }
        return CanonicalXLSXSharedStringTable(
            values: values,
            declaredCount: declaredCount,
            declaredUniqueCount: declaredUniqueCount
        )
    }

    private func parseRootAttributes(_ attributes: [String: String]) throws {
        var rawCount: String?
        var rawUniqueCount: String?
        var sawIgnorable = false

        for key in attributes.keys.sorted() {
            switch key {
            case "count": rawCount = attributes[key]
            case "uniqueCount": rawUniqueCount = attributes[key]
            default:
                guard let separator = key.firstIndex(of: ":"),
                      key[key.index(after: separator)...] == "Ignorable",
                      namespaceURI(forPrefix: String(key[..<separator]))
                        == sharedStringMarkupCompatibilityNamespace,
                      !sawIgnorable,
                      let value = attributes[key] else {
                    throw sharedError(.invalidElementStructure, key)
                }
                sawIgnorable = true
                let prefixes = value.split(whereSeparator: {
                    $0 == " " || $0 == "\t" || $0 == "\n" || $0 == "\r"
                })
                guard !prefixes.isEmpty,
                      prefixes.allSatisfy({ namespaceURI(forPrefix: String($0)) != nil }) else {
                    throw sharedError(.invalidElementStructure, value)
                }
            }
        }

        guard (rawCount == nil) == (rawUniqueCount == nil) else {
            throw sharedError(.pairedCountsRequired)
        }
        guard let rawCount, let rawUniqueCount else { return }
        let count = try parseUInt32(rawCount)
        let uniqueCount = try parseUInt32(rawUniqueCount)
        guard count >= uniqueCount else {
            throw sharedError(
                .countLessThanUniqueCount,
                "\(count):\(uniqueCount)"
            )
        }
        guard UInt64(uniqueCount) <= UInt64(stringLimits.maximumSharedStringItemCount) else {
            throw sharedError(.itemLimitExceeded, String(uniqueCount))
        }
        declaredCount = count
        declaredUniqueCount = uniqueCount
    }

    private func parseUInt32(_ raw: String) throws -> UInt32 {
        guard !raw.isEmpty,
              raw.utf8.allSatisfy({ (48...57).contains($0) }),
              let value = UInt32(raw) else {
            throw sharedError(.invalidUnsignedInteger, raw)
        }
        return value
    }

    private func sharedError(
        _ issue: CanonicalXLSXSharedStringIssue,
        _ value: String? = nil
    ) -> CanonicalXLSXSharedStringParsingError {
        .invalidSharedStrings(part: part, issue: issue, value: value)
    }
}

/// Flattens the scientifically relevant base text from one SpreadsheetML string item.
///
/// The accepted product subset is intentionally narrower than the full `CT_Rst` schema: an item
/// must contain either one direct `t` or one or more rich-text `r` elements, never both. Phonetic
/// runs and formatting are bounded and minimally syntax-checked as presentation metadata, but are
/// never appended to the returned value. Each `t` is ST_Xstring-decoded independently before base
/// runs are concatenated. Phonetic base-index bounds are intentionally not interpreted until this
/// product defines the OOXML Unicode indexing unit; phonetic content is not retained or displayed.
final class CanonicalXLSXStringItemAssembler {
    private enum BaseRepresentation {
        case plain
        case rich
    }

    private enum ItemPhase {
        case base
        case phoneticRuns
        case phoneticProperties
    }

    private enum TextRole {
        case base
        case phonetic
    }

    private enum Node {
        case item
        case run
        case runProperties
        case runProperty(String)
        case text(TextRole)
        case phoneticRun
        case phoneticProperties
    }

    private let part: String
    private let context: CanonicalXLSXStringItemContext
    private let maximumRawTextNodeUTF8ByteCount: Int
    private let maximumDecodedStringUTF8ByteCount: Int
    private var stack: [Node] = [.item]
    private var representation: BaseRepresentation?
    private var itemPhase: ItemPhase = .base
    private var value = String()
    private var decodedValueUTF8ByteCount = 0
    private var currentRawText = String()
    private var currentTextNodeIndex = 0
    private var nextTextNodeIndex = 0
    private var runSawProperties = false
    private var runSawText = false
    private var phoneticRunSawText = false

    init(
        part: String,
        context: CanonicalXLSXStringItemContext,
        maximumRawTextNodeUTF8ByteCount: Int,
        maximumDecodedStringUTF8ByteCount: Int
    ) {
        self.part = part
        self.context = context
        self.maximumRawTextNodeUTF8ByteCount = maximumRawTextNodeUTF8ByteCount
        self.maximumDecodedStringUTF8ByteCount = maximumDecodedStringUTF8ByteCount
    }

    var isInsideTextElement: Bool {
        if case .text = stack.last { return true }
        return false
    }

    func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        guard namespaceURI == sharedStringSpreadsheetNamespace else {
            throw itemError(.invalidElementStructure, qualifiedName ?? elementName)
        }
        guard let current = stack.last else {
            throw itemError(.invalidElementStructure, elementName)
        }

        switch current {
        case .item:
            try startItemChild(elementName, attributes: attributes)
        case .run:
            try startRunChild(elementName, attributes: attributes)
        case .runProperties:
            try startRunProperty(elementName, attributes: attributes)
        case .phoneticRun:
            try startPhoneticRunChild(elementName, attributes: attributes)
        case .runProperty, .text, .phoneticProperties:
            throw itemError(.invalidElementStructure, elementName)
        }
    }

    func handleEnd(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?
    ) throws {
        guard namespaceURI == sharedStringSpreadsheetNamespace,
              let current = stack.last else {
            throw itemError(.invalidElementStructure, qualifiedName ?? elementName)
        }

        switch current {
        case .item:
            throw itemError(.invalidElementStructure, elementName)
        case .run:
            guard elementName == "r" else {
                throw itemError(.invalidElementStructure, elementName)
            }
            guard runSawText else { throw itemError(.missingRunText) }
            stack.removeLast()
            runSawProperties = false
            runSawText = false
        case .runProperties:
            guard elementName == "rPr" else {
                throw itemError(.invalidElementStructure, elementName)
            }
            stack.removeLast()
        case .runProperty(let expected):
            guard elementName == expected else {
                throw itemError(.invalidElementStructure, elementName)
            }
            stack.removeLast()
        case .text(let role):
            guard elementName == "t" else {
                throw itemError(.invalidElementStructure, elementName)
            }
            try finishText(role: role)
            stack.removeLast()
        case .phoneticRun:
            guard elementName == "rPh" else {
                throw itemError(.invalidElementStructure, elementName)
            }
            guard phoneticRunSawText else { throw itemError(.missingRunText) }
            stack.removeLast()
            phoneticRunSawText = false
        case .phoneticProperties:
            guard elementName == "phoneticPr" else {
                throw itemError(.invalidElementStructure, elementName)
            }
            stack.removeLast()
        }
    }

    func handleCharacters(_ string: String) throws {
        if isInsideTextElement {
            currentRawText.append(string)
            return
        }
        if let current = stack.last {
            switch current {
            case .runProperty, .phoneticProperties:
                throw itemError(.unexpectedText)
            case .item, .run, .runProperties, .text, .phoneticRun:
                break
            }
        }
        guard string.unicodeScalars.allSatisfy(isSharedStringXMLWhitespace) else {
            throw itemError(.unexpectedText)
        }
    }

    func handleCDATA(_ data: Data) throws {
        guard isInsideTextElement else { throw itemError(.invalidCDATA) }
        guard let string = String(data: data, encoding: .utf8) else {
            throw itemError(.invalidCDATA)
        }
        currentRawText.append(string)
    }

    func finish() throws -> String {
        guard stack.count == 1 else { throw itemError(.invalidElementStructure) }
        guard representation != nil else { throw itemError(.missingBaseText) }
        return value
    }

    private func startItemChild(
        _ elementName: String,
        attributes: [String: String]
    ) throws {
        switch elementName {
        case "t":
            guard itemPhase == .base else {
                throw itemError(.invalidElementStructure, elementName)
            }
            guard representation != .rich else {
                throw itemError(.mixedPlainAndRich)
            }
            guard representation == nil else {
                throw itemError(.invalidElementStructure, elementName)
            }
            representation = .plain
            try startText(role: .base, attributes: attributes)

        case "r":
            guard itemPhase == .base else {
                throw itemError(.invalidElementStructure, elementName)
            }
            guard representation != .plain else {
                throw itemError(.mixedPlainAndRich)
            }
            guard attributes.isEmpty else {
                throw itemError(
                    .invalidElementStructure,
                    attributes.keys.sorted().first
                )
            }
            representation = .rich
            runSawProperties = false
            runSawText = false
            stack.append(.run)

        case "rPh":
            guard itemPhase != .phoneticProperties else {
                throw itemError(.invalidElementStructure, elementName)
            }
            itemPhase = .phoneticRuns
            try validatePhoneticRunAttributes(attributes)
            phoneticRunSawText = false
            stack.append(.phoneticRun)

        case "phoneticPr":
            guard itemPhase != .phoneticProperties else {
                throw itemError(.invalidElementStructure, elementName)
            }
            itemPhase = .phoneticProperties
            try validatePhoneticProperties(attributes)
            stack.append(.phoneticProperties)

        default:
            throw itemError(.invalidElementStructure, elementName)
        }
    }

    private func startRunChild(
        _ elementName: String,
        attributes: [String: String]
    ) throws {
        switch elementName {
        case "rPr":
            guard !runSawProperties, !runSawText, attributes.isEmpty else {
                throw itemError(
                    .invalidElementStructure,
                    attributes.keys.sorted().first ?? elementName
                )
            }
            runSawProperties = true
            stack.append(.runProperties)
        case "t":
            guard !runSawText else {
                throw itemError(.invalidElementStructure, elementName)
            }
            runSawText = true
            try startText(role: .base, attributes: attributes)
        default:
            throw itemError(.invalidElementStructure, elementName)
        }
    }

    private func startRunProperty(
        _ elementName: String,
        attributes: [String: String]
    ) throws {
        let requiredValueProperties: Set<String> = [
            "rFont", "charset", "family", "sz", "vertAlign", "scheme",
        ]
        let optionalValueProperties: Set<String> = [
            "b", "i", "strike", "outline", "shadow", "condense", "extend", "u",
        ]
        let allowedColorAttributes: Set<String> = [
            "auto", "indexed", "rgb", "theme", "tint",
        ]

        if requiredValueProperties.contains(elementName) {
            guard attributes.keys.allSatisfy({ $0 == "val" }),
                  attributes["val"] != nil else {
                throw itemError(
                    .invalidElementStructure,
                    attributes.keys.sorted().first ?? elementName
                )
            }
        } else if optionalValueProperties.contains(elementName) {
            guard attributes.keys.allSatisfy({ $0 == "val" }) else {
                throw itemError(
                    .invalidElementStructure,
                    attributes.keys.sorted().first
                )
            }
        } else if elementName == "color" {
            guard attributes.keys.allSatisfy({ allowedColorAttributes.contains($0) }) else {
                throw itemError(
                    .invalidElementStructure,
                    attributes.keys.sorted().first
                )
            }
        } else {
            throw itemError(.invalidElementStructure, elementName)
        }
        stack.append(.runProperty(elementName))
    }

    private func startPhoneticRunChild(
        _ elementName: String,
        attributes: [String: String]
    ) throws {
        guard elementName == "t", !phoneticRunSawText else {
            throw itemError(.invalidElementStructure, elementName)
        }
        phoneticRunSawText = true
        try startText(role: .phonetic, attributes: attributes)
    }

    private func startText(
        role: TextRole,
        attributes: [String: String]
    ) throws {
        if let unexpected = attributes.keys.sorted().first(where: { $0 != "xml:space" }) {
            throw itemError(.invalidElementStructure, unexpected)
        }
        if let space = attributes["xml:space"], space != "default", space != "preserve" {
            throw itemError(.invalidXMLSpace, space)
        }
        currentRawText.removeAll(keepingCapacity: true)
        currentTextNodeIndex = nextTextNodeIndex
        nextTextNodeIndex += 1
        stack.append(.text(role))
    }

    private func finishText(role: TextRole) throws {
        let decoded: String
        do {
            decoded = try CanonicalXLSXStringDecoder.decodeTextNode(
                currentRawText,
                maximumRawUTF8ByteCount: maximumRawTextNodeUTF8ByteCount,
                maximumDecodedUTF8ByteCount: maximumDecodedStringUTF8ByteCount
            )
        } catch let error as CanonicalXLSXStringDecodingError {
            throw CanonicalXLSXSharedStringParsingError.invalidStringText(
                part: part,
                context: context,
                textNodeIndex: currentTextNodeIndex,
                error: error
            )
        }
        currentRawText.removeAll(keepingCapacity: true)
        guard role == .base else { return }

        let (proposed, overflow) = decodedValueUTF8ByteCount.addingReportingOverflow(
            decoded.utf8.count
        )
        guard !overflow, proposed <= maximumDecodedStringUTF8ByteCount else {
            throw itemError(
                .decodedItemLimitExceeded,
                "\(maximumDecodedStringUTF8ByteCount):\(overflow ? Int.max : proposed)"
            )
        }
        value.append(decoded)
        decodedValueUTF8ByteCount = proposed
    }

    private func validatePhoneticRunAttributes(_ attributes: [String: String]) throws {
        let allowed: Set<String> = ["sb", "eb"]
        if let unexpected = attributes.keys.sorted().first(where: { !allowed.contains($0) }) {
            throw itemError(.invalidPhoneticRun, unexpected)
        }
        guard let rawStart = attributes["sb"], let rawEnd = attributes["eb"] else {
            throw itemError(.invalidPhoneticRun)
        }
        let start = try parseUInt32(rawStart, issue: .invalidPhoneticRun)
        let end = try parseUInt32(rawEnd, issue: .invalidPhoneticRun)
        guard start < end else {
            throw itemError(.invalidPhoneticRun, "\(start):\(end)")
        }
    }

    private func validatePhoneticProperties(_ attributes: [String: String]) throws {
        let allowed: Set<String> = ["fontId", "type", "alignment"]
        if let unexpected = attributes.keys.sorted().first(where: { !allowed.contains($0) }) {
            throw itemError(.invalidPhoneticProperties, unexpected)
        }
        guard let rawFontID = attributes["fontId"] else {
            throw itemError(.invalidPhoneticProperties)
        }
        _ = try parseUInt32(rawFontID, issue: .invalidPhoneticProperties)
        if let type = attributes["type"] {
            let allowedTypes: Set<String> = [
                "halfwidthKatakana", "fullwidthKatakana", "Hiragana", "noConversion",
            ]
            guard allowedTypes.contains(type) else {
                throw itemError(.invalidPhoneticProperties, type)
            }
        }
        if let alignment = attributes["alignment"] {
            let allowedAlignments: Set<String> = [
                "noControl", "left", "center", "distributed",
            ]
            guard allowedAlignments.contains(alignment) else {
                throw itemError(.invalidPhoneticProperties, alignment)
            }
        }
    }

    private func parseUInt32(
        _ raw: String,
        issue: CanonicalXLSXStringItemIssue
    ) throws -> UInt32 {
        guard !raw.isEmpty,
              raw.utf8.allSatisfy({ (48...57).contains($0) }),
              let value = UInt32(raw) else {
            throw itemError(issue, raw)
        }
        return value
    }

    private func itemError(
        _ issue: CanonicalXLSXStringItemIssue,
        _ value: String? = nil
    ) -> CanonicalXLSXSharedStringParsingError {
        .invalidStringItem(
            part: part,
            context: context,
            issue: issue,
            value: value
        )
    }
}

private func isSharedStringXMLWhitespace(_ scalar: Unicode.Scalar) -> Bool {
    scalar.value == 0x09 || scalar.value == 0x0A
        || scalar.value == 0x0D || scalar.value == 0x20
}
