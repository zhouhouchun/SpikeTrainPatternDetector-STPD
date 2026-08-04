import CryptoKit
import Foundation
import STPDCore
import STPDTabularIO
import Testing

private let supportedLimits = CanonicalCSVStagingLimits.supportedDatasetEnvelope

private func sha256Hex(_ data: Data) -> String {
    SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
}

private func limits(
    sourceBytes: Int? = nil,
    columns: Int? = nil,
    dataRows: Int? = nil,
    materializedCells: Int? = nil,
    rawFieldBytes: Int? = nil,
    decodedFieldBytes: Int? = nil,
    totalTextBytes: Int? = nil
) -> CanonicalCSVStagingLimits {
    CanonicalCSVStagingLimits(
        maximumSourceByteCount: sourceBytes ?? supportedLimits.maximumSourceByteCount,
        maximumColumnCount: columns ?? supportedLimits.maximumColumnCount,
        maximumLogicalDataRowCount: dataRows ?? supportedLimits.maximumLogicalDataRowCount,
        maximumMaterializedDataCellCount:
            materializedCells ?? supportedLimits.maximumMaterializedDataCellCount,
        maximumRawFieldUTF8ByteCount:
            rawFieldBytes ?? supportedLimits.maximumRawFieldUTF8ByteCount,
        maximumDecodedFieldUTF8ByteCount:
            decodedFieldBytes ?? supportedLimits.maximumDecodedFieldUTF8ByteCount,
        maximumTotalMaterializedTextUTF8ByteCount:
            totalTextBytes ?? supportedLimits.maximumTotalMaterializedTextUTF8ByteCount
    )
}

private func readCSV(
    _ source: String,
    headerDecision: CanonicalTabularHeaderDecision,
    limits: CanonicalCSVStagingLimits = supportedLimits
) throws -> StagedScientificImport {
    try CanonicalCSVStagingReader.read(
        data: Data(source.utf8),
        headerDecision: headerDecision,
        limits: limits
    )
}

private func readerError(
    data: Data,
    headerDecision: CanonicalTabularHeaderDecision = .headerless,
    limits: CanonicalCSVStagingLimits = supportedLimits
) -> CanonicalCSVStagingReaderError? {
    do {
        _ = try CanonicalCSVStagingReader.read(
            data: data,
            headerDecision: headerDecision,
            limits: limits
        )
        return nil
    } catch let error as CanonicalCSVStagingReaderError {
        return error
    } catch {
        Issue.record("Unexpected error type: \(error)")
        return nil
    }
}

private func readerError(
    _ source: String,
    headerDecision: CanonicalTabularHeaderDecision = .headerless,
    limits: CanonicalCSVStagingLimits = supportedLimits
) -> CanonicalCSVStagingReaderError? {
    readerError(
        data: Data(source.utf8),
        headerDecision: headerDecision,
        limits: limits
    )
}

private func sourceLocation(
    byteOffset: Int,
    line: Int,
    column: Int
) -> CanonicalCSVSourceLocation {
    CanonicalCSVSourceLocation(
        oneBasedByteOffset: byteOffset,
        oneBasedPhysicalLine: line,
        oneBasedByteColumn: column
    )
}

private func fieldLocation(
    byteOffset: Int,
    line: Int,
    column: Int,
    logicalRecord: Int,
    field: Int
) -> CanonicalCSVFieldLocation {
    CanonicalCSVFieldLocation(
        source: sourceLocation(byteOffset: byteOffset, line: line, column: column),
        oneBasedLogicalRecord: logicalRecord,
        oneBasedField: field
    )
}

@Test
func optionalUTF8BOMDoesNotChangeStagedContent() throws {
    let source = Data("unit_a,unit_b\r\n1.000000,2.000000".utf8)
    var withBOM = Data([0xEF, 0xBB, 0xBF])
    withBOM.append(source)

    let plain = try CanonicalCSVStagingReader.readWithProvenance(
        data: source,
        headerDecision: .firstRecordIsHeader,
        limits: supportedLimits
    )
    let bom = try CanonicalCSVStagingReader.readWithProvenance(
        data: withBOM,
        headerDecision: .firstRecordIsHeader,
        limits: supportedLimits
    )

    #expect(bom.stagedImport != plain.stagedImport)
    #expect(bom.stagedImport.source == plain.stagedImport.source)
    #expect(bom.stagedImport.columns == plain.stagedImport.columns)
    #expect(bom.stagedImport.dataRowCount == plain.stagedImport.dataRowCount)
    #expect(bom.stagedImport.suggestions == plain.stagedImport.suggestions)
    #expect(plain.stagedImport.source == .commaSeparatedValues)
    #expect(plain.stagedImport.columns.map(\.header) == ["unit_a", "unit_b"])
    #expect(plain.provenance.sourceData == source)
    #expect(bom.provenance.sourceData == withBOM)
    #expect(plain.provenance.sourceByteCount == source.count)
    #expect(bom.provenance.sourceByteCount == withBOM.count)
    #expect(plain.provenance.sourceSHA256 == sha256Hex(source))
    #expect(bom.provenance.sourceSHA256 == sha256Hex(withBOM))
    #expect(plain.provenance.sourceSHA256 != bom.provenance.sourceSHA256)
    #expect(
        plain.stagedImport.sourceTransactionBinding?.sourceBytesSHA256
            == plain.provenance.sourceSHA256
    )
    #expect(
        bom.stagedImport.sourceTransactionBinding?.sourceBytesSHA256
            == bom.provenance.sourceSHA256
    )
    #expect(
        plain.stagedImport.sourceTransactionBinding?.selection == .commaSeparatedValues
    )
    #expect(plain.stagedImport.sourceTransactionBinding != bom.stagedImport.sourceTransactionBinding)
    #expect(plain.provenance.appliedLimits == supportedLimits)
    #expect(plain.provenance.headerDecision == .firstRecordIsHeader)
    #expect(plain.provenance.logicalRecords[0].fields[0].location.source.oneBasedByteOffset == 1)
    #expect(bom.provenance.logicalRecords[0].fields[0].location.source.oneBasedByteOffset == 4)

    let stagedOnly = try CanonicalCSVStagingReader.read(
        data: source,
        headerDecision: .firstRecordIsHeader,
        limits: supportedLimits
    )
    #expect(stagedOnly == plain.stagedImport)

    let repeated = try CanonicalCSVStagingReader.readWithProvenance(
        data: source,
        headerDecision: .firstRecordIsHeader,
        limits: supportedLimits
    )
    #expect(repeated == plain)
}

@Test
func exactCSVSnapshotBindingInvalidatesCrossSourceDraftWithoutChangingPreparedData() throws {
    let source = Data("unit\n0\n0.001".utf8)
    var withBOM = Data([0xEF, 0xBB, 0xBF])
    withBOM.append(source)
    let plain = try CanonicalCSVStagingReader.readWithProvenance(
        data: source,
        headerDecision: .firstRecordIsHeader,
        limits: supportedLimits
    ).stagedImport
    let bom = try CanonicalCSVStagingReader.readWithProvenance(
        data: withBOM,
        headerDecision: .firstRecordIsHeader,
        limits: supportedLimits
    ).stagedImport

    func draft(boundTo staged: StagedScientificImport) throws -> ScientificImportManifestDraft {
        let column = try StagedSourceColumnReference(oneBasedIndex: 1)
        let groupID = ScientificEventScopeGroupID(
            try ScientificSemanticID(validating: "recording")
        )
        let trainID = ScientificSpikeTrainID(
            try ScientificSemanticID(validating: "unit")
        )
        return ScientificImportManifestDraft(
            boundTo: staged,
            sourceTimeUnit: .seconds,
            activityMode: .putativeSingleUnit,
            eventScopeGroups: [
                EventScopeGroupManifestDraft(
                    semanticID: groupID,
                    spikeTrains: [
                        SpikeTrainColumnManifestDraft(
                            sourceColumn: column,
                            semanticID: trainID,
                            orderDecision: .preserveSourceOrder,
                            duplicateDecision: .preserveMultiplicity
                        ),
                    ],
                    eventDefinitions: [],
                    timeBasis: .recordingElapsed
                ),
            ]
        )
    }

    let plainDraft = try draft(boundTo: plain)
    let bomDraft = try draft(boundTo: bom)
    let plainPlan = try ScientificImportPlanResolver.resolve(
        stagedImport: plain,
        draft: plainDraft
    )
    let bomPlan = try ScientificImportPlanResolver.resolve(stagedImport: bom, draft: bomDraft)
    let plainPrepared = try ScientificImportNormalizer.normalize(resolvedPlan: plainPlan)
    let bomPrepared = try ScientificImportNormalizer.normalize(resolvedPlan: bomPlan)

    #expect(plainPrepared.data == bomPrepared.data)
    #expect(plainPrepared.provenance != bomPrepared.provenance)
    #expect(PreparedScientificImportValidator.validate(plainPrepared).blockingIssues.isEmpty)
    #expect(PreparedScientificImportValidator.validate(bomPrepared).blockingIssues.isEmpty)

    do {
        _ = try ScientificImportPlanResolver.resolve(stagedImport: bom, draft: plainDraft)
        Issue.record("Expected exact-source transaction mismatch")
    } catch let error as ScientificImportPlanResolutionError {
        #expect(error.issues == [.sourceBindingMismatch])
    }
}

@Test
func legacyCSVHeaderDecisionAndSixArgumentLimitsInitializerRemainSourceCompatible() throws {
    let legacyDecision: CanonicalCSVHeaderDecision = .firstRecordIsHeader
    let sharedDecision: CanonicalTabularHeaderDecision = legacyDecision
    let legacyLimits = CanonicalCSVStagingLimits(
        maximumSourceByteCount: 32,
        maximumColumnCount: 2,
        maximumLogicalDataRowCount: 2,
        maximumMaterializedDataCellCount: 4,
        maximumRawFieldUTF8ByteCount: 8,
        maximumDecodedFieldUTF8ByteCount: 8
    )

    #expect(sharedDecision == .firstRecordIsHeader)
    #expect(
        legacyLimits.maximumTotalMaterializedTextUTF8ByteCount
            == CanonicalTabularWorkloadLimits.supportedDatasetEnvelope
                .maximumTotalMaterializedTextUTF8ByteCount
    )
    let staged = try CanonicalCSVStagingReader.read(
        data: Data("header\nvalue".utf8),
        headerDecision: legacyDecision,
        limits: legacyLimits
    )
    #expect(staged.columns[0].header == "header")
    #expect(staged.columns[0].cells == [.text(rawText: "value")])
}

@Test
func headerDecisionIsExplicitAndChangesOnlyTableStructure() throws {
    let withHeader = try readCSV("a,b\n1,2", headerDecision: .firstRecordIsHeader)
    let headerless = try readCSV("a,b\n1,2", headerDecision: .headerless)

    #expect(withHeader.columns.map(\.header) == ["a", "b"])
    #expect(withHeader.dataRowCount == 1)
    #expect(withHeader.columns[0].cells == [.text(rawText: "1")])
    #expect(headerless.columns.map(\.header) == [nil, nil])
    #expect(headerless.dataRowCount == 2)
    #expect(headerless.columns[0].cells == [.text(rawText: "a"), .text(rawText: "1")])
}

@Test
func duplicateAndEmptyHeadersRemainDistinctSourceFacts() throws {
    let staged = try readCSV("same,,same\r\n1,2,3", headerDecision: .firstRecordIsHeader)

    #expect(staged.columns.map(\.header) == ["same", "", "same"])
    #expect(staged.columns.map(\.cells) == [
        [.text(rawText: "1")],
        [.text(rawText: "2")],
        [.text(rawText: "3")],
    ])
}

@Test
func quotedCommasAndDoubledQuotesAreDecodedWithoutSemanticParsing() throws {
    let staged = try readCSV(#""a,b","a""b","1.000000","@x=2""#,
                             headerDecision: .headerless)

    #expect(staged.dataRowCount == 1)
    #expect(staged.columns.map(\.cells) == [
        [.text(rawText: "a,b")],
        [.text(rawText: "a\"b")],
        [.text(rawText: "1.000000")],
        [.text(rawText: "@x=2")],
    ])
    #expect(staged.suggestions == .none)
}

@Test
func provenanceRetainsRawQuotedCRLFCommaAndEscapedQuoteLexeme() throws {
    let source = "header,other\r\n\"line 1\r\nline 2, \"\"quoted\"\"\",plain"
    let result = try CanonicalCSVStagingReader.readWithProvenance(
        data: Data(source.utf8),
        headerDecision: .firstRecordIsHeader,
        limits: supportedLimits
    )
    let firstColumn = try StagedSourceColumnReference(oneBasedIndex: 1)
    let firstDataCell = try StagedSourceCellReference(
        column: firstColumn,
        oneBasedDataRowIndex: 1
    )
    let header = try #require(result.provenance.headerField(for: firstColumn))
    let field = try #require(result.provenance.dataField(at: firstDataCell))

    #expect(header.rawUTF8Lexeme == Data("header".utf8))
    #expect(header.decodedText == "header")
    #expect(!header.wasQuoted)
    #expect(field.rawUTF8Lexeme == Data("\"line 1\r\nline 2, \"\"quoted\"\"\"".utf8))
    #expect(field.decodedText == "line 1\r\nline 2, \"quoted\"")
    #expect(field.wasQuoted)
    #expect(field.location == fieldLocation(
        byteOffset: 15,
        line: 2,
        column: 1,
        logicalRecord: 2,
        field: 1
    ))
}

@Test
func multibyteUTF8TextIsPreservedExactly() throws {
    let staged = try readCSV("\"神经,β\",café", headerDecision: .headerless)

    #expect(staged.columns.map(\.cells) == [
        [.text(rawText: "神经,β")],
        [.text(rawText: "café")],
    ])
}

@Test
func quotedCRLFVariantsArePreservedExactlyInsideFields() throws {
    let staged = try readCSV(
        "\"a\rb\",\"c\nd\",\"e\r\nf\"",
        headerDecision: .headerless
    )

    #expect(staged.dataRowCount == 1)
    #expect(staged.columns.map(\.cells) == [
        [.text(rawText: "a\rb")],
        [.text(rawText: "c\nd")],
        [.text(rawText: "e\r\nf")],
    ])
}

@Test
func embeddedNewlineDoesNotChangeLogicalDataRowProvenance() throws {
    let staged = try readCSV(
        "value\r\n\"first\ncontinued\"\r\nsecond",
        headerDecision: .firstRecordIsHeader
    )
    let firstColumn = try StagedSourceColumnReference(oneBasedIndex: 1)
    let firstCell = try StagedSourceCellReference(
        column: firstColumn,
        oneBasedDataRowIndex: 1
    )
    let secondCell = try StagedSourceCellReference(
        column: firstColumn,
        oneBasedDataRowIndex: 2
    )

    #expect(staged.dataRowCount == 2)
    #expect(staged.cell(at: firstCell) == .text(rawText: "first\ncontinued"))
    #expect(staged.cell(at: secondCell) == .text(rawText: "second"))
}

@Test
func structuralBlankWhitespaceAndQuotedEmptyRemainDifferent() throws {
    let staged = try readCSV(", ,\"\",x", headerDecision: .headerless)

    #expect(staged.columns.map(\.cells) == [
        [.blank],
        [.text(rawText: " ")],
        [.text(rawText: "")],
        [.text(rawText: "x")],
    ])
    #expect(staged.columns.flatMap(\.cells).allSatisfy { cell in
        if case .spreadsheetNumber = cell { return false }
        return true
    })
}

@Test
func headerlessRaggedRowsExpandToMaximumWidthAndPadStructuralBlanks() throws {
    let data = Data("a\nb,c\n".utf8)
    let result = try CanonicalCSVStagingReader.readWithProvenance(
        data: data,
        headerDecision: .headerless,
        limits: supportedLimits
    )
    let staged = result.stagedImport
    let firstColumn = try StagedSourceColumnReference(oneBasedIndex: 1)
    let secondColumn = try StagedSourceColumnReference(oneBasedIndex: 2)
    let paddedCell = try StagedSourceCellReference(
        column: secondColumn,
        oneBasedDataRowIndex: 1
    )
    let presentCell = try StagedSourceCellReference(
        column: secondColumn,
        oneBasedDataRowIndex: 2
    )

    #expect(staged.dataRowCount == 2)
    #expect(staged.columns.count == 2)
    #expect(staged.columns[0].cells == [.text(rawText: "a"), .text(rawText: "b")])
    #expect(staged.columns[1].cells == [.blank, .text(rawText: "c")])
    #expect(result.provenance.headerField(for: firstColumn) == nil)
    #expect(result.provenance.dataField(at: paddedCell) == nil)
    #expect(result.provenance.dataField(at: presentCell)?.rawUTF8Lexeme == Data("c".utf8))
}

@Test
func provenanceLookupReturnsNilForMaximumRepresentableDataRow() throws {
    let column = try StagedSourceColumnReference(oneBasedIndex: 1)
    let farOutsideCell = try StagedSourceCellReference(
        column: column,
        oneBasedDataRowIndex: Int.max
    )
    for headerDecision: CanonicalTabularHeaderDecision in [
        .firstRecordIsHeader,
        .headerless,
    ] {
        let result = try CanonicalCSVStagingReader.readWithProvenance(
            data: Data("value\n1".utf8),
            headerDecision: headerDecision,
            limits: supportedLimits
        )
        #expect(result.provenance.dataField(at: farOutsideCell) == nil)
    }
}

@Test
func headerWidthPadsShortRowsButBlocksWiderRows() throws {
    let padded = try readCSV("a,b\n1\n2,3", headerDecision: .firstRecordIsHeader)
    #expect(padded.columns[0].cells == [.text(rawText: "1"), .text(rawText: "2")])
    #expect(padded.columns[1].cells == [.blank, .text(rawText: "3")])

    #expect(
        readerError("a,b\n1,2,3", headerDecision: .firstRecordIsHeader)
            == .dataRecordWiderThanHeader(
                headerColumnCount: 2,
                actual: 3,
                location: fieldLocation(
                    byteOffset: 9,
                    line: 2,
                    column: 5,
                    logicalRecord: 2,
                    field: 3
                )
            )
    )
}

@Test(arguments: ["a\n", "a\r", "a\r\n"])
func terminalRecordSeparatorDoesNotCreatePhantomRecord(_ source: String) throws {
    let staged = try readCSV(source, headerDecision: .headerless)

    #expect(staged.dataRowCount == 1)
    #expect(staged.columns.count == 1)
    #expect(staged.columns[0].cells == [.text(rawText: "a")])
}

@Test
func trailingDelimiterCreatesARealFinalEmptyField() throws {
    let staged = try readCSV("a,b,", headerDecision: .headerless)

    #expect(staged.columns.count == 3)
    #expect(staged.columns.map(\.cells) == [
        [.text(rawText: "a")],
        [.text(rawText: "b")],
        [.blank],
    ])
}

@Test
func quoteInsideUnquotedFieldHasPhysicalAndLogicalLocation() {
    #expect(
        readerError("a\nb\"c") == .syntax(
            issue: .quoteInUnquotedField,
            location: fieldLocation(
                byteOffset: 4,
                line: 2,
                column: 2,
                logicalRecord: 2,
                field: 1
            )
        )
    )
}

@Test
func contentAfterClosingQuoteHasEmbeddedNewlineLocation() {
    #expect(
        readerError("\"a\nb\"x") == .syntax(
            issue: .unexpectedContentAfterClosingQuote,
            location: fieldLocation(
                byteOffset: 6,
                line: 2,
                column: 3,
                logicalRecord: 1,
                field: 1
            )
        )
    )
}

@Test
func unterminatedQuotedFieldPointsToOpeningQuote() {
    #expect(
        readerError("x,\"abc") == .syntax(
            issue: .unterminatedQuotedField,
            location: fieldLocation(
                byteOffset: 3,
                line: 1,
                column: 3,
                logicalRecord: 1,
                field: 2
            )
        )
    )
}

@Test
func malformedUTF8ProducesTypedByteLocation() {
    let malformed = Data([0x61, 0x0A, 0xC3, 0x28])

    #expect(
        readerError(data: malformed) == .invalidUTF8(
            location: sourceLocation(byteOffset: 4, line: 2, column: 2)
        )
    )
}

@Test
func emptyAndBOMOnlySourcesAreBlocked() {
    #expect(readerError(data: Data()) == .emptySource)
    #expect(readerError(data: Data([0xEF, 0xBB, 0xBF])) == .zeroColumnLogicalTable)
}

@Test
func supportedLimitsAreNamedAndExact() {
    let shared = CanonicalTabularWorkloadLimits.supportedDatasetEnvelope
    #expect(shared.maximumColumnCount == 512)
    #expect(shared.maximumLogicalDataRowCount == 1_000_000)
    #expect(shared.maximumMaterializedDataCellCount == 1_000_000)
    #expect(shared.maximumDecodedCellUTF8ByteCount == 65_794)
    #expect(shared.maximumTotalMaterializedTextUTF8ByteCount == 67_108_864)

    #expect(supportedLimits.maximumSourceByteCount == 5_242_880)
    #expect(supportedLimits.maximumColumnCount == 512)
    #expect(supportedLimits.maximumLogicalDataRowCount == 1_000_000)
    #expect(supportedLimits.maximumMaterializedDataCellCount == 1_000_000)
    #expect(supportedLimits.maximumRawFieldUTF8ByteCount == 131_590)
    #expect(supportedLimits.maximumDecodedFieldUTF8ByteCount == 65_794)
    #expect(supportedLimits.maximumTotalMaterializedTextUTF8ByteCount == 67_108_864)
    #expect(supportedLimits.workloadLimits == shared)
}

@Test
func fullyEscapedQuotedFieldReachesBothSupportedFieldBoundaries() throws {
    let decoded = String(
        repeating: "\"",
        count: CanonicalCSVStagingLimits.supportedMaximumDecodedFieldUTF8ByteCount
    )
    let encoded = "\"" + String(
        repeating: "\"\"",
        count: CanonicalCSVStagingLimits.supportedMaximumDecodedFieldUTF8ByteCount
    ) + "\""

    #expect(decoded.utf8.count == 65_794)
    #expect(encoded.utf8.count == 131_590)
    let staged = try readCSV(encoded, headerDecision: .headerless)
    #expect(staged.columns[0].cells == [.text(rawText: decoded)])
}

@Test
func exactFiveMiBSourcePassesAndOneByteOverBlocksBeforeParsing() throws {
    var exact = Data()
    for _ in 0..<79 {
        exact.append(Data(repeating: 0x61, count: 65_535))
        exact.append(0x0A)
    }
    exact.append(Data(repeating: 0x61, count: 65_536))
    #expect(exact.count == supportedLimits.maximumSourceByteCount)

    let staged = try CanonicalCSVStagingReader.read(
        data: exact,
        headerDecision: .headerless,
        limits: supportedLimits
    )
    #expect(staged.columns.count == 1)
    #expect(staged.dataRowCount == 80)

    var over = exact
    over.append(0x61)
    #expect(
        readerError(data: over) == .sourceByteLimitExceeded(
            maximum: 5_242_880,
            actual: 5_242_881
        )
    )
}

@Test
func invalidAndRelaxedLimitsAreRejected() {
    #expect(
        readerError("a", limits: limits(sourceBytes: 0))
            == .invalidLimit(kind: .sourceBytes, actual: 0)
    )
    #expect(
        readerError("a", limits: limits(columns: 0))
            == .invalidLimit(kind: .columns, actual: 0)
    )
    #expect(
        readerError("a", limits: limits(dataRows: 0))
            == .invalidLimit(kind: .logicalDataRows, actual: 0)
    )
    #expect(
        readerError("a", limits: limits(materializedCells: 0))
            == .invalidLimit(kind: .materializedDataCells, actual: 0)
    )
    #expect(
        readerError("a", limits: limits(decodedFieldBytes: 0))
            == .invalidLimit(kind: .decodedFieldUTF8Bytes, actual: 0)
    )
    #expect(
        readerError("a", limits: limits(totalTextBytes: 0))
            == .invalidLimit(kind: .totalMaterializedTextUTF8Bytes, actual: 0)
    )
    #expect(
        readerError(
            "a",
            limits: limits(sourceBytes: supportedLimits.maximumSourceByteCount + 1)
        ) == .limitExceedsSupportedEnvelope(
            kind: .sourceBytes,
            maximumSupported: supportedLimits.maximumSourceByteCount,
            actual: supportedLimits.maximumSourceByteCount + 1
        )
    )
    #expect(
        readerError(
            "a",
            limits: limits(columns: supportedLimits.maximumColumnCount + 1)
        ) == .limitExceedsSupportedEnvelope(
            kind: .columns,
            maximumSupported: supportedLimits.maximumColumnCount,
            actual: supportedLimits.maximumColumnCount + 1
        )
    )
    #expect(
        readerError(
            "a",
            limits: limits(dataRows: supportedLimits.maximumLogicalDataRowCount + 1)
        ) == .limitExceedsSupportedEnvelope(
            kind: .logicalDataRows,
            maximumSupported: supportedLimits.maximumLogicalDataRowCount,
            actual: supportedLimits.maximumLogicalDataRowCount + 1
        )
    )
    #expect(
        readerError(
            "a",
            limits: limits(materializedCells: supportedLimits.maximumMaterializedDataCellCount + 1)
        ) == .limitExceedsSupportedEnvelope(
            kind: .materializedDataCells,
            maximumSupported: supportedLimits.maximumMaterializedDataCellCount,
            actual: supportedLimits.maximumMaterializedDataCellCount + 1
        )
    )
    #expect(
        readerError(
            "a",
            limits: limits(rawFieldBytes: supportedLimits.maximumRawFieldUTF8ByteCount + 1)
        ) == .limitExceedsSupportedEnvelope(
            kind: .rawFieldUTF8Bytes,
            maximumSupported: supportedLimits.maximumRawFieldUTF8ByteCount,
            actual: supportedLimits.maximumRawFieldUTF8ByteCount + 1
        )
    )
    #expect(
        readerError(
            "a",
            limits: limits(decodedFieldBytes: supportedLimits.maximumDecodedFieldUTF8ByteCount + 1)
        ) == .limitExceedsSupportedEnvelope(
            kind: .decodedFieldUTF8Bytes,
            maximumSupported: supportedLimits.maximumDecodedFieldUTF8ByteCount,
            actual: supportedLimits.maximumDecodedFieldUTF8ByteCount + 1
        )
    )
    #expect(
        readerError(
            "a",
            limits: limits(
                totalTextBytes:
                    supportedLimits.maximumTotalMaterializedTextUTF8ByteCount + 1
            )
        ) == .limitExceedsSupportedEnvelope(
            kind: .totalMaterializedTextUTF8Bytes,
            maximumSupported: supportedLimits.maximumTotalMaterializedTextUTF8ByteCount,
            actual: supportedLimits.maximumTotalMaterializedTextUTF8ByteCount + 1
        )
    )
}

@Test
func columnRowAndRectangularizedCellLimitsAreEnforced() {
    #expect(
        readerError("a,b,c", limits: limits(columns: 2))
            == .columnLimitExceeded(
                maximum: 2,
                actual: 3,
                location: fieldLocation(
                    byteOffset: 5,
                    line: 1,
                    column: 5,
                    logicalRecord: 1,
                    field: 3
                )
            )
    )
    #expect(
        readerError("a\nb\nc", limits: limits(dataRows: 2))
            == .logicalDataRowLimitExceeded(
                maximum: 2,
                actual: 3,
                location: sourceLocation(byteOffset: 5, line: 3, column: 1)
            )
    )
    #expect(
        readerError("a,b\nc,d", limits: limits(materializedCells: 3))
            == .materializedDataCellLimitExceeded(
                maximum: 3,
                actual: 4,
                location: sourceLocation(byteOffset: 5, line: 2, column: 1)
            )
    )
    // The second row has one explicit field, but padding makes the table 2 rows x 3 columns.
    #expect(
        readerError("a,b,c\nd", limits: limits(materializedCells: 5))
            == .materializedDataCellLimitExceeded(
                maximum: 5,
                actual: 6,
                location: sourceLocation(byteOffset: 7, line: 2, column: 1)
            )
    )
}

@Test
func sharedWorkloadShapeAndDecodedCellLimitsHaveExactAndNextValueCoverage() throws {
    let exactLimits = limits(
        columns: 2,
        dataRows: 2,
        materializedCells: 4,
        decodedFieldBytes: 2,
        totalTextBytes: 6
    )
    let exact = try readCSV(
        "aa,b\nc,dd",
        headerDecision: .headerless,
        limits: exactLimits
    )
    #expect(exact.columns.count == 2)
    #expect(exact.dataRowCount == 2)

    #expect(
        readerError("a,b,c", limits: limits(columns: 2))
            == .columnLimitExceeded(
                maximum: 2,
                actual: 3,
                location: fieldLocation(
                    byteOffset: 5,
                    line: 1,
                    column: 5,
                    logicalRecord: 1,
                    field: 3
                )
            )
    )
    #expect(
        readerError("a\nb\nc", limits: limits(dataRows: 2))
            == .logicalDataRowLimitExceeded(
                maximum: 2,
                actual: 3,
                location: sourceLocation(byteOffset: 5, line: 3, column: 1)
            )
    )
    #expect(
        readerError("a,b\nc,d", limits: limits(materializedCells: 3))
            == .materializedDataCellLimitExceeded(
                maximum: 3,
                actual: 4,
                location: sourceLocation(byteOffset: 5, line: 2, column: 1)
            )
    )
    #expect(
        readerError("abc", limits: limits(decodedFieldBytes: 2))
            == .decodedFieldByteLimitExceeded(
                maximum: 2,
                actual: 3,
                location: fieldLocation(
                    byteOffset: 3,
                    line: 1,
                    column: 3,
                    logicalRecord: 1,
                    field: 1
                )
            )
    )
}

@Test
func totalMaterializedTextLimitCountsHeadersAndEveryActualRepeatedField() throws {
    let headerLimits = limits(decodedFieldBytes: 2, totalTextBytes: 3)
    let exact = try readCSV(
        "hh\nx",
        headerDecision: .firstRecordIsHeader,
        limits: headerLimits
    )
    #expect(exact.columns[0].header == "hh")
    #expect(exact.columns[0].cells == [.text(rawText: "x")])

    #expect(
        readerError(
            "hh\nxx",
            headerDecision: .firstRecordIsHeader,
            limits: headerLimits
        ) == .totalMaterializedTextUTF8ByteLimitExceeded(
            maximum: 3,
            actual: 4,
            location: fieldLocation(
                byteOffset: 4,
                line: 2,
                column: 1,
                logicalRecord: 2,
                field: 1
            )
        )
    )

    #expect(
        readerError(
            "x,x,x",
            limits: limits(decodedFieldBytes: 1, totalTextBytes: 2)
        ) == .totalMaterializedTextUTF8ByteLimitExceeded(
            maximum: 2,
            actual: 3,
            location: fieldLocation(
                byteOffset: 5,
                line: 1,
                column: 5,
                logicalRecord: 1,
                field: 3
            )
        )
    )
}

@Test
func rawAndDecodedFieldLimitsHaveIndependentExactBoundaries() throws {
    let rawLimits = limits(rawFieldBytes: 3)
    let exactRaw = try readCSV("abc", headerDecision: .headerless, limits: rawLimits)
    #expect(exactRaw.columns[0].cells == [.text(rawText: "abc")])
    #expect(
        readerError("abcd", limits: rawLimits) == .rawFieldByteLimitExceeded(
            maximum: 3,
            actual: 4,
            location: fieldLocation(
                byteOffset: 4,
                line: 1,
                column: 4,
                logicalRecord: 1,
                field: 1
            )
        )
    )

    let decodedLimits = limits(rawFieldBytes: 10, decodedFieldBytes: 3)
    let exactDecoded = try readCSV("\"a\"\"b\"", headerDecision: .headerless,
                                   limits: decodedLimits)
    #expect(exactDecoded.columns[0].cells == [.text(rawText: "a\"b")])
    #expect(
        readerError("abcd", limits: decodedLimits) == .decodedFieldByteLimitExceeded(
            maximum: 3,
            actual: 4,
            location: fieldLocation(
                byteOffset: 4,
                line: 1,
                column: 4,
                logicalRecord: 1,
                field: 1
            )
        )
    )
}

@Test
func highFieldCountInputCompletesWithoutPrefixRescanning() throws {
    let row = Array(repeating: "x", count: 100).joined(separator: ",")
    let source = Array(repeating: row, count: 1_000).joined(separator: "\n")

    let staged = try readCSV(source, headerDecision: .headerless)

    #expect(staged.columns.count == 100)
    #expect(staged.dataRowCount == 1_000)
    #expect(staged.columns.allSatisfy { $0.cells.count == 1_000 })
}

@Test
func errorsAreDeterministicAndNeverReturnPartialStaging() {
    let source = "a,b\n1,2,3"
    let first = readerError(source, headerDecision: .firstRecordIsHeader)
    let second = readerError(source, headerDecision: .firstRecordIsHeader)

    #expect(first != nil)
    #expect(first == second)
}
