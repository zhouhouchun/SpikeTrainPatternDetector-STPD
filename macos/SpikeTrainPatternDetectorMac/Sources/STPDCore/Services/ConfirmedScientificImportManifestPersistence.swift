import Foundation
import CryptoKit

// MARK: - Core-owned header rule

/// A functional, Core-owned representation of whether the first logical tabular record is a header.
///
/// STPDCore intentionally does not depend on STPDTabularIO, so it cannot reference
/// `CanonicalTabularHeaderDecision`. The app boundary maps between the two. Projection DERIVES this
/// rule from the optional header presence of the sealed, validated base's source columns (present →
/// first record is header; absent → headerless). The receipt then encodes that derived result
/// explicitly precisely because it stores no header text, and decoding reads back the stored value
/// rather than re-inferring the rule from header text it does not have.
public enum PersistedTabularHeaderRule: Hashable, Sendable {
    case firstRecordIsHeader
    case headerless
}

/// A fail-closed defect while deriving the header rule from a sealed, independently validated base
/// confirmation. The header rule is NEVER a caller-supplied value: it is derived solely from whether
/// the base's validated source columns carry header text (optional presence only — the text itself is
/// never inspected, trimmed, or parsed). A sealed base can only reach these states through a defect, so
/// derivation fails closed rather than guessing.
public enum PersistedHeaderRuleDerivationError: Error, Equatable, Sendable {
    /// The sealed base has no source columns, so header presence is undefined.
    case noSourceColumns
    /// Some source columns carry a header and others do not; a sealed validated source is never mixed.
    case mixedHeaderPresence
}

// MARK: - Receipt value type (decisions only — never raw source)

/// A source-bound, decision-only confirmation receipt. It is deliberately lightweight: it carries the
/// canonical fingerprint identity triple, the exact source transaction binding, the explicit header
/// rule, and every confirmed manifest decision needed to *replay* the import chain from a reselected
/// source — and nothing that would let it *reconstruct* the source. It never contains source bytes,
/// raw cells, raw lexemes, header text, timestamps, event values, or Presentation values.
///
/// The receipt is `Hashable`/`Equatable` because it holds only bounded decision metadata (no
/// spike-timestamp arrays or source cells), so exact structural comparison is cheap and safe. Its
/// `confirmationRecordDigest` identifies only this source-bound saved record; it is never a second
/// scientific dataset identity — the canonical fingerprint remains the sole scientific identity.
public struct ConfirmedScientificImportReceipt: Hashable, Sendable {
    // Canonical fingerprint identity triple — the sole scientific identity, copied for verification.
    public let canonicalSchemaContractID: String
    public let canonicalSchemaContractDigest: String
    public let canonicalDatasetDigest: String

    // Exact reviewed source transaction (SHA + CSV/XLSX selection). Source-bound provenance only.
    public let sourceTransactionBinding: StagedSourceTransactionBinding
    public let headerRule: PersistedTabularHeaderRule
    public let sourceTimeUnit: SpikeTimeUnit
    public let activityMode: ScientificDatasetActivityMode
    public let recordingSegment: ConfirmedRecordingSegment
    public let eventScopeGroups: [ReceiptEventScopeGroup]
    /// Every confirmed attribute definition, including Presentation-role definitions (which change
    /// the record digest but never the canonical fingerprint). Canonically ordered by key.
    public let attributeDefinitions: [ReceiptAttributeDefinition]

    /// A deterministic digest of the canonical receipt body, domain-separated from the schema and
    /// canonical-dataset digests. Identifies only this saved record, never scientific identity.
    public let confirmationRecordDigest: String

    internal init(
        canonicalSchemaContractID: String,
        canonicalSchemaContractDigest: String,
        canonicalDatasetDigest: String,
        sourceTransactionBinding: StagedSourceTransactionBinding,
        headerRule: PersistedTabularHeaderRule,
        sourceTimeUnit: SpikeTimeUnit,
        activityMode: ScientificDatasetActivityMode,
        recordingSegment: ConfirmedRecordingSegment,
        eventScopeGroups: [ReceiptEventScopeGroup],
        attributeDefinitions: [ReceiptAttributeDefinition],
        confirmationRecordDigest: String
    ) {
        self.canonicalSchemaContractID = canonicalSchemaContractID
        self.canonicalSchemaContractDigest = canonicalSchemaContractDigest
        self.canonicalDatasetDigest = canonicalDatasetDigest
        self.sourceTransactionBinding = sourceTransactionBinding
        self.headerRule = headerRule
        self.sourceTimeUnit = sourceTimeUnit
        self.activityMode = activityMode
        self.recordingSegment = recordingSegment
        self.eventScopeGroups = eventScopeGroups
        self.attributeDefinitions = attributeDefinitions
        self.confirmationRecordDigest = confirmationRecordDigest
    }

    /// The canonical fingerprint identity triple carried by this receipt.
    public var canonicalFingerprintTriple: (schemaContractID: String, schemaContractDigest: String, datasetDigest: String) {
        (canonicalSchemaContractID, canonicalSchemaContractDigest, canonicalDatasetDigest)
    }
}

/// One event-scope group's confirmed decisions, in canonical (ascending source-column) order.
public struct ReceiptEventScopeGroup: Hashable, Sendable {
    public let semanticID: ScientificEventScopeGroupID
    public let timeBasis: ReceiptEventScopeTimeBasis
    public let spikeTrains: [ReceiptSpikeTrainColumn]
    public let eventDefinitions: [ReceiptEventDefinitionColumn]

    public init(
        semanticID: ScientificEventScopeGroupID,
        timeBasis: ReceiptEventScopeTimeBasis,
        spikeTrains: [ReceiptSpikeTrainColumn],
        eventDefinitions: [ReceiptEventDefinitionColumn]
    ) {
        self.semanticID = semanticID
        self.timeBasis = timeBasis
        self.spikeTrains = spikeTrains
        self.eventDefinitions = eventDefinitions
    }
}

/// The confirmed group time basis. An event-relative origin is stored as its draft-level source-cell
/// coordinates (one-based column and data-row) so the draft can be replayed, never as a timestamp.
public enum ReceiptEventScopeTimeBasis: Hashable, Sendable {
    case recordingElapsed
    case eventRelative(originColumnOneBasedIndex: Int, originDataRowOneBasedIndex: Int)
}

public struct ReceiptSpikeTrainColumn: Hashable, Sendable {
    public let sourceColumnOneBasedIndex: Int
    public let semanticID: ScientificSpikeTrainID
    public let orderDecision: TimestampOrderDecision
    public let duplicateDecision: ExactDuplicateDecision

    public init(
        sourceColumnOneBasedIndex: Int,
        semanticID: ScientificSpikeTrainID,
        orderDecision: TimestampOrderDecision,
        duplicateDecision: ExactDuplicateDecision
    ) {
        self.sourceColumnOneBasedIndex = sourceColumnOneBasedIndex
        self.semanticID = semanticID
        self.orderDecision = orderDecision
        self.duplicateDecision = duplicateDecision
    }
}

public struct ReceiptEventDefinitionColumn: Hashable, Sendable {
    public let sourceColumnOneBasedIndex: Int
    public let semanticID: ScientificEventDefinitionID
    public let eventTypeID: ScientificEventTypeID
    public let orderDecision: TimestampOrderDecision

    public init(
        sourceColumnOneBasedIndex: Int,
        semanticID: ScientificEventDefinitionID,
        eventTypeID: ScientificEventTypeID,
        orderDecision: TimestampOrderDecision
    ) {
        self.sourceColumnOneBasedIndex = sourceColumnOneBasedIndex
        self.semanticID = semanticID
        self.eventTypeID = eventTypeID
        self.orderDecision = orderDecision
    }
}

public struct ReceiptAttributeDefinition: Hashable, Sendable {
    public let key: EventAttributeKey
    public let scalarType: EventAttributeScalarType
    public let role: EventAttributeRole
    public let unit: ConfirmedEventAttributeUnit
    public let emptyStringPolicy: EventEmptyStringPolicy

    public init(
        key: EventAttributeKey,
        scalarType: EventAttributeScalarType,
        role: EventAttributeRole,
        unit: ConfirmedEventAttributeUnit,
        emptyStringPolicy: EventEmptyStringPolicy
    ) {
        self.key = key
        self.scalarType = scalarType
        self.role = role
        self.unit = unit
        self.emptyStringPolicy = emptyStringPolicy
    }
}

// MARK: - Decoding errors (all fail-closed)

/// A fail-closed decode/parse defect. The bounded reader never repairs, migrates, or best-effort
/// interprets a defective receipt; every defect is a distinct, named rejection.
public enum PersistedReceiptDecodingError: Error, Equatable, Sendable {
    case fileTooLarge(maximumBytes: Int)
    case truncated
    case trailingBytes
    case unexpectedFieldTag(expected: UInt8, found: UInt8)
    case unknownToken(String)
    case invalidUTF8
    case invalidIdentifier
    case invalidSourceBinding
    case invalidSourceColumnIndex
    case invalidWorksheetSheetID
    case countOutOfRange
    case tokenLengthOutOfRange
    case noncanonicalCollectionOrder
    case unsupportedSchemaContract
    case schemaContractDigestMismatch
    case recordDigestMismatch
    // Receipt-graph invariants a valid resolver/validator output could never violate.
    case emptyGroupCollection
    case groupWithoutSpikeTrain
    case duplicateSourceColumn
    case duplicateSemanticIdentity
    case invalidGroupPartition
    case eventRelativeOriginNotInGroup
    case invalidAttributeCombination
    case headerlessTopologyInvalid
    case collapseNotAllowedForActivityMode
}

/// A fail-closed defect while reading a receipt file from disk through the bounded reader.
public enum PersistedStoreReadError: Error, Equatable, Sendable {
    case notFound
    case cannotOpen
    /// The path is a symlink, directory, device, or otherwise not a regular file.
    case notRegularFile
    case ioError
    case fileTooLarge(maximumBytes: Int)
    case decode(PersistedReceiptDecodingError)
}

// MARK: - Codec

/// The dedicated deterministic persistence codec and strict bounded reader for a decision-only
/// confirmation receipt. It does not use `Codable`/`JSONDecoder`. The encoder and the schema-contract
/// transcript bind field tags/order, the token vocabulary, integer/string/count encoding, collection
/// ordering, size/count limits, and record-digest domain separation. The reader fails closed on every
/// listed defect. This codec is independent of the canonical dataset fingerprint codec; changing
/// either does not silently change the other, and fixed goldens pin both.
public enum ConfirmedScientificImportManifestPersistence {
    /// A functional (non-ordinal) schema identifier describing what the receipt binds.
    public static let schemaContractID =
        "confirmed_scientific_import_manifest_source_bound_decision_receipt"

    /// The largest receipt file the bounded reader will accept.
    public static let maximumFileByteCount = 8 * 1_024 * 1_024

    /// Bounded auxiliary limits the reader enforces before allocating.
    static let maximumTokenByteCount = 4_096
    static let maximumCollectionCount = 100_000

    // MARK: Schema contract digest

    /// The digest of the machine-auditable schema-contract transcript. Deterministic and stable.
    public static func schemaContractDigest() -> String {
        var sink = ReceiptDigestSHA256Sink()
        encodeSchemaContract(into: &sink)
        return sink.finalizeHex()
    }

    // MARK: Header-rule derivation (the single authoritative source)

    /// The ONE authoritative derivation of the persisted header rule. It reads only the sealed,
    /// independently validated base confirmation's source columns —
    /// `validatedImport.preparedImport.provenance.resolvedPlan.source.columns` — and inspects ONLY each
    /// column's optional header presence. It never inspects, trims, parses, or interprets header text,
    /// and never consults the file extension, transport type, column names/contents, UI state, receipt
    /// contents, first-row values, majority voting, or the first column alone. There is no
    /// caller-supplied header rule anywhere in the persistence trust chain.
    public static func deriveHeaderRule(
        from confirmation: ConfirmedScientificImportManifest
    ) throws -> PersistedTabularHeaderRule {
        try deriveHeaderRule(
            fromColumnHeaders:
                confirmation.validatedImport.preparedImport.provenance.resolvedPlan.source.columns
                .map(\.header)
        )
    }

    /// The pure derivation kernel over each source column's optional header. Presence is `header != nil`
    /// ONLY: an explicitly present empty or whitespace-only header string still means "header present"
    /// and is never treated as headerless. Fails closed on an empty or mixed column collection rather
    /// than defaulting to headerless — the validator prevents these in normal sealed bases, but the
    /// shared helper still guards defensively.
    static func deriveHeaderRule(
        fromColumnHeaders headers: [String?]
    ) throws -> PersistedTabularHeaderRule {
        guard !headers.isEmpty else { throw PersistedHeaderRuleDerivationError.noSourceColumns }
        let presence = headers.map { $0 != nil }
        if presence.allSatisfy({ $0 }) { return .firstRecordIsHeader }
        if presence.allSatisfy({ !$0 }) { return .headerless }
        throw PersistedHeaderRuleDerivationError.mixedHeaderPresence
    }

    // MARK: Projection (decision-only; no restage/normalize/validate/project/hash-of-timestamps)

    /// Projects a receipt from a confirmed manifest by traversing decision metadata only. It reads
    /// the resolved plan and the already-produced canonical fingerprint; it never restages the
    /// source, reruns normalization/validation/projection/fingerprinting, or scans spike timestamps.
    /// The header rule is DERIVED from the sealed base (never supplied), so a headerful base can never
    /// be projected as headerless. Fails closed if the base's header presence is empty or mixed.
    public static func project(
        from confirmation: ConfirmedScientificImportManifest
    ) throws -> ConfirmedScientificImportReceipt {
        let headerRule = try deriveHeaderRule(from: confirmation)
        let plan = confirmation.validatedImport.preparedImport.provenance.resolvedPlan
        let fingerprint = confirmation.canonicalFingerprint

        let groups = plan.eventScopeGroups
            .map(receiptGroup(from:))
            .sorted { canonicalGroupOrderKey($0) < canonicalGroupOrderKey($1) }

        let attributes = plan.eventAttributeDefinitions
            .map { ReceiptAttributeDefinition(
                key: $0.key, scalarType: $0.scalarType, role: $0.role,
                unit: $0.unit, emptyStringPolicy: $0.emptyStringPolicy
            ) }
            .sorted { $0.key.canonicalText.utf8.lexicographicallyPrecedes($1.key.canonicalText.utf8) }

        let interim = ConfirmedScientificImportReceipt(
            canonicalSchemaContractID: fingerprint.schemaContractID,
            canonicalSchemaContractDigest: fingerprint.schemaContractDigest,
            canonicalDatasetDigest: fingerprint.datasetDigest,
            sourceTransactionBinding: confirmation.sourceTransactionBinding,
            headerRule: headerRule,
            sourceTimeUnit: confirmation.sourceTimeUnit,
            activityMode: confirmation.activityMode,
            recordingSegment: confirmation.recordingSegment,
            eventScopeGroups: groups,
            attributeDefinitions: attributes,
            confirmationRecordDigest: ""
        )
        let digest = recordDigest(of: interim)
        let receipt = withRecordDigest(interim, digest)
        // Shared invariant: a projected receipt must satisfy the same graph invariants decoding
        // enforces, so projection and decoding cannot drift. The resolver guarantees this, so a
        // violation is a programmer error (debug assertion only; no production behavior change).
        assert((try? checkReceiptGraph(receipt)) != nil, "projected receipt violated graph invariants")
        return receipt
    }

    private static func receiptGroup(from group: ResolvedEventScopeGroupPlan) -> ReceiptEventScopeGroup {
        let spikes = group.spikeTrains
            .map { ReceiptSpikeTrainColumn(
                sourceColumnOneBasedIndex: $0.sourceColumn.oneBasedIndex,
                semanticID: $0.semanticID,
                orderDecision: $0.orderDecision,
                duplicateDecision: $0.duplicateDecision
            ) }
            .sorted { $0.sourceColumnOneBasedIndex < $1.sourceColumnOneBasedIndex }
        let events = group.eventDefinitions
            .map { ReceiptEventDefinitionColumn(
                sourceColumnOneBasedIndex: $0.sourceColumn.oneBasedIndex,
                semanticID: $0.semanticID,
                eventTypeID: $0.eventTypeID,
                orderDecision: $0.orderDecision
            ) }
            .sorted { $0.sourceColumnOneBasedIndex < $1.sourceColumnOneBasedIndex }
        let timeBasis: ReceiptEventScopeTimeBasis
        switch group.timeBasis {
        case .recordingElapsed:
            timeBasis = .recordingElapsed
        case .eventRelative(let origin):
            timeBasis = .eventRelative(
                originColumnOneBasedIndex: origin.timestampCell.column.oneBasedIndex,
                originDataRowOneBasedIndex: origin.timestampCell.oneBasedDataRowIndex
            )
        }
        return ReceiptEventScopeGroup(
            semanticID: group.semanticID,
            timeBasis: timeBasis,
            spikeTrains: spikes,
            eventDefinitions: events
        )
    }

    private static func canonicalGroupOrderKey(_ group: ReceiptEventScopeGroup) -> Int {
        group.spikeTrains.first?.sourceColumnOneBasedIndex ?? Int.max
    }

    private static func withRecordDigest(
        _ receipt: ConfirmedScientificImportReceipt,
        _ digest: String
    ) -> ConfirmedScientificImportReceipt {
        ConfirmedScientificImportReceipt(
            canonicalSchemaContractID: receipt.canonicalSchemaContractID,
            canonicalSchemaContractDigest: receipt.canonicalSchemaContractDigest,
            canonicalDatasetDigest: receipt.canonicalDatasetDigest,
            sourceTransactionBinding: receipt.sourceTransactionBinding,
            headerRule: receipt.headerRule,
            sourceTimeUnit: receipt.sourceTimeUnit,
            activityMode: receipt.activityMode,
            recordingSegment: receipt.recordingSegment,
            eventScopeGroups: receipt.eventScopeGroups,
            attributeDefinitions: receipt.attributeDefinitions,
            confirmationRecordDigest: digest
        )
    }

    /// The deterministic record digest: SHA-256 over the domain-separated receipt body (everything
    /// except the trailing record-digest field itself).
    static func recordDigest(of receipt: ConfirmedScientificImportReceipt) -> String {
        var sink = ReceiptDigestSHA256Sink()
        encodeBody(receipt, into: &sink)
        return sink.finalizeHex()
    }

    // MARK: Encoding

    /// Encodes the complete receipt file: the domain-separated body followed by the trailing
    /// record-digest field.
    public static func encode(_ receipt: ConfirmedScientificImportReceipt) -> [UInt8] {
        var sink = ReceiptByteCollectingSink()
        encodeBody(receipt, into: &sink)
        sink.feedField(FieldTag.recordDigest)
        sink.feedToken(receipt.confirmationRecordDigest)
        return sink.bytes
    }

    private static func encodeBody(
        _ receipt: ConfirmedScientificImportReceipt,
        into sink: inout some CanonicalTranscriptSink
    ) {
        sink.feedToken(Domain.record)

        sink.feedField(FieldTag.persistenceSchemaID)
        sink.feedToken(schemaContractID)
        sink.feedField(FieldTag.persistenceSchemaDigest)
        sink.feedToken(schemaContractDigest())

        sink.feedField(FieldTag.canonicalSchemaID)
        sink.feedToken(receipt.canonicalSchemaContractID)
        sink.feedField(FieldTag.canonicalSchemaDigest)
        sink.feedToken(receipt.canonicalSchemaContractDigest)
        sink.feedField(FieldTag.canonicalDatasetDigest)
        sink.feedToken(receipt.canonicalDatasetDigest)

        sink.feedField(FieldTag.sourceSHA)
        sink.feedToken(receipt.sourceTransactionBinding.sourceBytesSHA256)
        sink.feedField(FieldTag.sourceSelection)
        encodeSelection(receipt.sourceTransactionBinding.selection, into: &sink)

        sink.feedField(FieldTag.headerRule)
        sink.feedToken(Token.headerRule(receipt.headerRule))
        sink.feedField(FieldTag.sourceTimeUnit)
        sink.feedToken(Token.sourceTimeUnit(receipt.sourceTimeUnit))
        sink.feedField(FieldTag.activityMode)
        sink.feedToken(Token.activityMode(receipt.activityMode))

        let segment = receipt.recordingSegment
        sink.feedField(FieldTag.recordingSegmentID)
        sink.feedToken(segment.semanticID.semanticID.canonicalText)
        sink.feedField(FieldTag.recordingRegime)
        sink.feedToken(Token.recordingRegime(segment.regime))
        sink.feedField(FieldTag.importedExcerptCoverage)
        sink.feedToken(Token.importedExcerptCoverage(segment.importedExcerptCoverage))
        sink.feedField(FieldTag.observationBounds)
        sink.feedToken(Token.observationBounds(segment.observationBounds))

        sink.feedField(FieldTag.eventScopeGroups)
        sink.feedCount(receipt.eventScopeGroups.count)
        for group in receipt.eventScopeGroups {
            sink.feedField(FieldTag.groupID)
            sink.feedToken(group.semanticID.semanticID.canonicalText)
            sink.feedField(FieldTag.groupTimeBasis)
            encodeTimeBasis(group.timeBasis, into: &sink)

            sink.feedField(FieldTag.spikeTrainColumns)
            sink.feedCount(group.spikeTrains.count)
            for train in group.spikeTrains {
                sink.feedField(FieldTag.spikeSourceColumn)
                sink.feedInt64(Int64(train.sourceColumnOneBasedIndex))
                sink.feedField(FieldTag.spikeTrainID)
                sink.feedToken(train.semanticID.semanticID.canonicalText)
                sink.feedField(FieldTag.spikeOrderDecision)
                sink.feedToken(Token.orderDecision(train.orderDecision))
                sink.feedField(FieldTag.spikeDuplicateDecision)
                sink.feedToken(Token.duplicateDecision(train.duplicateDecision))
            }

            sink.feedField(FieldTag.eventDefinitionColumns)
            sink.feedCount(group.eventDefinitions.count)
            for definition in group.eventDefinitions {
                sink.feedField(FieldTag.eventSourceColumn)
                sink.feedInt64(Int64(definition.sourceColumnOneBasedIndex))
                sink.feedField(FieldTag.eventDefinitionID)
                sink.feedToken(definition.semanticID.semanticID.canonicalText)
                sink.feedField(FieldTag.eventTypeID)
                sink.feedToken(definition.eventTypeID.semanticID.canonicalText)
                sink.feedField(FieldTag.eventOrderDecision)
                sink.feedToken(Token.orderDecision(definition.orderDecision))
            }
        }

        sink.feedField(FieldTag.attributeDefinitions)
        sink.feedCount(receipt.attributeDefinitions.count)
        for attribute in receipt.attributeDefinitions {
            sink.feedField(FieldTag.attributeKey)
            sink.feedToken(attribute.key.canonicalText)
            sink.feedField(FieldTag.attributeScalarType)
            sink.feedToken(Token.scalarType(attribute.scalarType))
            sink.feedField(FieldTag.attributeRole)
            sink.feedToken(Token.role(attribute.role))
            sink.feedField(FieldTag.attributeUnit)
            encodeUnit(attribute.unit, into: &sink)
            sink.feedField(FieldTag.attributeEmptyStringPolicy)
            sink.feedToken(Token.emptyStringPolicy(attribute.emptyStringPolicy))
        }
    }

    private static func encodeSelection(
        _ selection: StagedSourceTransactionSelection,
        into sink: inout some CanonicalTranscriptSink
    ) {
        switch selection {
        case .commaSeparatedValues:
            sink.feedToken(Token.selectionCSV)
        case .excelWorksheet(let name, let sheetID, let relationshipID, let normalizedPartPath):
            sink.feedToken(Token.selectionExcelWorksheet)
            sink.feedField(FieldTag.worksheetName)
            sink.feedToken(name)
            sink.feedField(FieldTag.worksheetSheetID)
            sink.feedInt64(Int64(sheetID))
            sink.feedField(FieldTag.worksheetRelationshipID)
            sink.feedToken(relationshipID)
            sink.feedField(FieldTag.worksheetPartPath)
            sink.feedToken(normalizedPartPath)
        }
    }

    private static func encodeTimeBasis(
        _ timeBasis: ReceiptEventScopeTimeBasis,
        into sink: inout some CanonicalTranscriptSink
    ) {
        switch timeBasis {
        case .recordingElapsed:
            sink.feedToken(Token.timeBasisRecordingElapsed)
        case .eventRelative(let column, let dataRow):
            sink.feedToken(Token.timeBasisEventRelative)
            sink.feedField(FieldTag.originColumn)
            sink.feedInt64(Int64(column))
            sink.feedField(FieldTag.originDataRow)
            sink.feedInt64(Int64(dataRow))
        }
    }

    private static func encodeUnit(
        _ unit: ConfirmedEventAttributeUnit,
        into sink: inout some CanonicalTranscriptSink
    ) {
        switch unit {
        case .notApplicable:
            sink.feedToken(Token.unitNotApplicable)
        case .dimensionless:
            sink.feedToken(Token.unitDimensionless)
        case .specified(let symbol):
            sink.feedToken(Token.unitSpecified)
            sink.feedField(FieldTag.unitSymbol)
            sink.feedToken(symbol.canonicalText)
        }
    }

    // MARK: Decoding (strict bounded reader — fail closed)

    /// Decodes a receipt from bytes with a strict bounded reader. Fails closed on every listed defect
    /// (see `PersistedReceiptDecodingError`); never repairs, migrates, or best-effort interprets.
    public static func decode(_ bytes: [UInt8]) throws -> ConfirmedScientificImportReceipt {
        guard bytes.count <= maximumFileByteCount else {
            throw PersistedReceiptDecodingError.fileTooLarge(maximumBytes: maximumFileByteCount)
        }
        var reader = ReceiptByteReader(bytes)

        try reader.expectDomain(Domain.record)

        try reader.expectField(FieldTag.persistenceSchemaID)
        guard try reader.readToken() == schemaContractID else {
            throw PersistedReceiptDecodingError.unsupportedSchemaContract
        }
        try reader.expectField(FieldTag.persistenceSchemaDigest)
        guard try reader.readToken() == schemaContractDigest() else {
            throw PersistedReceiptDecodingError.schemaContractDigestMismatch
        }

        try reader.expectField(FieldTag.canonicalSchemaID)
        let canonicalSchemaID = try reader.readToken()
        try reader.expectField(FieldTag.canonicalSchemaDigest)
        let canonicalSchemaDigest = try reader.readToken()
        try reader.expectField(FieldTag.canonicalDatasetDigest)
        let canonicalDatasetDigest = try reader.readToken()

        try reader.expectField(FieldTag.sourceSHA)
        let sourceSHA = try reader.readToken()
        try reader.expectField(FieldTag.sourceSelection)
        let selection = try decodeSelection(&reader)
        let binding: StagedSourceTransactionBinding
        do {
            binding = try StagedSourceTransactionBinding(
                sourceBytesSHA256: sourceSHA, selection: selection
            )
        } catch {
            throw PersistedReceiptDecodingError.invalidSourceBinding
        }

        try reader.expectField(FieldTag.headerRule)
        let headerRule = try Token.decodeHeaderRule(reader.readToken())
        try reader.expectField(FieldTag.sourceTimeUnit)
        let sourceTimeUnit = try Token.decodeSourceTimeUnit(reader.readToken())
        try reader.expectField(FieldTag.activityMode)
        let activityMode = try Token.decodeActivityMode(reader.readToken())

        try reader.expectField(FieldTag.recordingSegmentID)
        let segmentID = try reader.readIdentifier(ScientificRecordingSegmentID.init)
        try reader.expectField(FieldTag.recordingRegime)
        let regime = try Token.decodeRecordingRegime(reader.readToken())
        try reader.expectField(FieldTag.importedExcerptCoverage)
        let coverage = try Token.decodeImportedExcerptCoverage(reader.readToken())
        try reader.expectField(FieldTag.observationBounds)
        let bounds = try Token.decodeObservationBounds(reader.readToken())
        let segment = ConfirmedRecordingSegment(
            semanticID: segmentID, regime: regime,
            importedExcerptCoverage: coverage, observationBounds: bounds
        )

        try reader.expectField(FieldTag.eventScopeGroups)
        let groupCount = try reader.readCount()
        var groups: [ReceiptEventScopeGroup] = []
        groups.reserveCapacity(groupCount)
        for _ in 0..<groupCount {
            groups.append(try decodeGroup(&reader))
        }
        try requireCanonicalGroupOrder(groups)

        try reader.expectField(FieldTag.attributeDefinitions)
        let attributeCount = try reader.readCount()
        var attributes: [ReceiptAttributeDefinition] = []
        attributes.reserveCapacity(attributeCount)
        for _ in 0..<attributeCount {
            attributes.append(try decodeAttribute(&reader))
        }
        try requireCanonicalAttributeOrder(attributes)

        // The record digest covers exactly the body bytes read so far.
        let bodyDigest = reader.digestOfConsumedBytes()
        try reader.expectField(FieldTag.recordDigest)
        let storedDigest = try reader.readToken()
        guard storedDigest == bodyDigest else {
            throw PersistedReceiptDecodingError.recordDigestMismatch
        }
        try reader.requireEnd()

        let receipt = ConfirmedScientificImportReceipt(
            canonicalSchemaContractID: canonicalSchemaID,
            canonicalSchemaContractDigest: canonicalSchemaDigest,
            canonicalDatasetDigest: canonicalDatasetDigest,
            sourceTransactionBinding: binding,
            headerRule: headerRule,
            sourceTimeUnit: sourceTimeUnit,
            activityMode: activityMode,
            recordingSegment: segment,
            eventScopeGroups: groups,
            attributeDefinitions: attributes,
            confirmationRecordDigest: storedDigest
        )
        // Reject metadata graphs the resolver/validator could never have produced.
        try checkReceiptGraph(receipt)
        return receipt
    }

    // MARK: Bounded filesystem read (untrusted — never mints persistence standing)

    /// Reads and decodes a receipt from a regular file through the real filesystem-bounded reader,
    /// returning an UNTRUSTED bare receipt. It carries no proof and cannot, by itself, remove any
    /// readiness blocker or mint `PersistedConfirmedScientificImportManifest`; only
    /// `ConfirmedScientificImportManifestStore`, reading its own derived final record after every
    /// durability and readback barrier, can do that. The reader opens and pins an
    /// `O_NOFOLLOW | O_NONBLOCK` descriptor (so a substituted FIFO/device returns immediately instead of
    /// blocking the open), refuses symlinks and non-regular files, reads in bounded chunks stopping at
    /// `maximumFileByteCount + 1`, and never calls unbounded `Data(contentsOf:)`.
    public static func readUntrusted(fileURL url: URL) throws -> ConfirmedScientificImportReceipt {
        let bytes = try boundedReadRegularFile(at: url)
        do {
            return try decode(bytes)
        } catch let error as PersistedReceiptDecodingError {
            throw PersistedStoreReadError.decode(error)
        }
    }

    /// Bounded regular-file read: pins an `O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC` descriptor (so a
    /// substituted FIFO or device special file returns immediately rather than blocking the open),
    /// verifies `S_IFREG`, and reads at most `maximumFileByteCount + 1` bytes so an oversized file is
    /// rejected rather than slurped. Internal so the colocated store can read its own derived final path;
    /// not part of the public API.
    static func boundedReadRegularFile(at url: URL) throws -> [UInt8] {
        let descriptor = url.withUnsafeFileSystemRepresentation { path -> Int32 in
            guard let path else { return -1 }
            return open(path, O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC)
        }
        guard descriptor >= 0 else {
            switch errno {
            case ELOOP: throw PersistedStoreReadError.notRegularFile
            case ENOENT: throw PersistedStoreReadError.notFound
            default: throw PersistedStoreReadError.cannotOpen
            }
        }
        defer { close(descriptor) }

        var status = stat()
        guard fstat(descriptor, &status) == 0 else { throw PersistedStoreReadError.cannotOpen }
        guard (status.st_mode & S_IFMT) == S_IFREG else { throw PersistedStoreReadError.notRegularFile }

        let limit = maximumFileByteCount
        var buffer: [UInt8] = []
        var chunk = [UInt8](repeating: 0, count: 64 * 1_024)
        while buffer.count <= limit {
            let want = Swift.min(chunk.count, (limit + 1) - buffer.count)
            let readCount = chunk.withUnsafeMutableBytes { raw -> Int in
                read(descriptor, raw.baseAddress, want)
            }
            if readCount < 0 { throw PersistedStoreReadError.ioError }
            if readCount == 0 { break }
            buffer.append(contentsOf: chunk[0..<readCount])
        }
        guard buffer.count <= limit else {
            throw PersistedStoreReadError.fileTooLarge(maximumBytes: limit)
        }
        return buffer
    }

    private static func decodeSelection(
        _ reader: inout ReceiptByteReader
    ) throws -> StagedSourceTransactionSelection {
        let token = try reader.readToken()
        switch token {
        case Token.selectionCSV:
            return .commaSeparatedValues
        case Token.selectionExcelWorksheet:
            try reader.expectField(FieldTag.worksheetName)
            let name = try reader.readToken()
            try reader.expectField(FieldTag.worksheetSheetID)
            let sheetIDRaw = try reader.readInt64()
            guard sheetIDRaw >= 1, sheetIDRaw <= Int64(UInt32.max) else {
                throw PersistedReceiptDecodingError.invalidWorksheetSheetID
            }
            try reader.expectField(FieldTag.worksheetRelationshipID)
            let relationshipID = try reader.readToken()
            try reader.expectField(FieldTag.worksheetPartPath)
            let partPath = try reader.readToken()
            return .excelWorksheet(
                worksheetName: name,
                sheetID: UInt32(sheetIDRaw),
                relationshipID: relationshipID,
                normalizedPartPath: partPath
            )
        default:
            throw PersistedReceiptDecodingError.unknownToken(token)
        }
    }

    private static func decodeGroup(
        _ reader: inout ReceiptByteReader
    ) throws -> ReceiptEventScopeGroup {
        try reader.expectField(FieldTag.groupID)
        let groupID = try reader.readIdentifier(ScientificEventScopeGroupID.init)
        try reader.expectField(FieldTag.groupTimeBasis)
        let timeBasis = try decodeTimeBasis(&reader)

        try reader.expectField(FieldTag.spikeTrainColumns)
        let spikeCount = try reader.readCount()
        var spikes: [ReceiptSpikeTrainColumn] = []
        spikes.reserveCapacity(spikeCount)
        for _ in 0..<spikeCount {
            try reader.expectField(FieldTag.spikeSourceColumn)
            let column = try reader.readSourceColumnIndex()
            try reader.expectField(FieldTag.spikeTrainID)
            let id = try reader.readIdentifier(ScientificSpikeTrainID.init)
            try reader.expectField(FieldTag.spikeOrderDecision)
            let order = try Token.decodeOrderDecision(reader.readToken())
            try reader.expectField(FieldTag.spikeDuplicateDecision)
            let duplicate = try Token.decodeDuplicateDecision(reader.readToken())
            spikes.append(ReceiptSpikeTrainColumn(
                sourceColumnOneBasedIndex: column, semanticID: id,
                orderDecision: order, duplicateDecision: duplicate
            ))
        }
        try requireStrictlyAscendingColumns(spikes.map(\.sourceColumnOneBasedIndex))

        try reader.expectField(FieldTag.eventDefinitionColumns)
        let eventCount = try reader.readCount()
        var events: [ReceiptEventDefinitionColumn] = []
        events.reserveCapacity(eventCount)
        for _ in 0..<eventCount {
            try reader.expectField(FieldTag.eventSourceColumn)
            let column = try reader.readSourceColumnIndex()
            try reader.expectField(FieldTag.eventDefinitionID)
            let id = try reader.readIdentifier(ScientificEventDefinitionID.init)
            try reader.expectField(FieldTag.eventTypeID)
            let typeID = try reader.readIdentifier(ScientificEventTypeID.init)
            try reader.expectField(FieldTag.eventOrderDecision)
            let order = try Token.decodeOrderDecision(reader.readToken())
            events.append(ReceiptEventDefinitionColumn(
                sourceColumnOneBasedIndex: column, semanticID: id,
                eventTypeID: typeID, orderDecision: order
            ))
        }
        try requireStrictlyAscendingColumns(events.map(\.sourceColumnOneBasedIndex))

        return ReceiptEventScopeGroup(
            semanticID: groupID, timeBasis: timeBasis,
            spikeTrains: spikes, eventDefinitions: events
        )
    }

    private static func decodeTimeBasis(
        _ reader: inout ReceiptByteReader
    ) throws -> ReceiptEventScopeTimeBasis {
        let token = try reader.readToken()
        switch token {
        case Token.timeBasisRecordingElapsed:
            return .recordingElapsed
        case Token.timeBasisEventRelative:
            try reader.expectField(FieldTag.originColumn)
            let column = try reader.readSourceColumnIndex()
            try reader.expectField(FieldTag.originDataRow)
            let dataRowRaw = try reader.readInt64()
            guard dataRowRaw >= 1, dataRowRaw <= Int64(Int.max) else {
                throw PersistedReceiptDecodingError.invalidSourceColumnIndex
            }
            return .eventRelative(
                originColumnOneBasedIndex: column,
                originDataRowOneBasedIndex: Int(dataRowRaw)
            )
        default:
            throw PersistedReceiptDecodingError.unknownToken(token)
        }
    }

    private static func decodeAttribute(
        _ reader: inout ReceiptByteReader
    ) throws -> ReceiptAttributeDefinition {
        try reader.expectField(FieldTag.attributeKey)
        let key = try reader.readAttributeKey()
        try reader.expectField(FieldTag.attributeScalarType)
        let scalarType = try Token.decodeScalarType(reader.readToken())
        try reader.expectField(FieldTag.attributeRole)
        let role = try Token.decodeRole(reader.readToken())
        try reader.expectField(FieldTag.attributeUnit)
        let unit = try decodeUnit(&reader)
        try reader.expectField(FieldTag.attributeEmptyStringPolicy)
        let emptyStringPolicy = try Token.decodeEmptyStringPolicy(reader.readToken())
        return ReceiptAttributeDefinition(
            key: key, scalarType: scalarType, role: role,
            unit: unit, emptyStringPolicy: emptyStringPolicy
        )
    }

    private static func decodeUnit(
        _ reader: inout ReceiptByteReader
    ) throws -> ConfirmedEventAttributeUnit {
        let token = try reader.readToken()
        switch token {
        case Token.unitNotApplicable:
            return .notApplicable
        case Token.unitDimensionless:
            return .dimensionless
        case Token.unitSpecified:
            try reader.expectField(FieldTag.unitSymbol)
            let text = try reader.readToken()
            do {
                return .specified(try OpaqueUnitSymbol(validating: text))
            } catch {
                throw PersistedReceiptDecodingError.invalidIdentifier
            }
        default:
            throw PersistedReceiptDecodingError.unknownToken(token)
        }
    }

    // MARK: Canonical-order checks

    private static func requireCanonicalGroupOrder(_ groups: [ReceiptEventScopeGroup]) throws {
        let keys = groups.map { $0.spikeTrains.first?.sourceColumnOneBasedIndex ?? Int.max }
        try requireStrictlyAscendingColumns(keys)
    }

    private static func requireCanonicalAttributeOrder(_ attributes: [ReceiptAttributeDefinition]) throws {
        for index in 1..<max(attributes.count, 1) where index < attributes.count {
            let previous = attributes[index - 1].key.canonicalText
            let current = attributes[index].key.canonicalText
            guard previous.utf8.lexicographicallyPrecedes(current.utf8) else {
                throw PersistedReceiptDecodingError.noncanonicalCollectionOrder
            }
        }
    }

    private static func requireStrictlyAscendingColumns(_ columns: [Int]) throws {
        for index in 1..<max(columns.count, 1) where index < columns.count {
            guard columns[index - 1] < columns[index] else {
                throw PersistedReceiptDecodingError.noncanonicalCollectionOrder
            }
        }
    }

    // MARK: Shared receipt-graph invariants

    /// The single, shared, pure invariant checker for a decoded/projected receipt graph. It rejects
    /// metadata graphs the resolver and independent validator could never have produced. Projection
    /// (debug assertion), decoding (hard rejection), and replay (through the resolver/validator) all
    /// rely on it so they cannot drift. It does not weaken the resolver or validator.
    static func checkReceiptGraph(_ receipt: ConfirmedScientificImportReceipt) throws {
        let groups = receipt.eventScopeGroups
        guard !groups.isEmpty else {
            throw PersistedReceiptDecodingError.emptyGroupCollection
        }

        // Headerless input obeys the resolver's headerless topology: exactly one group, no event
        // definitions, and a recording-elapsed time basis.
        if receipt.headerRule == .headerless {
            guard groups.count == 1 else {
                throw PersistedReceiptDecodingError.headerlessTopologyInvalid
            }
            for group in groups {
                guard group.eventDefinitions.isEmpty else {
                    throw PersistedReceiptDecodingError.headerlessTopologyInvalid
                }
                guard case .recordingElapsed = group.timeBasis else {
                    throw PersistedReceiptDecodingError.headerlessTopologyInvalid
                }
            }
        }

        var seenColumns = Set<Int>()
        var seenGroupIDs = Set<String>()
        // Spike-train identity is unique across the whole dataset.
        var seenSpikeIDs = Set<String>()
        // Flattened source columns in group order, spike-then-event within a group.
        var flattenedColumns: [Int] = []

        for group in groups {
            guard seenGroupIDs.insert(group.semanticID.semanticID.canonicalText).inserted else {
                throw PersistedReceiptDecodingError.duplicateSemanticIdentity
            }
            guard !group.spikeTrains.isEmpty else {
                throw PersistedReceiptDecodingError.groupWithoutSpikeTrain
            }

            for train in group.spikeTrains {
                flattenedColumns.append(train.sourceColumnOneBasedIndex)
                guard seenColumns.insert(train.sourceColumnOneBasedIndex).inserted else {
                    throw PersistedReceiptDecodingError.duplicateSourceColumn
                }
                guard seenSpikeIDs.insert(train.semanticID.semanticID.canonicalText).inserted else {
                    throw PersistedReceiptDecodingError.duplicateSemanticIdentity
                }
                // `.collapseExact` is a future Single-unit analysis-view request only.
                if train.duplicateDecision == .collapseExact, receipt.activityMode != .putativeSingleUnit {
                    throw PersistedReceiptDecodingError.collapseNotAllowedForActivityMode
                }
            }

            // Event-definition identity is unique only WITHIN its own group; two groups may reuse an ID.
            var seenEventDefinitionIDsInGroup = Set<String>()
            for definition in group.eventDefinitions {
                flattenedColumns.append(definition.sourceColumnOneBasedIndex)
                guard seenColumns.insert(definition.sourceColumnOneBasedIndex).inserted else {
                    throw PersistedReceiptDecodingError.duplicateSourceColumn
                }
                guard seenEventDefinitionIDsInGroup.insert(definition.semanticID.semanticID.canonicalText).inserted else {
                    throw PersistedReceiptDecodingError.duplicateSemanticIdentity
                }
            }

            // An event-relative origin must reference an event-definition column in the same group.
            if case .eventRelative(let originColumn, _) = group.timeBasis {
                let eventColumns = Set(group.eventDefinitions.map(\.sourceColumnOneBasedIndex))
                guard eventColumns.contains(originColumn) else {
                    throw PersistedReceiptDecodingError.eventRelativeOriginNotInGroup
                }
            }
        }

        // The flattened columns must form exactly one complete, non-overlapping, contiguous partition
        // 1...N in group-then-spike-then-event order (no gaps, duplicates, interleaving, or unassigned
        // columns). Duplicates are already rejected above, so this catches gaps and interleaving.
        for (position, column) in flattenedColumns.enumerated() where column != position + 1 {
            throw PersistedReceiptDecodingError.invalidGroupPartition
        }

        var seenAttributeKeys = Set<String>()
        for attribute in receipt.attributeDefinitions {
            guard seenAttributeKeys.insert(attribute.key.canonicalText).inserted else {
                throw PersistedReceiptDecodingError.duplicateSemanticIdentity
            }
            // `allowExplicitEmptyString` is only meaningful for string-typed attributes.
            if attribute.emptyStringPolicy == .allowExplicitEmptyString, attribute.scalarType != .string {
                throw PersistedReceiptDecodingError.invalidAttributeCombination
            }
        }
    }

    // MARK: Draft reconstruction (for replay after restart)

    /// Rebuilds a complete manifest draft from a decoded receipt, bound to a freshly reselected and
    /// staged source. The draft is fed into exactly one normal import chain; it is never a shortcut
    /// around validation. Structural mismatches (missing columns, changed shape) fail closed in the
    /// resolver/validator downstream.
    public static func reconstructDraft(
        from receipt: ConfirmedScientificImportReceipt,
        boundTo stagedImport: StagedScientificImport
    ) throws -> ScientificImportManifestDraft {
        let groups = try receipt.eventScopeGroups.map { group -> EventScopeGroupManifestDraft in
            let spikes = try group.spikeTrains.map { train in
                SpikeTrainColumnManifestDraft(
                    sourceColumn: try StagedSourceColumnReference(oneBasedIndex: train.sourceColumnOneBasedIndex),
                    semanticID: train.semanticID,
                    orderDecision: train.orderDecision,
                    duplicateDecision: train.duplicateDecision
                )
            }
            let events = try group.eventDefinitions.map { definition in
                EventDefinitionColumnManifestDraft(
                    sourceColumn: try StagedSourceColumnReference(oneBasedIndex: definition.sourceColumnOneBasedIndex),
                    semanticID: definition.semanticID,
                    eventTypeID: definition.eventTypeID,
                    orderDecision: definition.orderDecision
                )
            }
            let timeBasis: EventScopeTimeBasisDraft
            switch group.timeBasis {
            case .recordingElapsed:
                timeBasis = .recordingElapsed
            case .eventRelative(let column, let dataRow):
                let cell = try StagedSourceCellReference(
                    column: try StagedSourceColumnReference(oneBasedIndex: column),
                    oneBasedDataRowIndex: dataRow
                )
                timeBasis = .eventRelative(origin: StagedEventOccurrenceReference(timestampCell: cell))
            }
            return EventScopeGroupManifestDraft(
                semanticID: group.semanticID,
                spikeTrains: spikes,
                eventDefinitions: events,
                timeBasis: timeBasis
            )
        }
        let attributes = receipt.attributeDefinitions.map { attribute in
            EventAttributeDefinitionDraft(
                key: attribute.key,
                scalarType: attribute.scalarType,
                role: attribute.role,
                unit: attribute.unit,
                emptyStringPolicy: attribute.emptyStringPolicy
            )
        }
        return ScientificImportManifestDraft(
            boundTo: stagedImport,
            sourceTimeUnit: receipt.sourceTimeUnit,
            activityMode: receipt.activityMode,
            recordingSegmentID: receipt.recordingSegment.semanticID,
            recordingRegime: receipt.recordingSegment.regime,
            importedExcerptCoverage: receipt.recordingSegment.importedExcerptCoverage,
            observationBoundsAvailability: receipt.recordingSegment.observationBounds,
            eventScopeGroups: groups,
            eventAttributeDefinitions: attributes
        )
    }
}

// MARK: - Bounded byte reader

/// A strict, bounded, forward-only reader over receipt bytes. Every read is length- and range-checked
/// before it touches memory; it never seeks backwards and never over-reads.
private struct ReceiptByteReader {
    private let bytes: [UInt8]
    private var index = 0

    init(_ bytes: [UInt8]) {
        self.bytes = bytes
    }

    private mutating func readByte() throws -> UInt8 {
        guard index < bytes.count else { throw PersistedReceiptDecodingError.truncated }
        defer { index += 1 }
        return bytes[index]
    }

    mutating func expectField(_ tag: UInt8) throws {
        let found = try readByte()
        guard found == tag else {
            throw PersistedReceiptDecodingError.unexpectedFieldTag(expected: tag, found: found)
        }
    }

    mutating func readInt64() throws -> Int64 {
        guard bytes.count - index >= 8 else { throw PersistedReceiptDecodingError.truncated }
        var value: UInt64 = 0
        for _ in 0..<8 {
            value = (value << 8) | UInt64(bytes[index])
            index += 1
        }
        return Int64(bitPattern: value)
    }

    mutating func readCount() throws -> Int {
        let raw = try readInt64()
        guard raw >= 0, raw <= Int64(ConfirmedScientificImportManifestPersistence.maximumCollectionCount) else {
            throw PersistedReceiptDecodingError.countOutOfRange
        }
        return Int(raw)
    }

    mutating func readToken() throws -> String {
        let rawLength = try readInt64()
        guard rawLength >= 0,
              rawLength <= Int64(ConfirmedScientificImportManifestPersistence.maximumTokenByteCount),
              rawLength <= Int64(bytes.count - index) else {
            throw PersistedReceiptDecodingError.tokenLengthOutOfRange
        }
        let length = Int(rawLength)
        let slice = bytes[index..<(index + length)]
        index += length
        guard let string = String(bytes: slice, encoding: .utf8) else {
            throw PersistedReceiptDecodingError.invalidUTF8
        }
        return string
    }

    mutating func expectDomain(_ domain: String) throws {
        guard try readToken() == domain else {
            throw PersistedReceiptDecodingError.unknownToken(domain)
        }
    }

    mutating func readSourceColumnIndex() throws -> Int {
        let raw = try readInt64()
        guard raw >= 1, raw <= Int64(Int.max) else {
            throw PersistedReceiptDecodingError.invalidSourceColumnIndex
        }
        return Int(raw)
    }

    mutating func readIdentifier<T>(_ wrap: (ScientificSemanticID) -> T) throws -> T {
        let text = try readToken()
        do {
            return wrap(try ScientificSemanticID(validating: text))
        } catch {
            throw PersistedReceiptDecodingError.invalidIdentifier
        }
    }

    mutating func readAttributeKey() throws -> EventAttributeKey {
        let text = try readToken()
        do {
            return try EventAttributeKey(validating: text)
        } catch {
            throw PersistedReceiptDecodingError.invalidIdentifier
        }
    }

    /// The digest of every byte consumed so far — used to verify the trailing record digest against
    /// exactly the body bytes.
    func digestOfConsumedBytes() -> String {
        let body = bytes[0..<index]
        return SHA256.hash(data: Data(body)).map { String(format: "%02x", $0) }.joined()
    }

    mutating func requireEnd() throws {
        guard index == bytes.count else { throw PersistedReceiptDecodingError.trailingBytes }
    }
}

// MARK: - Streaming sinks (reuse the shared CanonicalTranscriptSink primitives)

/// Production digest sink: feeds bytes straight to an incremental SHA-256.
private struct ReceiptDigestSHA256Sink: CanonicalTranscriptSink {
    private var hasher = SHA256()
    private(set) var transcriptByteCount = 0

    mutating func write(_ buffer: UnsafeRawBufferPointer) {
        hasher.update(bufferPointer: buffer)
        transcriptByteCount += buffer.count
    }

    mutating func finalizeHex() -> String {
        hasher.finalize().map { String(format: "%02x", $0) }.joined()
    }
}

/// Byte-collecting sink for the file bytes and the deterministic byte-transcript oracle.
private struct ReceiptByteCollectingSink: CanonicalTranscriptSink {
    private(set) var bytes: [UInt8] = []
    private(set) var transcriptByteCount = 0

    mutating func write(_ buffer: UnsafeRawBufferPointer) {
        bytes.append(contentsOf: buffer)
        transcriptByteCount += buffer.count
    }
}

// MARK: - Domain separation, field tags, and token vocabulary

extension ConfirmedScientificImportManifestPersistence {
    fileprivate enum Domain {
        static let record = "stpd.confirmed_scientific_import_manifest_receipt"
        static let schemaContract = "stpd.confirmed_scientific_import_manifest_receipt.schema_contract"
    }

    fileprivate enum ContractSection {
        static let identity: UInt8 = 0x01
        static let domains: UInt8 = 0x02
        static let primitives: UInt8 = 0x03
        static let limits: UInt8 = 0x04
        static let fieldTags: UInt8 = 0x05
        static let tokens: UInt8 = 0x06
        static let traversal: UInt8 = 0x07
        static let structure: UInt8 = 0x08
    }

    fileprivate enum FieldTag {
        static let persistenceSchemaID: UInt8 = 0x01
        static let persistenceSchemaDigest: UInt8 = 0x02
        static let canonicalSchemaID: UInt8 = 0x03
        static let canonicalSchemaDigest: UInt8 = 0x04
        static let canonicalDatasetDigest: UInt8 = 0x05
        static let sourceSHA: UInt8 = 0x06
        static let sourceSelection: UInt8 = 0x07
        static let worksheetName: UInt8 = 0x08
        static let worksheetSheetID: UInt8 = 0x09
        static let worksheetRelationshipID: UInt8 = 0x0A
        static let worksheetPartPath: UInt8 = 0x0B
        static let headerRule: UInt8 = 0x0C
        static let sourceTimeUnit: UInt8 = 0x0D
        static let activityMode: UInt8 = 0x0E
        static let recordingSegmentID: UInt8 = 0x0F
        static let recordingRegime: UInt8 = 0x10
        static let importedExcerptCoverage: UInt8 = 0x11
        static let observationBounds: UInt8 = 0x12
        static let eventScopeGroups: UInt8 = 0x20
        static let groupID: UInt8 = 0x21
        static let groupTimeBasis: UInt8 = 0x22
        static let originColumn: UInt8 = 0x23
        static let originDataRow: UInt8 = 0x24
        static let spikeTrainColumns: UInt8 = 0x25
        static let spikeSourceColumn: UInt8 = 0x26
        static let spikeTrainID: UInt8 = 0x27
        static let spikeOrderDecision: UInt8 = 0x28
        static let spikeDuplicateDecision: UInt8 = 0x29
        static let eventDefinitionColumns: UInt8 = 0x2A
        static let eventSourceColumn: UInt8 = 0x2B
        static let eventDefinitionID: UInt8 = 0x2C
        static let eventTypeID: UInt8 = 0x2D
        static let eventOrderDecision: UInt8 = 0x2E
        static let attributeDefinitions: UInt8 = 0x30
        static let attributeKey: UInt8 = 0x31
        static let attributeScalarType: UInt8 = 0x32
        static let attributeRole: UInt8 = 0x33
        static let attributeUnit: UInt8 = 0x34
        static let unitSymbol: UInt8 = 0x35
        static let attributeEmptyStringPolicy: UInt8 = 0x36
        static let recordDigest: UInt8 = 0x40
    }

    fileprivate enum Token {
        static let selectionCSV = "source_selection.comma_separated_values"
        static let selectionExcelWorksheet = "source_selection.excel_worksheet"
        static let timeBasisRecordingElapsed = "time_basis.recording_elapsed"
        static let timeBasisEventRelative = "time_basis.event_relative"
        static let unitNotApplicable = "unit.not_applicable"
        static let unitDimensionless = "unit.dimensionless"
        static let unitSpecified = "unit.specified"

        static func headerRule(_ rule: PersistedTabularHeaderRule) -> String {
            switch rule {
            case .firstRecordIsHeader: return "header_rule.first_record_is_header"
            case .headerless: return "header_rule.headerless"
            }
        }

        static func decodeHeaderRule(_ token: String) throws -> PersistedTabularHeaderRule {
            switch token {
            case "header_rule.first_record_is_header": return .firstRecordIsHeader
            case "header_rule.headerless": return .headerless
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func sourceTimeUnit(_ unit: SpikeTimeUnit) -> String {
            switch unit {
            case .seconds: return "source_time_unit.seconds"
            case .milliseconds: return "source_time_unit.milliseconds"
            }
        }

        static func decodeSourceTimeUnit(_ token: String) throws -> SpikeTimeUnit {
            switch token {
            case "source_time_unit.seconds": return .seconds
            case "source_time_unit.milliseconds": return .milliseconds
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func activityMode(_ mode: ScientificDatasetActivityMode) -> String {
            switch mode {
            case .putativeSingleUnit: return "activity_mode.putative_single_unit"
            case .intentionalMultiUnit: return "activity_mode.intentional_multi_unit"
            case .unknownOrUncertain: return "activity_mode.unknown_or_uncertain"
            }
        }

        static func decodeActivityMode(_ token: String) throws -> ScientificDatasetActivityMode {
            switch token {
            case "activity_mode.putative_single_unit": return .putativeSingleUnit
            case "activity_mode.intentional_multi_unit": return .intentionalMultiUnit
            case "activity_mode.unknown_or_uncertain": return .unknownOrUncertain
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func recordingRegime(_ regime: ScientificRecordingRegime) -> String {
            switch regime {
            case .continuousUntrialed: return "recording_regime.continuous_untrialed"
            case .trialized: return "recording_regime.trialized"
            case .unknownOrUncertain: return "recording_regime.unknown_or_uncertain"
            }
        }

        static func decodeRecordingRegime(_ token: String) throws -> ScientificRecordingRegime {
            switch token {
            case "recording_regime.continuous_untrialed": return .continuousUntrialed
            case "recording_regime.trialized": return .trialized
            case "recording_regime.unknown_or_uncertain": return .unknownOrUncertain
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func importedExcerptCoverage(_ coverage: ImportedExcerptCoverage) -> String {
            switch coverage {
            case .allSpikeTrainsFullImportedExcerpt:
                return "imported_excerpt_coverage.all_spike_trains_full_imported_excerpt"
            case .notAllSpikeTrainsFullImportedExcerpt:
                return "imported_excerpt_coverage.not_all_spike_trains_full_imported_excerpt"
            case .unknownOrUncertain:
                return "imported_excerpt_coverage.unknown_or_uncertain"
            }
        }

        static func decodeImportedExcerptCoverage(_ token: String) throws -> ImportedExcerptCoverage {
            switch token {
            case "imported_excerpt_coverage.all_spike_trains_full_imported_excerpt":
                return .allSpikeTrainsFullImportedExcerpt
            case "imported_excerpt_coverage.not_all_spike_trains_full_imported_excerpt":
                return .notAllSpikeTrainsFullImportedExcerpt
            case "imported_excerpt_coverage.unknown_or_uncertain":
                return .unknownOrUncertain
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func observationBounds(_ bounds: ObservationBoundsAvailability) -> String {
            switch bounds {
            case .unknownOrUnavailable: return "observation_bounds.unknown_or_unavailable"
            }
        }

        static func decodeObservationBounds(_ token: String) throws -> ObservationBoundsAvailability {
            switch token {
            case "observation_bounds.unknown_or_unavailable": return .unknownOrUnavailable
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func orderDecision(_ decision: TimestampOrderDecision) -> String {
            switch decision {
            case .preserveSourceOrder: return "order.preserve_source_order"
            case .stableAscendingSort: return "order.stable_ascending_sort"
            }
        }

        static func decodeOrderDecision(_ token: String) throws -> TimestampOrderDecision {
            switch token {
            case "order.preserve_source_order": return .preserveSourceOrder
            case "order.stable_ascending_sort": return .stableAscendingSort
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func duplicateDecision(_ decision: ExactDuplicateDecision) -> String {
            switch decision {
            case .preserveMultiplicity: return "duplicate.preserve_multiplicity"
            case .collapseExact: return "duplicate.collapse_exact"
            }
        }

        static func decodeDuplicateDecision(_ token: String) throws -> ExactDuplicateDecision {
            switch token {
            case "duplicate.preserve_multiplicity": return .preserveMultiplicity
            case "duplicate.collapse_exact": return .collapseExact
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func scalarType(_ type: EventAttributeScalarType) -> String {
            switch type {
            case .string: return "scalar.string"
            case .integer: return "scalar.integer"
            case .exactDecimal: return "scalar.exact_decimal"
            case .boolean: return "scalar.boolean"
            }
        }

        static func decodeScalarType(_ token: String) throws -> EventAttributeScalarType {
            switch token {
            case "scalar.string": return .string
            case "scalar.integer": return .integer
            case "scalar.exact_decimal": return .exactDecimal
            case "scalar.boolean": return .boolean
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func role(_ role: EventAttributeRole) -> String {
            switch role {
            case .scientific: return "role.scientific"
            case .presentation: return "role.presentation"
            }
        }

        static func decodeRole(_ token: String) throws -> EventAttributeRole {
            switch token {
            case "role.scientific": return .scientific
            case "role.presentation": return .presentation
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }

        static func emptyStringPolicy(_ policy: EventEmptyStringPolicy) -> String {
            switch policy {
            case .forbid: return "empty_string.forbid"
            case .allowExplicitEmptyString: return "empty_string.allow_explicit_empty_string"
            }
        }

        static func decodeEmptyStringPolicy(_ token: String) throws -> EventEmptyStringPolicy {
            switch token {
            case "empty_string.forbid": return .forbid
            case "empty_string.allow_explicit_empty_string": return .allowExplicitEmptyString
            default: throw PersistedReceiptDecodingError.unknownToken(token)
            }
        }
    }
}

// MARK: - Schema-contract transcript (binds the real codec)

extension ConfirmedScientificImportManifestPersistence {
    fileprivate static func encodeSchemaContract(into sink: inout some CanonicalTranscriptSink) {
        sink.feedToken(Domain.schemaContract)
        sink.feedField(ContractSection.identity)
        sink.feedToken(schemaContractID)

        sink.feedField(ContractSection.domains)
        sink.feedCount(2)
        sink.feedToken(Domain.record)
        sink.feedToken(Domain.schemaContract)

        sink.feedField(ContractSection.primitives)
        sink.feedCount(primitiveRules.count)
        for rule in primitiveRules { sink.feedToken(rule) }

        sink.feedField(ContractSection.limits)
        sink.feedCount(3)
        sink.feedToken("max_file_bytes")
        sink.feedInt64(Int64(maximumFileByteCount))
        sink.feedToken("max_token_bytes")
        sink.feedInt64(Int64(maximumTokenByteCount))
        sink.feedToken("max_collection_count")
        sink.feedInt64(Int64(maximumCollectionCount))

        sink.feedField(ContractSection.fieldTags)
        sink.feedCount(fieldTagBindings.count)
        for binding in fieldTagBindings {
            sink.feedToken(binding.name)
            sink.feedByte(binding.value)
        }

        sink.feedField(ContractSection.tokens)
        sink.feedCount(tokenVocabulary.count)
        for token in tokenVocabulary { sink.feedToken(token) }

        sink.feedField(ContractSection.traversal)
        sink.feedCount(traversalGrammar.count)
        for step in traversalGrammar { sink.feedToken(step) }

        sink.feedField(ContractSection.structure)
        sink.feedCount(structuralFacts.count)
        for fact in structuralFacts { sink.feedToken(fact) }
    }

    private static let primitiveRules = [
        "hash=sha256",
        "field_tag=one_byte",
        "int64=big_endian_signed_two_complement_8_bytes",
        "collection_count=int64",
        "source_column_index=int64_one_based",
        "string=int64_length_prefix_then_utf8",
        "record_digest=sha256_over_domain_separated_body",
    ]

    private static let fieldTagBindings: [(name: String, value: UInt8)] = [
        ("persistence_schema_id", FieldTag.persistenceSchemaID),
        ("persistence_schema_digest", FieldTag.persistenceSchemaDigest),
        ("canonical_schema_id", FieldTag.canonicalSchemaID),
        ("canonical_schema_digest", FieldTag.canonicalSchemaDigest),
        ("canonical_dataset_digest", FieldTag.canonicalDatasetDigest),
        ("source_sha", FieldTag.sourceSHA),
        ("source_selection", FieldTag.sourceSelection),
        ("worksheet_name", FieldTag.worksheetName),
        ("worksheet_sheet_id", FieldTag.worksheetSheetID),
        ("worksheet_relationship_id", FieldTag.worksheetRelationshipID),
        ("worksheet_part_path", FieldTag.worksheetPartPath),
        ("header_rule", FieldTag.headerRule),
        ("source_time_unit", FieldTag.sourceTimeUnit),
        ("activity_mode", FieldTag.activityMode),
        ("recording_segment_id", FieldTag.recordingSegmentID),
        ("recording_regime", FieldTag.recordingRegime),
        ("imported_excerpt_coverage", FieldTag.importedExcerptCoverage),
        ("observation_bounds", FieldTag.observationBounds),
        ("event_scope_groups", FieldTag.eventScopeGroups),
        ("group_id", FieldTag.groupID),
        ("group_time_basis", FieldTag.groupTimeBasis),
        ("origin_column", FieldTag.originColumn),
        ("origin_data_row", FieldTag.originDataRow),
        ("spike_train_columns", FieldTag.spikeTrainColumns),
        ("spike_source_column", FieldTag.spikeSourceColumn),
        ("spike_train_id", FieldTag.spikeTrainID),
        ("spike_order_decision", FieldTag.spikeOrderDecision),
        ("spike_duplicate_decision", FieldTag.spikeDuplicateDecision),
        ("event_definition_columns", FieldTag.eventDefinitionColumns),
        ("event_source_column", FieldTag.eventSourceColumn),
        ("event_definition_id", FieldTag.eventDefinitionID),
        ("event_type_id", FieldTag.eventTypeID),
        ("event_order_decision", FieldTag.eventOrderDecision),
        ("attribute_definitions", FieldTag.attributeDefinitions),
        ("attribute_key", FieldTag.attributeKey),
        ("attribute_scalar_type", FieldTag.attributeScalarType),
        ("attribute_role", FieldTag.attributeRole),
        ("attribute_unit", FieldTag.attributeUnit),
        ("unit_symbol", FieldTag.unitSymbol),
        ("attribute_empty_string_policy", FieldTag.attributeEmptyStringPolicy),
        ("record_digest", FieldTag.recordDigest),
    ]

    private static let tokenVocabulary = [
        Token.selectionCSV,
        Token.selectionExcelWorksheet,
        Token.headerRule(.firstRecordIsHeader),
        Token.headerRule(.headerless),
        Token.sourceTimeUnit(.seconds),
        Token.sourceTimeUnit(.milliseconds),
        Token.activityMode(.putativeSingleUnit),
        Token.activityMode(.intentionalMultiUnit),
        Token.activityMode(.unknownOrUncertain),
        Token.recordingRegime(.continuousUntrialed),
        Token.recordingRegime(.trialized),
        Token.recordingRegime(.unknownOrUncertain),
        Token.importedExcerptCoverage(.allSpikeTrainsFullImportedExcerpt),
        Token.importedExcerptCoverage(.notAllSpikeTrainsFullImportedExcerpt),
        Token.importedExcerptCoverage(.unknownOrUncertain),
        Token.observationBounds(.unknownOrUnavailable),
        Token.timeBasisRecordingElapsed,
        Token.timeBasisEventRelative,
        Token.orderDecision(.preserveSourceOrder),
        Token.orderDecision(.stableAscendingSort),
        Token.duplicateDecision(.preserveMultiplicity),
        Token.duplicateDecision(.collapseExact),
        Token.scalarType(.string),
        Token.scalarType(.integer),
        Token.scalarType(.exactDecimal),
        Token.scalarType(.boolean),
        Token.role(.scientific),
        Token.role(.presentation),
        Token.unitNotApplicable,
        Token.unitDimensionless,
        Token.unitSpecified,
        Token.emptyStringPolicy(.forbid),
        Token.emptyStringPolicy(.allowExplicitEmptyString),
    ]

    private static let traversalGrammar = [
        "domain",
        "persistence_schema_id;persistence_schema_digest",
        "canonical_schema_id;canonical_schema_digest;canonical_dataset_digest",
        "source_sha;source_selection(comma_separated_values|excel_worksheet"
            + "{worksheet_name;worksheet_sheet_id;worksheet_relationship_id;worksheet_part_path})",
        "header_rule;source_time_unit;activity_mode",
        "recording_segment_id;recording_regime;imported_excerpt_coverage;observation_bounds",
        "event_scope_groups[count]{group_id;group_time_basis(recording_elapsed|event_relative"
            + "{origin_column;origin_data_row});"
            + "spike_train_columns[count]{spike_source_column;spike_train_id;spike_order_decision;"
            + "spike_duplicate_decision};"
            + "event_definition_columns[count]{event_source_column;event_definition_id;event_type_id;"
            + "event_order_decision}}",
        "attribute_definitions[count]{attribute_key;attribute_scalar_type;attribute_role;"
            + "attribute_unit(not_applicable|dimensionless|specified{unit_symbol});"
            + "attribute_empty_string_policy}",
        "record_digest",
    ]

    private static let structuralFacts = [
        "identity=canonical_fingerprint_triple_is_sole_scientific_identity",
        "record_digest_identifies=source_bound_saved_record_only",
        "source_binding=exact_sha256_and_csv_or_excel_worksheet_selection",
        "header_rule=explicit_first_record_is_header_or_headerless",
        "header_rule=derived_from_sealed_validated_source_header_presence",
        "header_rule_derivation=all_columns_header_present_first_record_is_header;all_columns_header_absent_headerless;empty_or_mixed_header_presence_fail_closed",
        "group_order=ascending_first_spike_source_column",
        "spike_train_order=ascending_source_column",
        "event_definition_order=ascending_source_column",
        "attribute_order=utf8_lexicographic_by_key",
        "event_relative_origin=draft_source_cell_column_and_data_row_never_timestamp",
        "excludes=source_bytes;raw_cells;raw_lexemes;header_text;timestamps;event_values;presentation_values",
        "excludes=prepared_import;canonical_dataset;normalization_provenance;timestamp_source_arrays",
        "excludes=source_path;filename;mtime;inode;security_bookmark;ui_uuid;wall_clock;app_build",
        "encoded_byte_count_is_not_identity_or_authority",
        "scientific_or_presentation_role_change_alters_canonical_fingerprint",
        "presentation_only_definition_change_may_alter_record_digest_not_canonical_fingerprint",
        // Receipt-graph invariants, bound to the schema so any change to the accepted graph rules
        // necessarily changes the schema digest and its goldens.
        "graph.group_collection=non_empty",
        "graph.group_has_spike_train=at_least_one",
        "graph.spike_train_id=dataset_unique",
        "graph.group_id=dataset_unique",
        "graph.event_definition_id=unique_within_group_may_reuse_across_groups",
        "graph.event_relative_origin=references_event_definition_column_in_same_group",
        "graph.source_columns=exactly_one_contiguous_partition_1_to_n_group_then_spike_then_event_order",
        "graph.headerless=single_group_no_event_definitions_recording_elapsed",
        "graph.collapse_exact=permitted_only_for_putative_single_unit",
        "graph.attribute_key=dataset_unique",
        "graph.allow_explicit_empty_string=only_for_string_scalar_type",
    ]
}
