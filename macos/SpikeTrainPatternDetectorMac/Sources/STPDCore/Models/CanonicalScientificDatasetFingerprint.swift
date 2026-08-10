import Foundation
import CryptoKit

/// A deterministic, non-authoritative identity fingerprint for a `CanonicalScientificDataset`.
///
/// The fingerprint is a stable content digest of the confirmed scientific dataset shape. It is NOT
/// an authority receipt: possessing it grants no detector, review, result-package, or export
/// authority, does not confirm or activate a dataset, and is not a persisted manifest identity. It
/// exists only for in-memory shadow comparison and later identity work.
///
/// `schemaContractID` and `schemaContractDigest` bind the actual byte codec — SHA-256, both
/// domain-separation strings, the primitive encoding rules, every `FieldTag`, every token and
/// enum-to-token mapping, the canonical traversal order, the canonical-ordering rules, and the
/// structural facts (dataset-global spike-train identity, the current strict one-group-per-train
/// partition, preserved duplicate multiplicity, and the absence of `RecordingSegment`/`Trial`).
/// Any codec or canonical-ordering change requires a schema-contract update; fixed schema and
/// byte-transcript goldens enforce that coordination.
public struct CanonicalScientificDatasetFingerprint: Hashable, Sendable {
    public let schemaContractID: String
    public let schemaContractDigest: String
    public let datasetDigest: String
    /// A deterministic diagnostic: the number of canonical transcript bytes fed to the dataset
    /// digest. It is not itself an identity input.
    public let encodedByteCount: Int

    internal init(
        schemaContractID: String,
        schemaContractDigest: String,
        datasetDigest: String,
        encodedByteCount: Int
    ) {
        self.schemaContractID = schemaContractID
        self.schemaContractDigest = schemaContractDigest
        self.datasetDigest = datasetDigest
        self.encodedByteCount = encodedByteCount
    }
}

/// A registry/partition defect in a `CanonicalScientificDataset`: the dataset-global spike-train
/// registry and its group references are not uniquely identified, canonically ordered, resolvable,
/// nonempty, or in the current strict one-group-per-train partition. This is NOT a full scientific
/// validity check; complete scientific validity is established upstream by the independent validator
/// that produces `CanonicalProjectionValidatedImport`.
public enum CanonicalRegistryPartitionError: Error, Equatable, Sendable {
    case spikeTrainRegistryIDsNotUniqueAndOrdered
    case eventScopeGroupIDsNotUniqueAndOrdered
    case emptyEventScopeGroup(ScientificEventScopeGroupID)
    case spikeTrainReferencesNotUniqueAndOrdered(ScientificEventScopeGroupID)
    case spikeTrainReferenceNotInRegistry(ScientificSpikeTrainID)
    case registrySpikeTrainNotReferencedExactlyOnce(ScientificSpikeTrainID)
}

/// Produces the canonical dataset fingerprint with one dedicated streaming encoder. The encoder
/// traversal is written once and driven into either a SHA-256 sink (production) or a byte-collecting
/// sink (the test transcript oracle); there is no second handwritten encoder.
///
/// Validation boundary: complete scientific validity of the canonical dataset is established
/// upstream by the independent validator that mints `CanonicalProjectionValidatedImport`. This type
/// additionally verifies only the registry/partition invariants — canonical registry order, group
/// order, resolvable references, nonempty groups, and the current strict one-group-per-train
/// partition — before hashing. A full independent canonical-graph validator is required before any
/// future second builder, deserializer, persistence path, or authority consumer relies on this
/// fingerprint.
///
/// The encoder is intentionally independent of the legacy `StableDigestEncoder`, whose Double-based
/// dataset identity has different semantics. It streams with bounded auxiliary memory: encoding does
/// not allocate in proportion to spike timestamp count (fixed-width primitives and one-byte fields
/// go through stack buffers to the incremental hasher, and no whole-payload String/Data/JSON is
/// materialized); the registry/partition check uses memory proportional to the number of
/// train/group identities; and the rare UTF-8 fallback is proportional to one token. The projector's
/// existing ordering is trusted rather than re-sorted.
internal enum CanonicalScientificDatasetFingerprinter {
    /// A functional (non-ordinal) schema contract identifier naming the supported canonical shape.
    internal static let schemaContractID = "canonical_microsecond_event_scope_dataset"

    /// The digest of the machine-auditable schema-contract transcript. Deterministic and stable.
    internal static func schemaContractDigest() -> String {
        var sink = CanonicalDigestSHA256Sink()
        encodeSchemaContract(into: &sink)
        return sink.finalizeHex()
    }

    /// Computes the fingerprint of a canonical dataset whose registry/partition is valid. Throws a
    /// `CanonicalRegistryPartitionError` before hashing if the registry and references are not
    /// uniquely identified, canonically ordered, resolvable, nonempty, and in the strict current
    /// one-group-per-train partition. This is not a full scientific-validity check.
    internal static func fingerprint(
        _ dataset: CanonicalScientificDataset
    ) throws -> CanonicalScientificDatasetFingerprint {
        try validateRegistryPartition(dataset)
        let contractDigest = schemaContractDigest()
        var sink = CanonicalDigestSHA256Sink()
        encodeDataset(dataset, schemaContractDigest: contractDigest, into: &sink)
        return CanonicalScientificDatasetFingerprint(
            schemaContractID: schemaContractID,
            schemaContractDigest: contractDigest,
            datasetDigest: sink.finalizeHex(),
            encodedByteCount: sink.transcriptByteCount
        )
    }

    // MARK: - Registry/partition validation (not full scientific validity)

    /// Registry/partition validation only: the registry IDs are unique and canonically ordered,
    /// group IDs are unique and canonically ordered, groups are nonempty, references are
    /// unique/sorted/resolvable, and every registry entry is referenced by exactly one group (the
    /// strict current partition). No orphan or dangling entries. This does not check event-graph or
    /// attribute scientific validity, which the independent validator establishes upstream.
    internal static func validateRegistryPartition(
        _ dataset: CanonicalScientificDataset
    ) throws {
        guard isStrictlyAscending(dataset.spikeTrains.map { semanticText($0.semanticID) }) else {
            throw CanonicalRegistryPartitionError
                .spikeTrainRegistryIDsNotUniqueAndOrdered
        }
        guard isStrictlyAscending(dataset.eventScopeGroups.map { semanticText($0.semanticID) }) else {
            throw CanonicalRegistryPartitionError
                .eventScopeGroupIDsNotUniqueAndOrdered
        }

        let registryIDs = Set(dataset.spikeTrains.map(\.semanticID))
        var referenceCounts: [ScientificSpikeTrainID: Int] = [:]
        for group in dataset.eventScopeGroups {
            guard !group.spikeTrainReferences.isEmpty else {
                throw CanonicalRegistryPartitionError.emptyEventScopeGroup(group.semanticID)
            }
            guard isStrictlyAscending(group.spikeTrainReferences.map { semanticText($0) }) else {
                throw CanonicalRegistryPartitionError
                    .spikeTrainReferencesNotUniqueAndOrdered(group.semanticID)
            }
            for reference in group.spikeTrainReferences {
                guard registryIDs.contains(reference) else {
                    throw CanonicalRegistryPartitionError
                        .spikeTrainReferenceNotInRegistry(reference)
                }
                referenceCounts[reference, default: 0] += 1
            }
        }
        for train in dataset.spikeTrains where referenceCounts[train.semanticID] != 1 {
            throw CanonicalRegistryPartitionError
                .registrySpikeTrainNotReferencedExactlyOnce(train.semanticID)
        }
    }

    private static func isStrictlyAscending(_ texts: [String]) -> Bool {
        for index in 1..<max(texts.count, 1) where index < texts.count {
            if !texts[index - 1].utf8.lexicographicallyPrecedes(texts[index].utf8) {
                return false
            }
        }
        return true
    }

    private static func semanticText(_ id: ScientificSpikeTrainID) -> String {
        id.semanticID.canonicalText
    }

    private static func semanticText(_ id: ScientificEventScopeGroupID) -> String {
        id.semanticID.canonicalText
    }

    // MARK: - Dataset transcript

    internal static func encodeDataset(
        _ dataset: CanonicalScientificDataset,
        schemaContractDigest contractDigest: String,
        into sink: inout some CanonicalTranscriptSink
    ) {
        sink.feedToken(Domain.dataset)
        sink.feedField(FieldTag.schemaContractID)
        sink.feedToken(schemaContractID)
        sink.feedField(FieldTag.schemaContractDigest)
        sink.feedToken(contractDigest)

        sink.feedField(FieldTag.activityMode)
        sink.feedToken(Token.activityMode(dataset.activityMode))

        sink.feedField(FieldTag.spikeTrainRegistry)
        sink.feedCount(dataset.spikeTrains.count)
        for train in dataset.spikeTrains {
            sink.feedField(FieldTag.spikeTrainID)
            sink.feedToken(train.semanticID.semanticID.canonicalText)
            sink.feedField(FieldTag.spikeTimestamps)
            sink.feedCount(train.rawTimestamps.count)
            for tick in train.rawTimestamps {
                sink.feedInt64(tick.microseconds)
            }
        }

        sink.feedField(FieldTag.eventScopeGroups)
        sink.feedCount(dataset.eventScopeGroups.count)
        for group in dataset.eventScopeGroups {
            sink.feedField(FieldTag.groupID)
            sink.feedToken(group.semanticID.semanticID.canonicalText)
            encodeTimeBasis(group.timeBasis, into: &sink)

            sink.feedField(FieldTag.spikeTrainReferences)
            sink.feedCount(group.spikeTrainReferences.count)
            for reference in group.spikeTrainReferences {
                sink.feedField(FieldTag.spikeTrainReference)
                sink.feedToken(reference.semanticID.canonicalText)
            }

            sink.feedField(FieldTag.eventDefinitions)
            sink.feedCount(group.eventDefinitions.count)
            for definition in group.eventDefinitions {
                sink.feedField(FieldTag.eventDefinitionID)
                sink.feedToken(definition.semanticID.semanticID.canonicalText)
                sink.feedField(FieldTag.eventTypeID)
                sink.feedToken(definition.eventTypeID.semanticID.canonicalText)
                sink.feedField(FieldTag.eventOccurrences)
                sink.feedCount(definition.occurrences.count)
                for occurrence in definition.occurrences {
                    sink.feedField(FieldTag.occurrenceTick)
                    sink.feedInt64(occurrence.tick.microseconds)
                    sink.feedField(FieldTag.occurrenceAttributes)
                    encodeAttributes(occurrence.scientificAttributes, into: &sink)
                }
            }
        }

        sink.feedField(FieldTag.attributeDefinitions)
        sink.feedCount(dataset.scientificAttributeDefinitions.count)
        for definition in dataset.scientificAttributeDefinitions {
            sink.feedField(FieldTag.attributeDefinitionKey)
            sink.feedToken(definition.key.canonicalText)
            sink.feedField(FieldTag.scalarType)
            sink.feedToken(Token.scalarType(definition.scalarType))
            sink.feedField(FieldTag.unit)
            encodeUnit(definition.unit, into: &sink)
            sink.feedField(FieldTag.emptyStringPolicy)
            sink.feedToken(Token.emptyStringPolicy(definition.emptyStringPolicy))
        }
    }

    private static func encodeTimeBasis(
        _ timeBasis: CanonicalEventScopeTimeBasis,
        into sink: inout some CanonicalTranscriptSink
    ) {
        sink.feedField(FieldTag.timeBasis)
        switch timeBasis {
        case .recordingElapsed:
            sink.feedToken(Token.timeBasisRecordingElapsed)
        case .eventRelative(let origin):
            sink.feedToken(Token.timeBasisEventRelative)
            sink.feedField(FieldTag.originEventDefinitionID)
            sink.feedToken(origin.eventDefinitionID.semanticID.canonicalText)
            sink.feedField(FieldTag.originTick)
            sink.feedInt64(origin.tick.microseconds)
            sink.feedField(FieldTag.originAttributes)
            encodeAttributes(origin.scientificAttributes, into: &sink)
        }
    }

    /// Encodes an attribute list the projector has already canonically ordered. A missing attribute
    /// simply does not appear here, whereas a present empty string appears with a zero-length value
    /// token, so the two are always distinct in the transcript.
    private static func encodeAttributes(
        _ attributes: [CanonicalEventAttributeValue],
        into sink: inout some CanonicalTranscriptSink
    ) {
        sink.feedCount(attributes.count)
        for attribute in attributes {
            sink.feedField(FieldTag.attributeKey)
            sink.feedToken(attribute.key.canonicalText)
            encodeValue(attribute.value, into: &sink)
        }
    }

    private static func encodeValue(
        _ value: EventAttributeValue,
        into sink: inout some CanonicalTranscriptSink
    ) {
        sink.feedField(FieldTag.valueScalarType)
        switch value {
        case .string(let string):
            sink.feedToken(Token.scalarString)
            sink.feedField(FieldTag.valuePayload)
            sink.feedToken(string.canonicalText)
        case .integer(let integer):
            sink.feedToken(Token.scalarInteger)
            sink.feedField(FieldTag.valuePayload)
            sink.feedToken(integer.canonicalText)
        case .exactDecimal(let decimal):
            sink.feedToken(Token.scalarExactDecimal)
            sink.feedField(FieldTag.valuePayload)
            sink.feedToken(decimal.canonicalText)
        case .boolean(let boolean):
            sink.feedToken(Token.scalarBoolean)
            sink.feedField(FieldTag.valuePayload)
            sink.feedBool(boolean)
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

    // MARK: - Schema contract transcript (binds the real codec)

    internal static func encodeSchemaContract(
        into sink: inout some CanonicalTranscriptSink
    ) {
        sink.feedToken(Domain.schemaContract)
        sink.feedField(ContractSection.identity)
        sink.feedToken(schemaContractID)

        sink.feedField(ContractSection.domains)
        sink.feedCount(2)
        sink.feedToken(Domain.dataset)
        sink.feedToken(Domain.schemaContract)

        sink.feedField(ContractSection.primitives)
        sink.feedCount(primitiveRules.count)
        for rule in primitiveRules { sink.feedToken(rule) }

        sink.feedField(ContractSection.fieldTags)
        sink.feedCount(fieldTagBindings.count)
        for binding in fieldTagBindings {
            sink.feedToken(binding.name)
            sink.feedByte(binding.value)
        }

        sink.feedField(ContractSection.tokens)
        sink.feedCount(tokenBindings.count)
        for binding in tokenBindings {
            sink.feedToken(binding.name)
            sink.feedToken(binding.value)
        }

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
        "boolean=one_byte_0_or_1",
        "string=int64_length_prefix_then_utf8",
    ]

    /// Every `FieldTag`, bound by name to its numeric value. Derived from the same constants used by
    /// the encoder, so changing any tag value necessarily changes the schema digest.
    private static let fieldTagBindings: [(name: String, value: UInt8)] = [
        ("schema_contract_id", FieldTag.schemaContractID),
        ("schema_contract_digest", FieldTag.schemaContractDigest),
        ("activity_mode", FieldTag.activityMode),
        ("spike_train_registry", FieldTag.spikeTrainRegistry),
        ("spike_train_id", FieldTag.spikeTrainID),
        ("spike_timestamps", FieldTag.spikeTimestamps),
        ("event_scope_groups", FieldTag.eventScopeGroups),
        ("group_id", FieldTag.groupID),
        ("time_basis", FieldTag.timeBasis),
        ("origin_event_definition_id", FieldTag.originEventDefinitionID),
        ("origin_tick", FieldTag.originTick),
        ("origin_attributes", FieldTag.originAttributes),
        ("spike_train_references", FieldTag.spikeTrainReferences),
        ("spike_train_reference", FieldTag.spikeTrainReference),
        ("event_definitions", FieldTag.eventDefinitions),
        ("event_definition_id", FieldTag.eventDefinitionID),
        ("event_type_id", FieldTag.eventTypeID),
        ("event_occurrences", FieldTag.eventOccurrences),
        ("occurrence_tick", FieldTag.occurrenceTick),
        ("occurrence_attributes", FieldTag.occurrenceAttributes),
        ("attribute_key", FieldTag.attributeKey),
        ("value_scalar_type", FieldTag.valueScalarType),
        ("value_payload", FieldTag.valuePayload),
        ("attribute_definitions", FieldTag.attributeDefinitions),
        ("attribute_definition_key", FieldTag.attributeDefinitionKey),
        ("scalar_type", FieldTag.scalarType),
        ("unit", FieldTag.unit),
        ("unit_symbol", FieldTag.unitSymbol),
        ("empty_string_policy", FieldTag.emptyStringPolicy),
    ]

    /// Every token and enum-to-token mapping, derived from the same `Token` constants used by the
    /// encoder, so renaming any token necessarily changes the schema digest.
    private static let tokenBindings: [(name: String, value: String)] = [
        ("activity_mode.putative_single_unit", Token.activityMode(.putativeSingleUnit)),
        ("activity_mode.intentional_multi_unit", Token.activityMode(.intentionalMultiUnit)),
        ("activity_mode.unknown_or_uncertain", Token.activityMode(.unknownOrUncertain)),
        ("scalar.string", Token.scalarType(.string)),
        ("scalar.integer", Token.scalarType(.integer)),
        ("scalar.exact_decimal", Token.scalarType(.exactDecimal)),
        ("scalar.boolean", Token.scalarType(.boolean)),
        ("time_basis.recording_elapsed", Token.timeBasisRecordingElapsed),
        ("time_basis.event_relative", Token.timeBasisEventRelative),
        ("unit.not_applicable", Token.unitNotApplicable),
        ("unit.dimensionless", Token.unitDimensionless),
        ("unit.specified", Token.unitSpecified),
        ("empty_string.forbid", Token.emptyStringPolicy(.forbid)),
        ("empty_string.allow_explicit_empty_string",
         Token.emptyStringPolicy(.allowExplicitEmptyString)),
    ]

    /// The canonical field/traversal order of the dataset transcript.
    private static let traversalGrammar = [
        "schema_contract_id",
        "schema_contract_digest",
        "activity_mode",
        "spike_train_registry[count]{spike_train_id;spike_timestamps[count]{int64_tick}}",
        "event_scope_groups[count]{group_id;time_basis(recording_elapsed|event_relative"
            + "{origin_event_definition_id;origin_tick;origin_attributes});"
            + "spike_train_references[count]{reference};"
            + "event_definitions[count]{event_definition_id;event_type_id;"
            + "event_occurrences[count]{occurrence_tick;occurrence_attributes}}}",
        "attributes[count]{attribute_key;value_scalar_type;value_payload}",
        "scientific_attribute_definitions[count]{attribute_definition_key;scalar_type;unit;"
            + "empty_string_policy}",
    ]

    private static let structuralFacts = [
        "spike_train_identity=dataset_global",
        "partition=strict_current_one_group_per_spike_train",
        "spike_train_referenced_exactly_once=true",
        "duplicate_timestamp_multiplicity=preserved_exact_in_canonical_raw",
        "duplicate_event_occurrence_multiplicity=retained_exact",
        "event_relative_origin=canonical_tick_zero_with_unique_group_local_matching_occurrence",
        "semantic_id_ordering=utf8_lexicographic",
        "occurrence_ordering=by_tick_then_scientific_attributes",
        "attribute_key_ordering=utf8_lexicographic",
        "attribute_value_tie_break_scalar_type_order=string<integer<exact_decimal<boolean",
        "attribute_value_payload=string|integer|exact_decimal_use_canonical_text;boolean=one_byte",
        "unit_branch=not_applicable|dimensionless|specified_with_symbol_token",
        "recording_segment=not_represented",
        "trial=not_represented",
        "excludes=source_provenance;presentation;duplicate_analysis_policy;detector_settings",
    ]
}

// MARK: - Streaming sink

/// A streaming byte sink with explicit domain separation, fixed-width big-endian integers,
/// one-byte Booleans, and length-prefixed UTF-8. The same traversal drives both the production
/// SHA-256 sink and the test byte-collecting sink.
internal protocol CanonicalTranscriptSink {
    var transcriptByteCount: Int { get }
    mutating func write(_ buffer: UnsafeRawBufferPointer)
}

extension CanonicalTranscriptSink {
    mutating func feedByte(_ value: UInt8) {
        var value = value
        withUnsafeBytes(of: &value) { write($0) }
    }

    mutating func feedField(_ tag: UInt8) {
        feedByte(tag)
    }

    mutating func feedBool(_ value: Bool) {
        feedByte(value ? 1 : 0)
    }

    mutating func feedInt64(_ value: Int64) {
        var bigEndian = value.bigEndian
        withUnsafeBytes(of: &bigEndian) { write($0) }
    }

    mutating func feedCount(_ value: Int) {
        feedInt64(Int64(value))
    }

    mutating func feedToken(_ value: String) {
        feedInt64(Int64(value.utf8.count))
        var value = value
        let wroteContiguously = value.utf8.withContiguousStorageIfAvailable { buffer -> Bool in
            if let base = buffer.baseAddress {
                write(UnsafeRawBufferPointer(start: base, count: buffer.count))
            }
            return true
        } ?? false
        if !wroteContiguously {
            var bytes = Array(value.utf8)
            bytes.withUnsafeBytes { write($0) }
        }
    }
}

/// Production sink: feeds bytes straight to an incremental SHA-256 with no per-field `Data`.
private struct CanonicalDigestSHA256Sink: CanonicalTranscriptSink {
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

private enum Domain {
    static let dataset = "stpd.canonical_scientific_dataset"
    static let schemaContract = "stpd.canonical_scientific_dataset.schema_contract"
}

private enum ContractSection {
    static let identity: UInt8 = 0x01
    static let domains: UInt8 = 0x02
    static let primitives: UInt8 = 0x03
    static let fieldTags: UInt8 = 0x04
    static let tokens: UInt8 = 0x05
    static let traversal: UInt8 = 0x06
    static let structure: UInt8 = 0x07
}

private enum FieldTag {
    static let schemaContractID: UInt8 = 0x02
    static let schemaContractDigest: UInt8 = 0x03
    static let activityMode: UInt8 = 0x10
    static let spikeTrainRegistry: UInt8 = 0x20
    static let spikeTrainID: UInt8 = 0x21
    static let spikeTimestamps: UInt8 = 0x22
    static let eventScopeGroups: UInt8 = 0x30
    static let groupID: UInt8 = 0x31
    static let timeBasis: UInt8 = 0x32
    static let originEventDefinitionID: UInt8 = 0x33
    static let originTick: UInt8 = 0x34
    static let originAttributes: UInt8 = 0x35
    static let spikeTrainReferences: UInt8 = 0x36
    static let spikeTrainReference: UInt8 = 0x37
    static let eventDefinitions: UInt8 = 0x38
    static let eventDefinitionID: UInt8 = 0x39
    static let eventTypeID: UInt8 = 0x3A
    static let eventOccurrences: UInt8 = 0x3B
    static let occurrenceTick: UInt8 = 0x3C
    static let occurrenceAttributes: UInt8 = 0x3D
    static let attributeKey: UInt8 = 0x41
    static let valueScalarType: UInt8 = 0x42
    static let valuePayload: UInt8 = 0x43
    static let attributeDefinitions: UInt8 = 0x50
    static let attributeDefinitionKey: UInt8 = 0x51
    static let scalarType: UInt8 = 0x52
    static let unit: UInt8 = 0x53
    static let unitSymbol: UInt8 = 0x54
    static let emptyStringPolicy: UInt8 = 0x55
}

private enum Token {
    static func activityMode(_ mode: ScientificDatasetActivityMode) -> String {
        switch mode {
        case .putativeSingleUnit: return "activity_mode.putative_single_unit"
        case .intentionalMultiUnit: return "activity_mode.intentional_multi_unit"
        case .unknownOrUncertain: return "activity_mode.unknown_or_uncertain"
        }
    }

    static func scalarType(_ type: EventAttributeScalarType) -> String {
        switch type {
        case .string: return scalarString
        case .integer: return scalarInteger
        case .exactDecimal: return scalarExactDecimal
        case .boolean: return scalarBoolean
        }
    }

    static func emptyStringPolicy(_ policy: EventEmptyStringPolicy) -> String {
        switch policy {
        case .forbid: return "empty_string.forbid"
        case .allowExplicitEmptyString: return "empty_string.allow_explicit_empty_string"
        }
    }

    static let scalarString = "scalar.string"
    static let scalarInteger = "scalar.integer"
    static let scalarExactDecimal = "scalar.exact_decimal"
    static let scalarBoolean = "scalar.boolean"

    static let timeBasisRecordingElapsed = "time_basis.recording_elapsed"
    static let timeBasisEventRelative = "time_basis.event_relative"

    static let unitNotApplicable = "unit.not_applicable"
    static let unitDimensionless = "unit.dimensionless"
    static let unitSpecified = "unit.specified"
}
