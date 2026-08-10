@testable import STPDCore
import Testing
import Foundation
import CryptoKit

// MARK: - Golden values (captured from the fixed machine transcript)

private let goldenSchemaContractDigest = "77d84671359ce1276c0a48951ca3cdf574392fd7c35a714d82bf814c326cf605"
private let goldenRichDatasetDigest = "172a04e8b81feae854e2ed11fb72f9cb1cbded8ff22b9ee6a53c1b595908d7bc"
private let goldenRichByteCount = 750
private let goldenBoundaryDigest = "00f3db9c25886c9cab36afbcc2c35edd695b59011f65a83ac212a6804b5db1a6"
private let goldenOracleDatasetDigest = "470d71f8840289647f098666657c1d7a703212617aa5ebe572f67286c63660b9"
private let goldenOracleByteCount = 517
private let goldenOracleTranscriptHex = "0000000000000021737470642e63616e6f6e6963616c5f736369656e74696669635f6461746173657402000000000000002963616e6f6e6963616c5f6d6963726f7365636f6e645f6576656e745f73636f70655f646174617365740300000000000000403737643834363731333539636531323736633061343839353163613363646635373433393266643763333561373134643832626638313463333236636636303510000000000000002261637469766974795f6d6f64652e70757461746976655f73696e676c655f756e697420000000000000000121000000000000000175220000000000000002000000000000000100000000000000013000000000000000013100000000000000016732000000000000001c74696d655f62617369732e7265636f7264696e675f656c617073656436000000000000000137000000000000000175380000000000000001390000000000000001653a0000000000000004747970653b00000000000000013c00000000000000023d00000000000000014100000000000000016b42000000000000000e7363616c61722e626f6f6c65616e43015000000000000000015100000000000000016b52000000000000000e7363616c61722e626f6f6c65616e530000000000000012756e69742e64696d656e73696f6e6c657373550000000000000013656d7074795f737472696e672e666f72626964"

// MARK: - Schema contract binds the real codec

@Test
func canonicalFingerprintExposesFunctionalSchemaContract() {
    #expect(
        CanonicalScientificDatasetFingerprinter.schemaContractID
            == "canonical_microsecond_event_scope_dataset"
    )
    #expect(CanonicalScientificDatasetFingerprinter.schemaContractDigest().count == 64)
    #expect(
        CanonicalScientificDatasetFingerprinter.schemaContractDigest()
            == goldenSchemaContractDigest
    )
}

/// Cryptographically closes the schema-contract loop: hashing the independently collected schema
/// transcript bytes must reproduce `schemaContractDigest()`.
@Test
func canonicalFingerprintSchemaTranscriptClosesUnderSHA256() {
    var sink = CollectingTranscriptSink()
    CanonicalScientificDatasetFingerprinter.encodeSchemaContract(into: &sink)
    let independent = sha256Hex(sink.bytes)
    #expect(independent == CanonicalScientificDatasetFingerprinter.schemaContractDigest())
    #expect(independent == goldenSchemaContractDigest)
}

// MARK: - Exact byte-transcript oracle, cryptographically closed

@Test
func canonicalFingerprintTranscriptOracleIsByteExactAndClosesUnderSHA256() throws {
    let dataset = try oracleDataset()
    let contractDigest = CanonicalScientificDatasetFingerprinter.schemaContractDigest()

    var sink = CollectingTranscriptSink()
    CanonicalScientificDatasetFingerprinter.encodeDataset(
        dataset,
        schemaContractDigest: contractDigest,
        into: &sink
    )

    // Exact transcript bytes and byte count.
    #expect(sink.hex == goldenOracleTranscriptHex)
    #expect(sink.transcriptByteCount == goldenOracleByteCount)

    // Independent SHA-256 over the collected bytes must equal the production dataset digest.
    let independent = sha256Hex(sink.bytes)
    let fingerprint = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)
    #expect(independent == fingerprint.datasetDigest)
    #expect(fingerprint.datasetDigest == goldenOracleDatasetDigest)
    #expect(fingerprint.encodedByteCount == goldenOracleByteCount)
    #expect(fingerprint.schemaContractDigest == goldenSchemaContractDigest)
}

// MARK: - Fixed dataset-digest golden + determinism (scientifically valid fixture)

@Test
func canonicalFingerprintMatchesFixedGoldenDataset() throws {
    let dataset = try Scenario().build()
    let first = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)
    let second = try CanonicalScientificDatasetFingerprinter.fingerprint(dataset)

    #expect(first == second)
    #expect(first.schemaContractID == "canonical_microsecond_event_scope_dataset")
    #expect(first.schemaContractDigest == goldenSchemaContractDigest)
    #expect(first.datasetDigest == goldenRichDatasetDigest)
    #expect(first.datasetDigest.count == 64)
    #expect(first.encodedByteCount == goldenRichByteCount)
}

// MARK: - Registry/partition check keeps invalid registries from being fingerprinted
// (Negative fixtures are intentionally invalid registries — never used to produce a golden.)

@Test
func canonicalFingerprintRejectsInvalidRegistryPartitions() throws {
    // Orphan registry entry: a train that no group references.
    #expect(throws: CanonicalRegistryPartitionError.self) {
        _ = try CanonicalScientificDatasetFingerprinter.fingerprint(
            CanonicalScientificDataset(
                activityMode: .putativeSingleUnit,
                spikeTrains: [try train("unit", ticks: [1])],
                eventScopeGroups: [],
                scientificAttributeDefinitions: []
            )
        )
    }
    // Dangling reference: a group referencing a train absent from the registry.
    #expect(throws: CanonicalRegistryPartitionError.self) {
        _ = try CanonicalScientificDatasetFingerprinter.fingerprint(
            CanonicalScientificDataset(
                activityMode: .putativeSingleUnit,
                spikeTrains: [try train("unit", ticks: [1])],
                eventScopeGroups: [try group("group", references: ["ghost"])],
                scientificAttributeDefinitions: []
            )
        )
    }
    // Empty group.
    #expect(throws: CanonicalRegistryPartitionError.self) {
        _ = try CanonicalScientificDatasetFingerprinter.fingerprint(
            CanonicalScientificDataset(
                activityMode: .putativeSingleUnit,
                spikeTrains: [],
                eventScopeGroups: [try group("group", references: [])],
                scientificAttributeDefinitions: []
            )
        )
    }
    // A registry train referenced by two groups (violates strict one-group-per-train partition).
    #expect(throws: CanonicalRegistryPartitionError.self) {
        _ = try CanonicalScientificDatasetFingerprinter.fingerprint(
            CanonicalScientificDataset(
                activityMode: .putativeSingleUnit,
                spikeTrains: [try train("unit", ticks: [1])],
                eventScopeGroups: [
                    try group("group_a", references: ["unit"]),
                    try group("group_b", references: ["unit"]),
                ],
                scientificAttributeDefinitions: []
            )
        )
    }
}

// MARK: - Length-prefix collision resistance (valid registries)

@Test
func canonicalFingerprintResistsLengthPrefixCollisions() throws {
    // Without a length prefix, "a" + "bc" and "ab" + "c" would hash identically.
    let left = try oneGroupDataset(trains: [("a", []), ("bc", [])])
    let right = try oneGroupDataset(trains: [("ab", []), ("c", [])])
    #expect(try digest(left) != digest(right))
}

// MARK: - Tick encoding boundaries

@Test
func canonicalFingerprintEncodesSignedZeroNegativeAndInt64BoundaryTicks() throws {
    // Negative ticks are only valid under an event-relative time basis; recording-elapsed ticks
    // must be non-negative, so every signed-boundary case uses the valid event-relative Scenario.
    let boundary = try eventRelativeBoundaryDataset(ticks: [.min, -1, 0, 1, .max])
    let boundaryDigest = try digest(boundary)
    #expect(boundaryDigest == goldenBoundaryDigest)

    #expect(try digest(eventRelativeBoundaryDataset(ticks: [.min]))
        != digest(eventRelativeBoundaryDataset(ticks: [.max])))
    // The non-negative cases remain valid under recording-elapsed.
    #expect(try digest(oneGroupDataset(trains: [("unit", [0])]))
        != digest(oneGroupDataset(trains: [("unit", [1])])))
    #expect(try digest(oneGroupDataset(trains: [("unit", [0])]))
        != digest(oneGroupDataset(trains: [("unit", [0, 0])])))
}

// MARK: - Multiplicity

@Test
func canonicalFingerprintChangesWithDuplicateSpikeMultiplicity() throws {
    #expect(try digest(oneGroupDataset(trains: [("unit", [1, 2])]))
        != digest(oneGroupDataset(trains: [("unit", [1, 1, 2])])))
}

@Test
func canonicalFingerprintChangesWithIdenticalEventOccurrenceMultiplicity() throws {
    let single = try oneGroupDataset(
        trains: [("unit", [1])],
        eventDefinitions: [try eventDefinition("e", occurrences: [try occurrence(tick: 5)])]
    )
    let doubled = try oneGroupDataset(
        trains: [("unit", [1])],
        eventDefinitions: [
            try eventDefinition("e", occurrences: [try occurrence(tick: 5), try occurrence(tick: 5)]),
        ]
    )
    #expect(try digest(single) != digest(doubled))
}

// MARK: - Missing attribute versus explicitly permitted empty string

@Test
func canonicalFingerprintDistinguishesMissingAttributeFromEmptyString() throws {
    let noteDefinition = try attributeDefinition("note", .string, .notApplicable, .allowExplicitEmptyString)
    let missing = try oneGroupDataset(
        trains: [("unit", [1])],
        eventDefinitions: [try eventDefinition("e", occurrences: [try occurrence(tick: 5)])],
        attributeDefinitions: [noteDefinition]
    )
    let emptyString = try oneGroupDataset(
        trains: [("unit", [1])],
        eventDefinitions: [
            try eventDefinition(
                "e",
                occurrences: [
                    try occurrence(tick: 5, attributes: [try value("note", canonicalString(""))]),
                ]
            ),
        ],
        attributeDefinitions: [noteDefinition]
    )
    #expect(try digest(missing) != digest(emptyString))
}

// MARK: - Activity modes, scalar value types (with matching definitions), unit kinds

@Test
func canonicalFingerprintDistinguishesAllActivityModes() throws {
    let modes: [ScientificDatasetActivityMode] = [
        .putativeSingleUnit, .intentionalMultiUnit, .unknownOrUncertain,
    ]
    let digests = try modes.map { try digest(oneGroupDataset(activityMode: $0, trains: [("unit", [1])])) }
    #expect(Set(digests).count == modes.count)
}

@Test
func canonicalFingerprintDistinguishesAllScalarValueTypesWithMatchingDefinitions() throws {
    let cases: [(EventAttributeValue, EventAttributeScalarType)] = [
        (try canonicalString("v"), .string),
        (.integer(try ExactIntegerValue.parse("7")), .integer),
        (.exactDecimal(try ExactDecimalValue.parse("1.5")), .exactDecimal),
        (.boolean(true), .boolean),
    ]
    let digests = try cases.map { pair -> String in
        try digest(oneGroupDataset(
            trains: [("unit", [1])],
            eventDefinitions: [
                try eventDefinition(
                    "e",
                    occurrences: [try occurrence(tick: 5, attributes: [try value("k", pair.0)])]
                ),
            ],
            attributeDefinitions: [try attributeDefinition("k", pair.1, .notApplicable, .forbid)]
        ))
    }
    #expect(Set(digests).count == cases.count)
}

@Test
func canonicalFingerprintDistinguishesAllUnitKinds() throws {
    let units: [ConfirmedEventAttributeUnit] = [
        .notApplicable, .dimensionless, .specified(try OpaqueUnitSymbol(validating: "mW")),
    ]
    let digests = try units.map { unit -> String in
        try digest(oneGroupDataset(
            trains: [("unit", [1])],
            attributeDefinitions: [try attributeDefinition("k", .exactDecimal, unit, .forbid)]
        ))
    }
    #expect(Set(digests).count == units.count)
}

// MARK: - Event-relative origin: valid comparisons (no invalid nonzero-origin fixture)

@Test
func canonicalFingerprintDistinguishesRecordingElapsedFromEventRelative() throws {
    var elapsed = Scenario()
    elapsed.eventRelative = false
    #expect(try digest(Scenario().build()) != digest(elapsed.build()))
}

@Test
func canonicalFingerprintDistinguishesSelectedGroupLocalOrigin() throws {
    // Two valid tick-zero origin occurrences exist in the group; selecting a different one changes
    // the digest while both datasets stay scientifically valid.
    var selectOnset = Scenario()
    selectOnset.secondOriginCandidate = true
    selectOnset.originDefinitionID = "onset"
    var selectReset = Scenario()
    selectReset.secondOriginCandidate = true
    selectReset.originDefinitionID = "reset"
    #expect(try digest(selectOnset.build()) != digest(selectReset.build()))
}

@Test
func canonicalFingerprintChangesWhenOriginAttributeChangesInOriginAndMatchingOccurrence() throws {
    var baseValue = Scenario()
    baseValue.originValueText = "b"
    var changedValue = Scenario()
    changedValue.originValueText = "c"   // changes both the origin tuple and its matching occurrence
    #expect(try digest(baseValue.build()) != digest(changedValue.build()))
}

// MARK: - Every scientific field is identity-bearing (all fixtures scientifically valid)

@Test
func canonicalFingerprintChangesWithEveryScientificField() throws {
    let baseline = try digest(Scenario().build())

    func changed(_ mutate: (inout Scenario) -> Void) throws -> String {
        var scenario = Scenario()
        mutate(&scenario)
        return try digest(scenario.build())
    }

    #expect(try changed { $0.activityMode = .intentionalMultiUnit } != baseline)
    #expect(try changed { $0.unitID = "unit_z" } != baseline)
    #expect(try changed { $0.spikeTicks = [1, 3] } != baseline)
    #expect(try changed { $0.groupID = "group_z" } != baseline)
    #expect(try changed { $0.eventRelative = false } != baseline)
    #expect(try changed { $0.signalDefinitionID = "signal_z" } != baseline)
    #expect(try changed { $0.signalTypeID = "stimulus_z" } != baseline)
    #expect(try changed { $0.signalOccurrenceTick = 99 } != baseline)
    #expect(try changed { $0.signalValueText = "changed" } != baseline)
    #expect(try changed { $0.attributeUnit = .dimensionless } != baseline)
    #expect(try changed { $0.attributeEmptyPolicy = .allowExplicitEmptyString } != baseline)
    // Changing scalar type consistently changes the definition and every occurrence value/text.
    #expect(try changed {
        $0.attributeScalarType = .integer
        $0.originValueText = "7"
        $0.signalValueText = "7"
    } != baseline)
    // Changing the definition key consistently changes every occurrence attribute key.
    #expect(try changed { $0.attributeKey = "state" } != baseline)
}

// MARK: - Test-only sink + independent hash

private struct CollectingTranscriptSink: CanonicalTranscriptSink {
    private(set) var bytes: [UInt8] = []
    var transcriptByteCount: Int { bytes.count }
    mutating func write(_ buffer: UnsafeRawBufferPointer) {
        bytes.append(contentsOf: buffer)
    }
    var hex: String { bytes.map { String(format: "%02x", $0) }.joined() }
}

private func sha256Hex(_ bytes: [UInt8]) -> String {
    SHA256.hash(data: Data(bytes)).map { String(format: "%02x", $0) }.joined()
}

// MARK: - Primitive builders

private func digest(_ dataset: CanonicalScientificDataset) throws -> String {
    try CanonicalScientificDatasetFingerprinter.fingerprint(dataset).datasetDigest
}
private func semantic(_ text: String) throws -> ScientificSemanticID { try ScientificSemanticID(validating: text) }
private func spikeTrainID(_ text: String) throws -> ScientificSpikeTrainID { ScientificSpikeTrainID(try semantic(text)) }
private func groupID(_ text: String) throws -> ScientificEventScopeGroupID { ScientificEventScopeGroupID(try semantic(text)) }
private func eventDefinitionID(_ text: String) throws -> ScientificEventDefinitionID { ScientificEventDefinitionID(try semantic(text)) }
private func eventTypeID(_ text: String) throws -> ScientificEventTypeID { ScientificEventTypeID(try semantic(text)) }
private func attributeKey(_ text: String) throws -> EventAttributeKey { try EventAttributeKey(validating: text) }
private func canonicalString(_ text: String) throws -> EventAttributeValue { .string(try CanonicalStringValue(validating: text)) }
private func value(_ key: String, _ value: EventAttributeValue) throws -> CanonicalEventAttributeValue {
    CanonicalEventAttributeValue(key: try attributeKey(key), value: value)
}
private func attributeValue(_ text: String, _ type: EventAttributeScalarType) throws -> EventAttributeValue {
    switch type {
    case .string: return .string(try CanonicalStringValue(validating: text))
    case .integer: return .integer(try ExactIntegerValue.parse(text))
    case .exactDecimal: return .exactDecimal(try ExactDecimalValue.parse(text))
    case .boolean: return .boolean(text == "true")
    }
}
private func occurrence(
    tick: Int64,
    attributes: [CanonicalEventAttributeValue] = []
) throws -> CanonicalEventOccurrence {
    CanonicalEventOccurrence(tick: MicrosecondTick(microseconds: tick), scientificAttributes: attributes)
}
private func eventDefinition(
    _ id: String,
    type: String = "type",
    occurrences: [CanonicalEventOccurrence]
) throws -> CanonicalEventDefinition {
    CanonicalEventDefinition(
        semanticID: try eventDefinitionID(id),
        eventTypeID: try eventTypeID(type),
        occurrences: occurrences
    )
}
private func attributeDefinition(
    _ key: String,
    _ scalarType: EventAttributeScalarType,
    _ unit: ConfirmedEventAttributeUnit,
    _ policy: EventEmptyStringPolicy
) throws -> CanonicalEventAttributeDefinition {
    CanonicalEventAttributeDefinition(
        key: try attributeKey(key),
        scalarType: scalarType,
        unit: unit,
        emptyStringPolicy: policy
    )
}
private func train(_ id: String, ticks: [Int64]) throws -> CanonicalSpikeTrain {
    CanonicalSpikeTrain(
        semanticID: try spikeTrainID(id),
        rawTimestamps: ticks.map { MicrosecondTick(microseconds: $0) }
    )
}
private func group(
    _ id: String,
    references: [String],
    timeBasis: CanonicalEventScopeTimeBasis = .recordingElapsed,
    eventDefinitions: [CanonicalEventDefinition] = []
) throws -> CanonicalEventScopeGroup {
    CanonicalEventScopeGroup(
        semanticID: try groupID(id),
        timeBasis: timeBasis,
        spikeTrainReferences: try references.map { try spikeTrainID($0) },
        eventDefinitions: eventDefinitions
    )
}

/// A structurally valid dataset (recording-elapsed, no origin): one group references the entire
/// registry exactly once. Occurrence attributes, when present, always carry matching definitions.
private func oneGroupDataset(
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit,
    trains: [(id: String, ticks: [Int64])],
    eventDefinitions: [CanonicalEventDefinition] = [],
    attributeDefinitions: [CanonicalEventAttributeDefinition] = []
) throws -> CanonicalScientificDataset {
    let registry = try trains.map { try train($0.id, ticks: $0.ticks) }.sorted {
        $0.semanticID.semanticID.canonicalText.utf8
            .lexicographicallyPrecedes($1.semanticID.semanticID.canonicalText.utf8)
    }
    let references = trains.map(\.id).sorted { $0.utf8.lexicographicallyPrecedes($1.utf8) }
    return CanonicalScientificDataset(
        activityMode: activityMode,
        spikeTrains: registry,
        eventScopeGroups: [try group("group", references: references, eventDefinitions: eventDefinitions)],
        scientificAttributeDefinitions: attributeDefinitions
    )
}

/// A scientifically valid, event-relative dataset whose spike ticks exercise signed boundaries.
/// Event-relative time bases permit negative canonical ticks; recording-elapsed does not.
private func eventRelativeBoundaryDataset(ticks: [Int64]) throws -> CanonicalScientificDataset {
    var scenario = Scenario()
    scenario.spikeTicks = ticks
    return try scenario.build()
}

/// A small, valid fixture for the byte-transcript oracle (recording-elapsed).
private func oracleDataset() throws -> CanonicalScientificDataset {
    CanonicalScientificDataset(
        activityMode: .putativeSingleUnit,
        spikeTrains: [try train("u", ticks: [1, 1])],
        eventScopeGroups: [
            try group(
                "g",
                references: ["u"],
                eventDefinitions: [
                    try eventDefinition(
                        "e",
                        occurrences: [try occurrence(tick: 2, attributes: [try value("k", .boolean(true))])]
                    ),
                ]
            ),
        ],
        scientificAttributeDefinitions: [try attributeDefinition("k", .boolean, .dimensionless, .forbid)]
    )
}

/// A scientifically valid scenario builder. It guarantees the approved event-graph contract:
/// an event-relative origin has canonical tick zero, its selected definition exists in the group
/// with exactly one matching occurrence at tick zero, the origin tuple and that occurrence carry
/// identical Scientific attributes, and every occurrence attribute key has a matching Scientific
/// definition whose scalar type matches the value.
private struct Scenario {
    var activityMode: ScientificDatasetActivityMode = .putativeSingleUnit
    var unitID = "unit_a"
    var spikeTicks: [Int64] = [1, 2]
    var groupID = "group_a"
    var eventRelative = true
    var originDefinitionID = "onset"       // must be a tick-zero definition ("onset" or "reset")
    var originValueText = "b"
    var secondOriginCandidate = false      // add a second tick-zero definition "reset"
    var signalDefinitionID = "signal"
    var signalTypeID = "stimulus"
    var signalOccurrenceTick: Int64 = 5
    var signalValueText = "b"
    var attributeKey = "condition"
    var attributeScalarType: EventAttributeScalarType = .string
    var attributeUnit: ConfirmedEventAttributeUnit = .notApplicable
    var attributeEmptyPolicy: EventEmptyStringPolicy = .forbid

    func build() throws -> CanonicalScientificDataset {
        let originValue = try attributeValue(originValueText, attributeScalarType)
        let signalValue = try attributeValue(signalValueText, attributeScalarType)

        func tickZeroDefinition(_ id: String) throws -> CanonicalEventDefinition {
            try eventDefinition(
                id,
                type: "marker",
                occurrences: [try occurrence(tick: 0, attributes: [value(attributeKey, originValue)])]
            )
        }

        var definitions = [try tickZeroDefinition("onset")]
        if secondOriginCandidate {
            definitions.append(try tickZeroDefinition("reset"))
        }
        definitions.append(
            try eventDefinition(
                signalDefinitionID,
                type: signalTypeID,
                occurrences: [try occurrence(tick: signalOccurrenceTick, attributes: [value(attributeKey, signalValue)])]
            )
        )
        definitions.sort {
            $0.semanticID.semanticID.canonicalText.utf8
                .lexicographicallyPrecedes($1.semanticID.semanticID.canonicalText.utf8)
        }

        let timeBasis: CanonicalEventScopeTimeBasis
        if eventRelative {
            timeBasis = .eventRelative(
                origin: CanonicalEventOrigin(
                    eventDefinitionID: try eventDefinitionID(originDefinitionID),
                    tick: .zero,
                    scientificAttributes: [try value(attributeKey, originValue)]
                )
            )
        } else {
            timeBasis = .recordingElapsed
        }

        return CanonicalScientificDataset(
            activityMode: activityMode,
            spikeTrains: [try train(unitID, ticks: spikeTicks)],
            eventScopeGroups: [
                try group(groupID, references: [unitID], timeBasis: timeBasis, eventDefinitions: definitions),
            ],
            scientificAttributeDefinitions: [
                try attributeDefinition(attributeKey, attributeScalarType, attributeUnit, attributeEmptyPolicy),
            ]
        )
    }
}
