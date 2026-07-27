import Foundation
import STPDCore
import Testing

// Phase 1A manual annotation core: model, geometry resolution, manual-first projection, manual lock,
// burst-only notBurst veto, JSON (Codable) persistence schema, and CSV import/export.

private let fiveSpikeTrain = SpikeTrain(name: "g", timestampsSec: [0, 0.1, 0.2, 0.3, 0.4])

private func annotation(
    train: String = "g",
    label: ManualAnnotationLabel,
    start: Double,
    end: Double,
    id: UUID = UUID(),
    createdAt: Date = Date(timeIntervalSince1970: 100),
    updatedAt: Date = Date(timeIntervalSince1970: 100)
) -> ManualAnnotation {
    ManualAnnotation(
        id: id,
        trainID: train,
        label: label,
        startSec: start,
        endSec: end,
        createdAt: createdAt,
        updatedAt: updatedAt
    )
}

// MARK: - 1. Geometry maps a time interval to the correct ISI indices.

@Test
func geometryResolvesTimeIntervalToISIIndices() {
    // Spikes at 0,0.1,0.2,0.3,0.4 -> ISI indices 1..4. Window [0.1,0.3] fully contains ISI 2 and 3
    // (the intervals strictly between spikes 1..3).
    let geo = ManualAnnotationGeometryResolver.resolve(startSec: 0.1, endSec: 0.3, in: fiveSpikeTrain)
    #expect(geo.isWithinTrain)
    #expect(geo.startISIIndex == 2)
    #expect(geo.endISIIndex == 3)
    #expect(geo.startSpikeIndex == 1)
    #expect(geo.endSpikeIndex == 3)
    #expect(geo.coveredISIIndices == 2...3)
}

@Test
func geometryFlagsAnnotationOutsideTrainInsteadOfCrashing() {
    let geo = ManualAnnotationGeometryResolver.resolve(startSec: 5.0, endSec: 6.0, in: fiveSpikeTrain)
    #expect(!geo.isWithinTrain)
    #expect(geo.coveredISIIndices == nil)
    // Wrong train id resolves to outside, not a crash.
    let wrongTrain = annotation(train: "other", label: .burst, start: 0.1, end: 0.3)
    #expect(!ManualAnnotationGeometryResolver.resolve(annotation: wrongTrain, in: fiveSpikeTrain).isWithinTrain)
}

@Test
func geometryFlagsNonFiniteBoundsAsOutsideRegardlessOfOrder() {
    // Swift.min/max(finite, .nan) returns the finite value, so the guard must check the raw inputs.
    #expect(!ManualAnnotationGeometryResolver.resolve(startSec: 0.1, endSec: .nan, in: fiveSpikeTrain).isWithinTrain)
    #expect(!ManualAnnotationGeometryResolver.resolve(startSec: .nan, endSec: 0.1, in: fiveSpikeTrain).isWithinTrain)
    #expect(!ManualAnnotationGeometryResolver.resolve(startSec: 0.1, endSec: .infinity, in: fiveSpikeTrain).isWithinTrain)
}

@Test
func csvImportRejectsNonFiniteBounds() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec
    77777777-7777-7777-7777-777777777777,g,positive,burst,0.1,nan
    88888888-8888-8888-8888-888888888888,g,positive,tonic,0.1,0.3
    """
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    #expect(result.annotations.count == 1)
    #expect(result.annotations.first?.label == .tonic)
    #expect(result.skippedRowCount == 1)
}

// MARK: - 2. Reversed time ranges normalize.

@Test
func geometryNormalizesReversedTimeRange() {
    let forward = ManualAnnotationGeometryResolver.resolve(startSec: 0.1, endSec: 0.3, in: fiveSpikeTrain)
    let reversed = ManualAnnotationGeometryResolver.resolve(startSec: 0.3, endSec: 0.1, in: fiveSpikeTrain)
    #expect(forward == reversed)
    #expect(reversed.coveredISIIndices == 2...3)
}

// MARK: - 3. Manual-first projection: positive overrides auto final label.

@Test
func positiveManualLabelOverridesAutoFinalLabelOnCoveredISIs() {
    let auto: [Int: String] = [1: "burst", 2: "burst", 3: "tonic", 4: "pause"]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: auto,
        annotations: [annotation(label: .tonic, start: 0.1, end: 0.3)],   // covers ISI 2..3
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )
    #expect(projection.manualPositiveLabelByISI[2] == "tonic")
    #expect(projection.manualPositiveLabelByISI[3] == "tonic")
    #expect(projection.finalLabelByISI[2] == "tonic")
    #expect(projection.finalLabelByISI[3] == "tonic")
    // The lock removes ISI 2 from the two-ISI auto burst, so the uncovered ISI 1 singleton is no
    // longer a structurally valid final burst. Unrelated non-burst labels remain unchanged.
    #expect(projection.finalLabelByISI[1] == nil)
    #expect(projection.finalLabelByISI[4] == "pause")
    #expect(projection.finalBurstISIsRemovedByStructuralMinimum == [1])
    #expect(projection.hasManualEffect)
}

// MARK: - 4. honorManualLock toggles auto availability; final stays manual-first either way.

@Test
func manualLockBlocksAutoForPublicOutputWhileFinalStaysManualFirst() {
    let auto: [Int: String] = [2: "burst", 3: "burst"]
    let annotations = [annotation(label: .tonic, start: 0.1, end: 0.3)]   // positive covers ISI 2..3

    let locked = ManualAnnotationProjector.project(
        train: fiveSpikeTrain, autoLabelsByISI: auto, annotations: annotations,
        honorManualLock: true, manualNegativeLabelsEnabled: true
    )
    // Under lock: auto suppressed for public output, but final is still manual-first.
    #expect(locked.autoBlockedByManualLockISIs == [2, 3])
    #expect(locked.effectiveAutoLabelByISI[2] == nil)
    #expect(locked.finalLabelByISI[2] == "tonic")

    let unlocked = ManualAnnotationProjector.project(
        train: fiveSpikeTrain, autoLabelsByISI: auto, annotations: annotations,
        honorManualLock: false, manualNegativeLabelsEnabled: true
    )
    // Without lock: auto labels remain available, manual diagnostics still exposed, final still manual-first.
    #expect(unlocked.autoBlockedByManualLockISIs.isEmpty)
    #expect(unlocked.effectiveAutoLabelByISI[2] == "burst")
    #expect(unlocked.manualPositiveLabelByISI[2] == "tonic")
    #expect(unlocked.finalLabelByISI[2] == "tonic")
}

// MARK: - 5. notBurst veto blocks only burst-family auto, never appears as a final label.

@Test
func notBurstVetoBlocksOnlyBurstFamilyAutoAndIsNeverAFinalLabel() {
    let auto: [Int: String] = [1: "burst", 2: "tonic", 3: "long_burst"]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: auto,
        annotations: [annotation(label: .notBurst, start: 0.0, end: 0.3)],   // covers ISI 1..3
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )
    // Burst-family auto (ISI 1 burst, ISI 3 long_burst) is blocked; non-burst (ISI 2 tonic) is kept.
    #expect(projection.autoBurstBlockedByVetoISIs == [1, 3])
    #expect(projection.effectiveAutoLabelByISI[1] == nil)
    #expect(projection.effectiveAutoLabelByISI[3] == nil)
    #expect(projection.effectiveAutoLabelByISI[2] == "tonic")
    #expect(projection.finalLabelByISI[2] == "tonic")
    // notBurst never becomes a final label.
    #expect(!projection.finalLabelByISI.values.contains("not_burst"))
    #expect(projection.finalLabelByISI[1] == nil)
}

@Test
func notBurstVetoIsInertWhenNegativeLabelsDisabled() {
    let auto: [Int: String] = [1: "burst", 2: "burst"]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: auto,
        annotations: [annotation(label: .notBurst, start: 0.0, end: 0.2)],
        honorManualLock: true,
        manualNegativeLabelsEnabled: false
    )
    #expect(projection.manualNegativeVetoByISI.isEmpty)
    #expect(projection.autoBurstBlockedByVetoISIs.isEmpty)
    #expect(projection.effectiveAutoLabelByISI[1] == "burst")
}

// MARK: - 6. Empty annotations are a no-op.

@Test
func emptyAnnotationsAreANoOpIdentityProjection() {
    let auto: [Int: String] = [1: "burst", 2: "tonic", 3: "pause"]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain, autoLabelsByISI: auto, annotations: [],
        honorManualLock: true, manualNegativeLabelsEnabled: true
    )
    #expect(!projection.hasManualEffect)
    #expect(projection.finalLabelByISI == auto)
    #expect(projection.effectiveAutoLabelByISI == auto)
    #expect(projection.autoBlockedByManualLockISIs.isEmpty)
    #expect(projection.autoBurstBlockedByVetoISIs.isEmpty)
    #expect(projection.finalBurstISIsRemovedByStructuralMinimum.isEmpty)
}

@Test
func vetoCannotLeaveSurvivingAutoBurstSingletonInProjectionMaps() {
    let auto: [Int: String] = [1: "burst", 2: "burst", 4: "pause"]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: auto,
        annotations: [annotation(label: .notBurst, start: 0.0, end: 0.1)],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )

    #expect(projection.autoBurstBlockedByVetoISIs == [1])
    #expect(projection.effectiveAutoLabelByISI[1] == nil)
    #expect(projection.effectiveAutoLabelByISI[2] == nil)
    #expect(projection.finalLabelByISI[2] == nil)
    #expect(projection.finalLabelByISI[4] == "pause")
    #expect(projection.finalBurstISIsRemovedByStructuralMinimum == [2])
}

@Test
func adjacentManualBurstCanCompleteAutoFragmentAfterVeto() {
    let auto: [Int: String] = [1: "burst", 2: "burst", 4: "pause"]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: auto,
        annotations: [
            annotation(label: .notBurst, start: 0.0, end: 0.1),
            annotation(label: .burst, start: 0.2, end: 0.3)
        ],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )

    #expect(projection.autoBurstBlockedByVetoISIs == [1])
    #expect(projection.effectiveAutoLabelByISI[1] == nil)
    #expect(projection.effectiveAutoLabelByISI[2] == "burst")
    #expect(projection.manualPositiveLabelByISI[3] == "burst")
    #expect(projection.finalLabelByISI[1] == nil)
    #expect(projection.finalLabelByISI[2] == "burst")
    #expect(projection.finalLabelByISI[3] == "burst")
    #expect(projection.finalLabelByISI[4] == "pause")
    #expect(projection.finalBurstISIsRemovedByStructuralMinimum.isEmpty)
    #expect(projection.skippedAnnotationCount == 0)
}

@Test
func unlockedPositiveOverrideCannotLeaveFinalAutoBurstSingletons() {
    let auto: [Int: String] = [1: "burst", 2: "burst", 3: "burst", 4: "pause"]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: auto,
        annotations: [annotation(label: .tonic, start: 0.1, end: 0.2)],
        honorManualLock: false,
        manualNegativeLabelsEnabled: true
    )

    // Lock is off, so the diagnostic auto surface remains the detector identity.
    #expect(projection.effectiveAutoLabelByISI == auto)
    // The manual tonic splits final burst coverage into two singletons, both structurally invalid.
    #expect(projection.finalLabelByISI[1] == nil)
    #expect(projection.finalLabelByISI[2] == "tonic")
    #expect(projection.finalLabelByISI[3] == nil)
    #expect(projection.finalLabelByISI[4] == "pause")
    #expect(projection.finalBurstISIsRemovedByStructuralMinimum == [1, 3])
}

// MARK: - 7. JSON (Codable) persistence round-trips annotations.

@Test
func annotationsRoundTripThroughJSON() throws {
    let payload: [String: [ManualAnnotation]] = [
        "g": [
            annotation(label: .burst, start: 0.1, end: 0.3, id: UUID(uuidString: "11111111-1111-1111-1111-111111111111")!),
            annotation(label: .notBurst, start: 0.3, end: 0.4, id: UUID(uuidString: "22222222-2222-2222-2222-222222222222")!)
        ]
    ]
    let encoder = JSONEncoder()
    encoder.dateEncodingStrategy = .iso8601
    let decoder = JSONDecoder()
    decoder.dateDecodingStrategy = .iso8601
    let data = try encoder.encode(payload)
    let restored = try decoder.decode([String: [ManualAnnotation]].self, from: data)
    #expect(restored == payload)
}

// MARK: - 8. CSV export/import round-trips and is separate from the auto-event CSV.

@Test
func csvExportImportRoundTripsAndIsSeparateFromAutoEventCSV() throws {
    let original = [
        ManualAnnotation(
            id: UUID(uuidString: "33333333-3333-3333-3333-333333333333")!,
            trainID: "g", label: .tonic, startSec: 0.1, endSec: 0.3,
            startISIIndex: 2, endISIIndex: 3, startSpikeIndex: 1, endSpikeIndex: 3,
            note: "needs, \"review\"",
            annotatorIdentitySource: .unknown,
            createdAt: Date(timeIntervalSince1970: 100), updatedAt: Date(timeIntervalSince1970: 200)
        )
    ]
    let csv = ManualAnnotationCSVExporter.csv(annotations: original)
    // It is the manual schema, not the auto-event schema.
    #expect(csv.hasPrefix("annotation_id,train_id,polarity,label,"))
    #expect(!csv.contains("decision_path"))
    #expect(!csv.contains("resolved_thresholds"))

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    #expect(result.unsupportedLabels.isEmpty)
    #expect(result.annotations == original)
}

@Test
func manualAnnotationCSVRoundTripsSubsecondTimestampsExactly() throws {
    let createdAt = Date(timeIntervalSince1970: 100.123_456_789)
    let updatedAt = Date(timeIntervalSince1970: 200.987_654_321)
    let original = [
        annotation(
            label: .tonic,
            start: 0.1,
            end: 0.3,
            id: UUID(uuidString: "12345678-1234-1234-1234-123456789012")!,
            createdAt: createdAt,
            updatedAt: updatedAt
        ),
    ]

    let csv = ManualAnnotationCSVExporter.csv(annotations: original)
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let restored = try #require(result.annotations.first)

    #expect(result.skippedRowCount == 0)
    #expect(restored.createdAt.timeIntervalSince1970
        == createdAt.timeIntervalSince1970)
    #expect(restored.updatedAt.timeIntervalSince1970
        == updatedAt.timeIntervalSince1970)
}

@Test
func manualAnnotationCSVRoundTripsNegativeSubsecondTimestampsExactly() throws {
    let createdAt = Date(timeIntervalSince1970: -123.987_654_321)
    let updatedAt = Date(timeIntervalSince1970: -0.1)
    let original = [
        annotation(
            label: .tonic,
            start: 0.1,
            end: 0.3,
            id: UUID(uuidString: "13345678-1234-1234-1234-123456789012")!,
            createdAt: createdAt,
            updatedAt: updatedAt
        ),
    ]

    let csv = ManualAnnotationCSVExporter.csv(annotations: original)
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let restored = try #require(result.annotations.first)

    #expect(result.skippedRowCount == 0)
    #expect(restored.createdAt.timeIntervalSince1970.bitPattern
        == createdAt.timeIntervalSince1970.bitPattern)
    #expect(restored.updatedAt.timeIntervalSince1970.bitPattern
        == updatedAt.timeIntervalSince1970.bitPattern)
}

@Test
func manualAnnotationCSVRoundTripsEpochScaleGeometryExactlyAndUsesCRLF() throws {
    let original = [
        annotation(
            label: .tonic,
            start: 1_800_000_000.123_456,
            end: 1_800_000_001.987_654,
            id: UUID(uuidString: "22345678-1234-1234-1234-123456789012")!
        ),
    ]

    let csv = ManualAnnotationCSVExporter.csv(annotations: original)
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let restored = try #require(result.annotations.first)

    #expect(csv.contains("\r\n"))
    #expect(!csv.replacingOccurrences(of: "\r\n", with: "").contains("\n"))
    #expect(restored.startSec == original[0].startSec)
    #expect(restored.endSec == original[0].endSec)
}

@Test
func manualAnnotationCSVPreservesQuotedEmbeddedCarriageReturn() throws {
    var original = annotation(
        label: .tonic,
        start: 0.1,
        end: 0.3,
        id: UUID(uuidString: "32345678-1234-1234-1234-123456789012")!
    )
    original.note = "first line\rsecond line"

    let csv = ManualAnnotationCSVExporter.csv(annotations: [original])
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)

    #expect(result.skippedRowCount == 0)
    #expect(result.annotations.first?.note == original.note)
}

@Test
func manualAnnotationCSVRoundTripsWhitespaceAndMultilineAuthorshipExactly()
    throws {
    var original = annotation(
        label: .tonic,
        start: 0.1,
        end: 0.3,
        id: UUID(uuidString: "42345678-1234-1234-1234-123456789012")!
    )
    original.note = "  first line\nsecond line  "
    original.annotator = "  Dr. Exact Name  "
    original.annotatorIdentitySource = .userProvided

    let csv = ManualAnnotationCSVExporter.csv(annotations: [original])
    let result = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )
    let restored = try #require(result.annotations.first)

    #expect(result.skippedRowCount == 0)
    #expect(restored.note == original.note)
    #expect(restored.annotator == original.annotator)
    #expect(
        restored.annotatorIdentitySource ==
            original.annotatorIdentitySource
    )
}

@Test
func manualAnnotationCSVRejectsUpdatedBeforeCreated() throws {
    let reversed = annotation(
        label: .tonic,
        start: 0.1,
        end: 0.3,
        id: UUID(uuidString: "42345678-1234-1234-1234-123456789012")!,
        createdAt: Date(timeIntervalSince1970: 200),
        updatedAt: Date(timeIntervalSince1970: 100)
    )

    let csv = ManualAnnotationCSVExporter.csv(annotations: [reversed])
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)

    #expect(result.annotations.isEmpty)
    #expect(result.skippedRowCount == 1)
}

@Test
func manualAnnotationCSVRejectsMalformedOrContradictoryTimestampEvidence() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,created_at,updated_at,created_at_unix_sec,updated_at_unix_sec
    10000000-0000-0000-0000-000000000001,g,positive,tonic,0.1,0.3,1970-01-01T00:01:40Z,1970-01-01T00:01:40Z,not-a-number,100
    10000000-0000-0000-0000-000000000002,g,positive,tonic,0.1,0.3,not-a-timestamp,1970-01-01T00:01:40Z,,100
    10000000-0000-0000-0000-000000000003,g,positive,tonic,0.1,0.3,1970-01-01T00:01:40.000000000Z,1970-01-01T00:01:40Z,101,100
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)

    #expect(result.annotations.isEmpty)
    #expect(result.skippedRowCount == 3)
}

@Test
func manualAnnotationCSVRejectsExactTimestampOutsideCanonicalRange() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,created_at_unix_sec,updated_at_unix_sec
    11000000-0000-0000-0000-000000000001,g,positive,tonic,0.1,0.3,9.223372036854776e18,9.223372036854776e18
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)

    #expect(result.annotations.isEmpty)
    #expect(result.skippedRowCount == 1)
}

@Test
func manualAnnotationCSVRejectsExactTimestampWithNonRoundTrippableISO()
    throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,created_at_unix_sec,updated_at_unix_sec
    12000000-0000-0000-0000-000000000001,g,positive,tonic,0.1,0.3,-1000000000000,-1000000000000
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )

    #expect(result.annotations.isEmpty)
    #expect(result.skippedRowCount == 1)
}

@Test
func manualAnnotationCSVExactOnlyTimestampsRequireExactDateRoundTrip()
    throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,created_at_unix_sec,updated_at_unix_sec
    13000000-0000-0000-0000-000000000001,g,positive,tonic,0.1,0.3,0.1,0.1
    13000000-0000-0000-0000-000000000002,g,positive,tonic,0.1,0.3,-0.1,-0.1
    13000000-0000-0000-0000-000000000003,g,positive,tonic,0.1,0.3,0.125,0.125
    13000000-0000-0000-0000-000000000004,g,positive,tonic,0.1,0.3,-0.125,-0.125
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )

    #expect(result.skippedRowCount == 2)
    #expect(result.annotations.map(\.id.uuidString) == [
        "13000000-0000-0000-0000-000000000003",
        "13000000-0000-0000-0000-000000000004"
    ])
    #expect(
        result.annotations.map(\.createdAt.timeIntervalSince1970.bitPattern)
            == [(0.125).bitPattern, (-0.125).bitPattern]
    )
}

@Test
func manualAnnotationCSVRejectsBlankOrTruncatedDeclaredTimestampEvidence()
    throws {
    let cases = [
        (
            "train_id,label,start_sec,end_sec,created_at",
            "g,tonic,0.1,0.3,"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at",
            "g,tonic,0.1,0.3"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at_unix_sec",
            "g,tonic,0.1,0.3,"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at_unix_sec",
            "g,tonic,0.1,0.3"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at,updated_at",
            "g,tonic,0.1,0.3,1970-01-01T00:01:40Z,"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at,updated_at",
            "g,tonic,0.1,0.3,1970-01-01T00:01:40Z"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at_unix_sec,updated_at_unix_sec",
            "g,tonic,0.1,0.3,100,"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at_unix_sec,updated_at_unix_sec",
            "g,tonic,0.1,0.3,100"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at,created_at_unix_sec",
            "g,tonic,0.1,0.3,1970-01-01T00:01:40Z,"
        ),
        (
            "train_id,label,start_sec,end_sec,created_at_unix_sec,updated_at,updated_at_unix_sec",
            "g,tonic,0.1,0.3,100,1970-01-01T00:03:20Z,"
        )
    ]

    for (header, row) in cases {
        let result = try ManualAnnotationCSVImporter.importAnnotations(
            contents: "\(header)\n\(row)\n"
        )

        #expect(result.annotations.isEmpty)
        #expect(result.skippedRowCount == 1)
    }
}

@Test
func legacyCSVWithoutTimestampColumnsUsesDocumentedFallbackChronology() throws {
    let csv = """
    train_id,label,start_sec,end_sec
    g,tonic,0.1,0.3
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )
    let restored = try #require(result.annotations.first)
    let fallback = Date(timeIntervalSince1970: 0)

    #expect(result.skippedRowCount == 0)
    #expect(restored.createdAt == fallback)
    #expect(restored.updatedAt == fallback)
}

@Test
func manualAnnotationCSVRejectsFractionalTimestampULPContradictions() throws {
    let seconds = 100.125
    let iso = "1970-01-01T00:01:40.125Z"

    for contradictorySeconds in [seconds.nextDown, seconds.nextUp] {
        let contradictoryExact = String(
            format: "%.17g",
            locale: Locale(identifier: "en_US_POSIX"),
            contradictorySeconds
        )
        let csv = """
        annotation_id,train_id,polarity,label,start_sec,end_sec,created_at,updated_at,created_at_unix_sec,updated_at_unix_sec
        18000000-0000-0000-0000-000000000001,g,positive,tonic,0.1,0.3,\(iso),\(iso),\(contradictoryExact),\(contradictoryExact)
        """

        let result = try ManualAnnotationCSVImporter.importAnnotations(
            contents: csv
        )
        #expect(result.annotations.isEmpty)
        #expect(result.skippedRowCount == 1)
    }
}

@Test
func manualAnnotationCSVRejectsNegativeFractionalTimestampULPContradictions()
    throws {
    let seconds = -123.987_654_321
    let iso = "1969-12-31T23:57:56.012345679Z"

    for contradictorySeconds in [seconds.nextDown, seconds.nextUp] {
        let contradictoryExact = String(
            format: "%.17g",
            locale: Locale(identifier: "en_US_POSIX"),
            contradictorySeconds
        )
        let csv = """
        annotation_id,train_id,polarity,label,start_sec,end_sec,created_at,updated_at,created_at_unix_sec,updated_at_unix_sec
        19000000-0000-0000-0000-000000000001,g,positive,tonic,0.1,0.3,\(iso),\(iso),\(contradictoryExact),\(contradictoryExact)
        """

        let result = try ManualAnnotationCSVImporter.importAnnotations(
            contents: csv
        )
        #expect(result.annotations.isEmpty)
        #expect(result.skippedRowCount == 1)
    }
}

@Test
func manualAnnotationCSVAcceptsLegacyWholeSecondISOWithinDeclaredPrecision() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,created_at,updated_at,created_at_unix_sec,updated_at_unix_sec
    20000000-0000-0000-0000-000000000001,g,positive,tonic,0.1,0.3,1970-01-01T00:01:40Z,1970-01-01T00:03:20Z,100.4,200.4
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let restored = try #require(result.annotations.first)

    #expect(result.skippedRowCount == 0)
    #expect(restored.createdAt == Date(timeIntervalSince1970: 100.4))
    #expect(restored.updatedAt == Date(timeIntervalSince1970: 200.4))
}

@Test
func manualAnnotationCSVRejectsMalformedBlankAndTruncatedExplicitIDs() throws {
    let csv = """
    train_id,label,start_sec,end_sec,annotation_id
    g,tonic,0.1,0.3,not-a-uuid
    g,tonic,0.4,0.6,
    g,tonic,0.7,0.9
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)

    #expect(result.annotations.isEmpty)
    #expect(result.skippedRowCount == 3)
}

@Test
func manualAnnotationCSVRejectsMalformedQuotedExplicitIDs() throws {
    let csv = """
    train_id,label,start_sec,end_sec,annotation_id
    g,tonic,0.1,0.3,10000000-0000"-"0000-0000-000000000001
    g,tonic,0.4,0.6,"10000000-0000-0000-0000-000000000002
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)

    #expect(result.annotations.isEmpty)
    #expect(result.skippedRowCount == 2)
}

@Test
func legacyCSVWithoutAnnotationIDDerivesStableDistinctMigrationIDs() throws {
    let csv = """
    train_id,label,start_sec,end_sec,annotator,annotator_identity_source,created_at_unix_sec,updated_at_unix_sec
    g,tonic,0.1,0.3,Legacy Reviewer,user_provided,100,200
    g,tonic,0.1,0.3,Legacy Reviewer,user_provided,100,200
    """

    let first = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let second = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)

    #expect(first.skippedRowCount == 0)
    #expect(first.annotations.count == 2)
    #expect(first.annotations.map(\.id) == second.annotations.map(\.id))
    #expect(Set(first.annotations.map(\.id)).count == 2)
}

// MARK: - 9. Unknown/unsupported negative labels never behave as active vetoes.

@Test
func unknownNegativeLabelsAreNotImportedAsActiveVetoes() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec
    44444444-4444-4444-4444-444444444444,g,negative,not_hf,0.0,0.2
    55555555-5555-5555-5555-555555555555,g,positive,burst,0.1,0.3
    """
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    // The unknown negative label is reported, not imported as an annotation (so it can never veto).
    #expect(result.unsupportedLabels == ["not_hf"])
    #expect(result.annotations.count == 1)
    #expect(result.annotations.first?.label == .burst)

    // And only `notBurst` is in the consumed-veto set.
    #expect(ManualAnnotationLabel.consumedVetoLabels == [.notBurst])
}

@Test
func explicitPolarityMustMatchTheLabelAuthority() throws {
    let csv = """
    annotation_id,train_id,label,start_sec,end_sec,polarity
    61000000-0000-0000-0000-000000000001,g,tonic,0.1,0.3,negative
    61000000-0000-0000-0000-000000000002,g,not_burst,0.4,0.6,positive
    61000000-0000-0000-0000-000000000003,g,burst,0.7,0.9,sideways
    61000000-0000-0000-0000-000000000004,g,tonic,1.0,1.2,
    61000000-0000-0000-0000-000000000005,g,tonic,1.3,1.5
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )

    #expect(result.annotations.isEmpty)
    #expect(result.unsupportedLabels.isEmpty)
    #expect(result.skippedRowCount == 5)
}

@Test
func legacyCSVWithoutPolarityStillDerivesPolarityFromLabel() throws {
    let csv = """
    annotation_id,train_id,label,start_sec,end_sec
    62000000-0000-0000-0000-000000000001,g,tonic,0.1,0.3
    62000000-0000-0000-0000-000000000002,g,not_burst,0.4,0.6
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )

    #expect(result.skippedRowCount == 0)
    #expect(result.annotations.map(\.polarity) == [.positive, .negative])
}

@Test
func manualAnnotationCSVRejectsDuplicateHeadersBeforeResolvingAuthority() {
    let cases: [(column: String, csv: String)] = [
        (
            "label",
            """
            annotation_id,train_id,label,label,start_sec,end_sec
            63000000-0000-0000-0000-000000000001,g,tonic,not_burst,0.1,0.3
            """
        ),
        (
            "annotation_id",
            """
            annotation_id,annotation_id,train_id,label,start_sec,end_sec
            63000000-0000-0000-0000-000000000002,63000000-0000-0000-0000-000000000003,g,tonic,0.1,0.3
            """
        ),
        (
            "polarity",
            """
            annotation_id,train_id,label,start_sec,end_sec,polarity,polarity
            63000000-0000-0000-0000-000000000004,g,tonic,0.1,0.3,positive,negative
            """
        ),
        (
            "annotator_identity_source",
            """
            annotation_id,train_id,label,start_sec,end_sec,annotator,annotator_identity_source,annotator_identity_source
            63000000-0000-0000-0000-000000000005,g,tonic,0.1,0.3,Reviewer,user_provided,imported
            """
        ),
        (
            "created_at",
            """
            annotation_id,train_id,label,start_sec,end_sec,created_at,created_at
            63000000-0000-0000-0000-000000000006,g,tonic,0.1,0.3,1970-01-01T00:01:40Z,1970-01-01T00:03:20Z
            """
        ),
        (
            "note",
            """
            annotation_id,train_id,label,start_sec,end_sec,note,note
            63000000-0000-0000-0000-000000000007,g,tonic,0.1,0.3,first,second
            """
        ),
    ]

    for testCase in cases {
        do {
            _ = try ManualAnnotationCSVImporter.importAnnotations(
                contents: testCase.csv
            )
            Issue.record(
                "Expected duplicate header \(testCase.column) to fail closed."
            )
        } catch ManualAnnotationCSVImporter.ImportError.duplicateColumn(
            let column
        ) {
            #expect(column == testCase.column)
        } catch {
            Issue.record(
                "Expected duplicateColumn(\(testCase.column)), got \(error)."
            )
        }
    }
}

@Test
func importToleratesMissingOptionalIndexColumns() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,note
    66666666-6666-6666-6666-666666666666,g,positive,pause,0.2,0.4,
    """
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let imported = try #require(result.annotations.first)
    #expect(imported.label == .pause)
    #expect(imported.startISIIndex == nil)   // recomputed later when a dataset is available
    #expect(imported.startSec == 0.2)
    #expect(imported.endSec == 0.4)
}

@Test
func legacyCSVWithAnnotatorDefaultsIdentitySourceToImported() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,annotator
    77777777-7777-7777-7777-777777777777,g,positive,tonic,0.2,0.4,Legacy Reviewer
    """
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let imported = try #require(result.annotations.first)

    #expect(imported.annotator == "Legacy Reviewer")
    #expect(imported.annotatorIdentitySource == .imported)
}

@Test
func explicitUnrecognizedAnnotatorIdentitySourceRemainsUnknown() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,annotator,annotator_identity_source
    99999999-9999-9999-9999-999999999999,g,positive,tonic,0.1,0.3,Named Reviewer,unrecognized_source
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let imported = try #require(result.annotations.first)

    #expect(imported.annotator == "Named Reviewer")
    #expect(imported.annotatorIdentitySource == .unknown)
}

@Test
func explicitBlankAnnotatorIdentitySourceRemainsUnknown() throws {
    let csv = """
    annotation_id,train_id,polarity,label,start_sec,end_sec,annotator,annotator_identity_source
    aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa,g,positive,tonic,0.1,0.3,Named Reviewer,
    """

    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    let imported = try #require(result.annotations.first)

    #expect(imported.annotator == "Named Reviewer")
    #expect(imported.annotatorIdentitySource == .unknown)
}

// MARK: - Label-aware geometry: fast-pattern labels require full ISI containment

// Spikes 0, 0.010, 0.014, 0.020, 0.030 -> ISI1=0.010, ISI2=0.004, ISI3=0.006, ISI4=0.010.
private let packetTrain = SpikeTrain(name: "p", timestampsSec: [0, 0.010, 0.014, 0.020, 0.030])

@Test
func burstDrawnStrictlyInsideOneISIDoesNotResolve() {
    // Narrow window [0.011,0.013] sits strictly inside ISI2 [0.010,0.014]: no fully-contained ISI.
    let mark = annotation(train: "p", label: .burst, start: 0.011, end: 0.013)
    #expect(!ManualAnnotationGeometryResolver.resolve(annotation: mark, in: packetTrain).isWithinTrain)
    #expect(ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(mark, in: [packetTrain]) == nil)
}

@Test
func tonicKeepsOverlapFallbackForNarrowWindow() {
    let mark = annotation(train: "p", label: .tonic, start: 0.011, end: 0.013)
    let geo = ManualAnnotationGeometryResolver.resolve(annotation: mark, in: packetTrain)
    #expect(geo.isWithinTrain)
    #expect(geo.coveredISIIndices == 2...2)   // overlap fallback includes ISI2
}

@Test
func pauseKeepsOverlapFallbackForNarrowWindow() {
    let mark = annotation(train: "p", label: .pause, start: 0.011, end: 0.013)
    let geo = ManualAnnotationGeometryResolver.resolve(annotation: mark, in: packetTrain)
    #expect(geo.isWithinTrain)
    #expect(geo.coveredISIIndices == 2...2)
}

@Test
func burstExtendingBeyondPacketResolvesAllContainedISIs() {
    // Drag slightly before spike 0.010 and after spike 0.020 -> ISI2 and ISI3 fully contained.
    let mark = annotation(train: "p", label: .burst, start: 0.008, end: 0.022)
    let geo = ManualAnnotationGeometryResolver.resolve(annotation: mark, in: packetTrain)
    #expect(geo.isWithinTrain)
    #expect(geo.coveredISIIndices == 2...3)
    #expect(geo.startSpikeIndex == 1)
    #expect(geo.endSpikeIndex == 3)
}

@Test
func allFastPatternLabelsRequireFullContainment() {
    let fastLabels: [ManualAnnotationLabel] = [.burst, .longBurst, .notBurst, .highFrequencyTonic, .highFrequencySpiking]
    for label in fastLabels {
        #expect(label.geometryResolutionPolicy == .fullyContainedISIOnly)
        // Strictly inside one ISI -> not resolved for any fast-pattern label.
        let narrow = annotation(train: "p", label: label, start: 0.011, end: 0.013)
        #expect(!ManualAnnotationGeometryResolver.resolve(annotation: narrow, in: packetTrain).isWithinTrain, "\(label.rawValue) must require full containment")
        // Extended beyond the packet -> resolves the fully-contained ISIs.
        let extended = annotation(train: "p", label: label, start: 0.008, end: 0.022)
        #expect(ManualAnnotationGeometryResolver.resolve(annotation: extended, in: packetTrain).coveredISIIndices == 2...3)
    }
}

@Test
func slowLabelsKeepContainedThenOverlapPolicy() {
    for label in [ManualAnnotationLabel.tonic, .pause, .other] {
        #expect(label.geometryResolutionPolicy == .containedThenOverlapFallback)
    }
}

@Test
func coveredSpikeIndicesMatchResolvedRangeForLiveHighlight() {
    // The live drag spike-highlight highlights `coveredSpikeIndices`: for ISI range s...e that is
    // (s-1)...e. Extended burst -> ISI 2...3 -> spikes 1...3.
    let burst = annotation(train: "p", label: .burst, start: 0.008, end: 0.022)
    #expect(ManualAnnotationGeometryResolver.resolve(annotation: burst, in: packetTrain).coveredSpikeIndices == 1...3)
    // Overlap-fallback tonic over one ISI (ISI 2...2) -> spikes 1...2.
    let tonic = annotation(train: "p", label: .tonic, start: 0.011, end: 0.013)
    #expect(ManualAnnotationGeometryResolver.resolve(annotation: tonic, in: packetTrain).coveredSpikeIndices == 1...2)
    // Invalid fast-pattern (too short) -> no covered spikes -> no highlight.
    let narrowBurst = annotation(train: "p", label: .burst, start: 0.011, end: 0.013)
    #expect(ManualAnnotationGeometryResolver.resolve(annotation: narrowBurst, in: packetTrain).coveredSpikeIndices == nil)
}

// MARK: - Import / restore compatibility guard (Phase 1B follow-up)

@Test
func compatibleGeometryResolvesOnlyForMatchingTrainWithinSpan() {
    let trains = [fiveSpikeTrain]   // id "g", spikes 0..0.4

    // Matching train + in-span window -> resolved with cached indices.
    let good = annotation(train: "g", label: .burst, start: 0.1, end: 0.3)
    let resolved = ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(good, in: trains)
    #expect(resolved?.startISIIndex == 2)
    #expect(resolved?.endISIIndex == 3)

    // Wrong train id -> nil (the ghost case: a CSV from another dataset).
    let wrongTrain = annotation(train: "other", label: .burst, start: 0.1, end: 0.3)
    #expect(ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(wrongTrain, in: trains) == nil)

    // Matching train but the time range is outside the train -> nil.
    let outside = annotation(train: "g", label: .burst, start: 9.0, end: 9.5)
    #expect(ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(outside, in: trains) == nil)

    // No trains at all -> nil.
    #expect(ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(good, in: []) == nil)
}

// MARK: - Dataset lifecycle isolation (P1 follow-up)

@Test
func datasetKeyIsStableAndSeparatesOrdinaryMetadata() {
    let a1 = ManualAnnotationDatasetKey.make(name: "A", sourceDescription: "src-a")
    let a2 = ManualAnnotationDatasetKey.make(name: "A", sourceDescription: "src-a")
    let b = ManualAnnotationDatasetKey.make(name: "B", sourceDescription: "src-a")
    let aOtherSource = ManualAnnotationDatasetKey.make(name: "A", sourceDescription: "src-b")
    // Same dataset -> same key (annotations restore on reload / rerun).
    #expect(a1 == a2)
    // Different name OR different source -> different key.
    #expect(a1 != b)
    #expect(a1 != aOtherSource)
}

@Test
func datasetKeyMatchesDatasetOverload() {
    let dataset = SpikeDataset(name: "A", sourceDescription: "src", trains: [fiveSpikeTrain])
    #expect(ManualAnnotationDatasetKey.make(for: dataset) == ManualAnnotationDatasetKey.make(name: "A", sourceDescription: "src"))
}

@Test
func overlappingPositiveAnnotationsUseLatestEditNotCallerArrayOrder() {
    let older = annotation(
        label: .tonic, start: 0.1, end: 0.3,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let newer = annotation(
        label: .pause, start: 0.1, end: 0.3,
        updatedAt: Date(timeIntervalSince1970: 300)
    )

    for annotations in [[newer, older], [older, newer]] {
        let projection = ManualAnnotationProjector.project(
            train: fiveSpikeTrain, autoLabelsByISI: [:], annotations: annotations,
            honorManualLock: true, manualNegativeLabelsEnabled: true
        )
        #expect(projection.finalLabelByISI[2] == "pause")
        #expect(projection.finalLabelByISI[3] == "pause")
    }
}

@Test
func overlapArbitrationCannotLeaveSingleISIBurstFragment() {
    let olderBurst = annotation(
        label: .burst, start: 0.1, end: 0.3,  // ISI 2...3: initially valid
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let newerTonic = annotation(
        label: .tonic, start: 0.2, end: 0.3,  // owns ISI 3, leaving burst ISI 2 alone
        updatedAt: Date(timeIntervalSince1970: 300)
    )
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: [2: "pause"],
        annotations: [newerTonic, olderBurst],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )

    #expect(projection.manualPositiveLabelByISI[2] == nil)
    #expect(projection.finalLabelByISI[2] == "pause")
    #expect(projection.manualPositiveLabelByISI[3] == "tonic")
    #expect(projection.finalLabelByISI[3] == "tonic")
    // ISI 3 has no auto label in this fixture, so the positive manual lock has
    // nothing to suppress even though tonic owns the final label there.
    #expect(projection.autoBlockedByManualLockISIs.isEmpty)
    #expect(projection.skippedAnnotationCount == 1) // burst lost its only post-overlap fragment
}

@Test
func sameAnnotationIDUsesLatestCrossTrainVersionIndependentOfCallerOrder() {
    let sharedID = UUID(uuidString: "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")!
    let oldLocal = annotation(
        train: "g", label: .tonic, start: 0.1, end: 0.3, id: sharedID,
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let movedAway = annotation(
        train: "other", label: .pause, start: 0.1, end: 0.3, id: sharedID,
        updatedAt: Date(timeIntervalSince1970: 300)
    )

    for annotations in [[oldLocal, movedAway], [movedAway, oldLocal]] {
        let projection = ManualAnnotationProjector.project(
            train: fiveSpikeTrain,
            autoLabelsByISI: [2: "burst"],
            annotations: annotations,
            honorManualLock: true,
            manualNegativeLabelsEnabled: true
        )
        #expect(projection.manualPositiveLabelByISI.isEmpty)
        #expect(projection.finalLabelByISI[2] == "burst")
        #expect(projection.skippedAnnotationCount == 2) // superseded old version + latest wrong-train version
    }
}

@Test
func overlappingPositiveAnnotationTimestampTiesResolveDeterministicallyByID() {
    let earlyID = annotation(
        label: .tonic, start: 0.1, end: 0.3,
        id: UUID(uuidString: "00000000-0000-0000-0000-000000000001")!
    )
    let lateID = annotation(
        label: .pause, start: 0.1, end: 0.3,
        id: UUID(uuidString: "00000000-0000-0000-0000-000000000002")!
    )
    let forward = ManualAnnotationProjector.project(
        train: fiveSpikeTrain, autoLabelsByISI: [:], annotations: [earlyID, lateID],
        honorManualLock: true, manualNegativeLabelsEnabled: true
    )
    let reversed = ManualAnnotationProjector.project(
        train: fiveSpikeTrain, autoLabelsByISI: [:], annotations: [lateID, earlyID],
        honorManualLock: true, manualNegativeLabelsEnabled: true
    )
    #expect(forward == reversed)
    #expect(forward.finalLabelByISI[2] == "pause")
}

@Test
func singleISIBurstManualAnnotationIsSkippedButSingleISITonicIsAllowed() {
    let burst = annotation(label: .burst, start: 0.1, end: 0.2) // ISI 2 only
    let tonic = annotation(label: .tonic, start: 0.2, end: 0.3) // ISI 3 only
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain, autoLabelsByISI: [:], annotations: [burst, tonic],
        honorManualLock: true, manualNegativeLabelsEnabled: true
    )
    #expect(projection.finalLabelByISI[2] == nil)
    #expect(projection.finalLabelByISI[3] == "tonic")
    #expect(projection.skippedAnnotationCount == 1)
}

@Test
func singleManualBurstISICanExtendAdjacentAutomaticBurst() {
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain,
        autoLabelsByISI: [2: "burst", 3: "burst"],
        annotations: [annotation(label: .burst, start: 0.3, end: 0.4)], // ISI 4 only
        honorManualLock: true,
        manualNegativeLabelsEnabled: true
    )

    #expect(projection.manualPositiveLabelByISI[4] == "burst")
    #expect(projection.finalLabelByISI[2] == "burst")
    #expect(projection.finalLabelByISI[3] == "burst")
    #expect(projection.finalLabelByISI[4] == "burst")
    #expect(projection.skippedAnnotationCount == 0)
    #expect(projection.finalBurstISIsRemovedByStructuralMinimum.isEmpty)
}

@Test
func projectorAppliesQCFloorBeforeBurstMinimum() {
    let train = SpikeTrain(name: "qc", timestampsSec: [0, 0.0005, 0.0105])
    let burst = annotation(
        train: train.id,
        label: .burst,
        start: 0,
        end: 0.0105
    )
    let projection = ManualAnnotationProjector.project(
        train: train,
        autoLabelsByISI: [:],
        annotations: [burst],
        honorManualLock: true,
        manualNegativeLabelsEnabled: true,
        minValidISISeconds: 0.001
    )

    #expect(projection.manualPositiveLabelByISI.isEmpty)
    #expect(projection.finalLabelByISI.isEmpty)
    #expect(projection.skippedAnnotationCount == 1)
    #expect(projection.finalBurstISIsRemovedByStructuralMinimum == [2])
}

@Test
func projectorSkipsAndCountsWrongTrainAnnotations() {
    let auto: [Int: String] = [2: "burst", 3: "tonic"]
    let mixed = [
        annotation(train: "g", label: .tonic, start: 0.1, end: 0.3),       // belongs to "g"
        annotation(train: "other", label: .burst, start: 0.1, end: 0.3)    // wrong train
    ]
    let projection = ManualAnnotationProjector.project(
        train: fiveSpikeTrain, autoLabelsByISI: auto, annotations: mixed,
        honorManualLock: true, manualNegativeLabelsEnabled: true
    )
    // Only the matching-train annotation is projected; the wrong-train one is skipped AND counted.
    #expect(projection.manualPositiveLabelByISI[2] == "tonic")
    #expect(projection.skippedAnnotationCount == 1)
    // The wrong-train burst annotation must NOT veto or label anything on this train.
    #expect(projection.manualNegativeVetoByISI.isEmpty)
    #expect(projection.finalLabelByISI[2] == "tonic")
}

// MARK: - Raster time-axis conversion (Phase 1B)

@Test
func rasterTimeConversionRawModeIsIdentity() {
    #expect(ManualAnnotationTimeConversion.rawSec(displaySec: 12.5, firstTimestampSec: 4.0, aligned: false) == 12.5)
    #expect(ManualAnnotationTimeConversion.displaySec(rawSec: 12.5, firstTimestampSec: 4.0, aligned: false) == 12.5)
}

@Test
func rasterTimeConversionAlignedModeShiftsByFirstTimestamp() {
    // Aligned display 8.5 on a train whose first raw spike is at 4.0 => raw 12.5.
    #expect(ManualAnnotationTimeConversion.rawSec(displaySec: 8.5, firstTimestampSec: 4.0, aligned: true) == 12.5)
    #expect(ManualAnnotationTimeConversion.displaySec(rawSec: 12.5, firstTimestampSec: 4.0, aligned: true) == 8.5)
}

@Test
func rasterTimeConversionRoundTrips() {
    for aligned in [true, false] {
        let raw = ManualAnnotationTimeConversion.rawSec(displaySec: 3.25, firstTimestampSec: 1.75, aligned: aligned)
        let backToDisplay = ManualAnnotationTimeConversion.displaySec(rawSec: raw, firstTimestampSec: 1.75, aligned: aligned)
        #expect(abs(backToDisplay - 3.25) < 1e-12)
    }
}

// MARK: - Candidate -> manual label mapping (Phase 1B)

@Test
func manualLabelMapsFromAutoLabel() {
    #expect(ManualAnnotationLabel(autoLabel: .burst) == .burst)
    #expect(ManualAnnotationLabel(autoLabel: .longBurst) == .longBurst)
    #expect(ManualAnnotationLabel(autoLabel: .tonic) == .tonic)
    #expect(ManualAnnotationLabel(autoLabel: .highFrequencyTonic) == .highFrequencyTonic)
    #expect(ManualAnnotationLabel(autoLabel: .highFrequencySpiking) == .highFrequencySpiking)
    #expect(ManualAnnotationLabel(autoLabel: .pause) == .pause)
    // Burst-family subtypes fold to the positive burst manual label.
    #expect(ManualAnnotationLabel(autoLabel: .highFrequencyBurst) == .burst)
    #expect(ManualAnnotationLabel(autoLabel: .possibleBurst) == .burst)
    // Non-event labels do not map (caller chooses a fallback / disables the action).
    #expect(ManualAnnotationLabel(autoLabel: .profile) == nil)
    #expect(ManualAnnotationLabel(autoLabel: .reject) == nil)
    // Every mapped label is positive (the candidate->manual action only authors positives).
    #expect(ManualAnnotationLabel(autoLabel: .burst)?.polarity == .positive)
}

// MARK: - Model invariants

@Test
func polarityIsDerivedFromLabel() {
    #expect(ManualAnnotationLabel.burst.polarity == .positive)
    #expect(ManualAnnotationLabel.notBurst.polarity == .negative)
    #expect(ManualAnnotationLabel.notBurst.finalPatternString == nil)
    #expect(ManualAnnotationLabel.tonic.finalPatternString == "tonic")
    #expect(annotation(label: .notBurst, start: 0, end: 0.1).polarity == .negative)
}
