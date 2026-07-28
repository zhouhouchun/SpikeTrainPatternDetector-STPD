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

// MARK: - Phase 2.2C-A: dataset-identity binding and fail-closed authority gating

private let dqcDatasetA = SpikeDataset(
    name: "dqc-a", sourceDescription: "",
    trains: [SpikeTrain(name: "g", timestampsSec: [0, 0.1, 0.2, 0.3, 0.4])]
)
private let dqcDatasetB = SpikeDataset(
    name: "dqc-b", sourceDescription: "",
    trains: [SpikeTrain(name: "g", timestampsSec: [0, 0.12, 0.24, 0.36, 0.48])]
)

private func dqcDigest(_ dataset: SpikeDataset) -> String {
    DetectionDatasetSnapshot.make(dataset: dataset).digest
}

private func dqcBoundExport(
    _ annotations: [ManualAnnotation],
    dataset: SpikeDataset = dqcDatasetA,
    runID: String? = nil,
    reviewState: ManualAnnotationCSVReviewState = .pendingConfirmation
) -> String {
    ManualAnnotationCSVExporter.csv(
        annotations: annotations,
        identity: .forDataset(dataset, runID: runID, reviewState: reviewState)
    )
}

private func dqcAnnotation(
    train: String = "g", startISI: Int? = 2, endISI: Int? = 3, start: Double = 0.1, end: Double = 0.3
) -> ManualAnnotation {
    ManualAnnotation(
        trainID: train, label: .tonic, startSec: start, endSec: end,
        startISIIndex: startISI, endISIIndex: endISI,
        createdAt: Date(timeIntervalSince1970: 100), updatedAt: Date(timeIntervalSince1970: 100)
    )
}

// Minimal identity-bound header (no optional timestamp/annotator columns) for hand-built envelope
// edge-case CSVs; avoids the importer's declared-but-blank-timestamp skip.
private let dqcMinimalHeader = [
    "train_id", "label", "start_sec", "end_sec", "start_isi_index", "end_isi_index",
    "schema_version", "dataset_digest", "run_id", "review_state",
]
private func dqcMinimalRow(schema: String, digest: String, run: String, review: String) -> [String] {
    ["g", ManualAnnotationLabel.tonic.rawValue, "0.1", "0.3", "2", "3", schema, digest, run, review]
}
private func dqcMinimalCSV(_ rows: [[String]], separator: String = "\r\n") -> String {
    ([dqcMinimalHeader] + rows).map { $0.joined(separator: ",") }.joined(separator: separator) + separator
}

// A single data row shaped to `dqcMinimalHeader` (10 columns), fully parameterized so eligibility
// blockers can be isolated (e.g. a skipped-but-CSV-valid row, an unsupported label, a wrong train).
private func dqcRow(
    train: String = "g",
    label: String = "tonic",
    start: String = "0.1",
    end: String = "0.3",
    startISI: String = "2",
    endISI: String = "3",
    schema: String = ManualAnnotationCSVExporter.identitySchemaVersion,
    digest: String,
    run: String = "",
    review: String = "pending_confirmation"
) -> [String] {
    [train, label, start, end, startISI, endISI, schema, digest, run, review]
}

// A valid, auditable approval whose file + dataset digests match the gated import by construction.
private func dqcApproval(
    _ gated: ManualAnnotationCSVGatedImport,
    approver: String = "Dr. Reviewer",
    at: Date = Date(timeIntervalSince1970: 1_000)
) -> ManualAnnotationCSVApproval {
    ManualAnnotationCSVApproval(
        sourceFileDigest: gated.sourceFileDigest,
        activeDatasetDigest: gated.activeDatasetDigest,
        approver: approver,
        approvedAt: at
    )!
}

@Test func dqcIdentityBoundRoundTrip() throws {
    let csv = dqcBoundExport([dqcAnnotation()], runID: "run_x")
    #expect(csv.contains("schema_version,dataset_digest,run_id,review_state"))
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.annotations.count == 1)
    #expect(imported.envelope.schemaVersion == ManualAnnotationCSVExporter.identitySchemaVersion)
    #expect(imported.envelope.datasetDigest == dqcDigest(dqcDatasetA))
    #expect(imported.envelope.runID == "run_x")
    guard case .bound(let digest) = imported.fileIdentity else { Issue.record("expected .bound"); return }
    #expect(digest == dqcDigest(dqcDatasetA))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    let authoritative = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    #expect(authoritative.count == 1)
    #expect(authoritative[0].trainID == "g")
    #expect(authoritative[0].label == .tonic)
}

@Test func dqcCollisionSameTrainAndISIButDifferentDatasetIsNeverAuthoritative() throws {
    // Same train_id "g" and same ISI indices in BOTH datasets, but the digests differ.
    #expect(dqcDigest(dqcDatasetA) != dqcDigest(dqcDatasetB))
    let csv = dqcBoundExport([dqcAnnotation(startISI: 2, endISI: 3)], dataset: dqcDatasetA)
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.annotations[0].trainID == "g")
    #expect(imported.annotations[0].startISIIndex == 2)
    // Import (author bound to A) against B: same train_id, same ISI indices, DIFFERENT dataset.
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetB)
    #expect(gated.identity == .datasetMismatch)
    #expect(gated.authority == .reviewOnly)
    #expect(throws: ManualAnnotationCSVAuthorityError.self) { _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated)) }
}

@Test func dqcMatchingDigestRemainsUnappliedUntilExplicitApproval() throws {
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcBoundExport([dqcAnnotation()]))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)   // eligible, NOT auto-applied
    // Only the explicit, auditable approval call promotes — there is no no-argument promotion.
    #expect(try gated.authoritativeAnnotations(approval: dqcApproval(gated)).count == 1)
}

@Test func dqcLegacyFileWithoutIdentityColumnsIsReviewOnly() throws {
    let csv = ManualAnnotationCSVExporter.csv(annotations: [dqcAnnotation()])  // legacy 17-column
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.annotations.count == 1)          // still parses
    #expect(imported.fileIdentity == .legacyUnbound)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .legacyUnbound)
    #expect(gated.authority == .reviewOnly)
    #expect(throws: ManualAnnotationCSVAuthorityError.self) { _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated)) }
}

@Test func dqcDeclaredButBlankDatasetDigestIsMalformedNotLegacy() throws {
    let row = dqcMinimalRow(schema: ManualAnnotationCSVExporter.identitySchemaVersion, digest: "", run: "", review: "")
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV([row]))
    #expect(imported.fileIdentity == .malformedIdentity)   // NOT .legacyUnbound
    #expect(ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA).authority == .reviewOnly)
}

@Test func dqcInconsistentDatasetDigestAcrossRowsIsMalformed() throws {
    let schema = ManualAnnotationCSVExporter.identitySchemaVersion
    let rows = [
        dqcMinimalRow(schema: schema, digest: dqcDigest(dqcDatasetA), run: "", review: ""),
        dqcMinimalRow(schema: schema, digest: dqcDigest(dqcDatasetB), run: "", review: ""),
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    #expect(imported.fileIdentity == .malformedIdentity)
    #expect(ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA).authority == .reviewOnly)
}

@Test func dqcInconsistentGovernanceColumnsFailClosed() throws {
    let schema = ManualAnnotationCSVExporter.identitySchemaVersion
    let digest = dqcDigest(dqcDatasetA)
    func classify(_ rows: [[String]]) throws -> ManualAnnotationCSVFileIdentity {
        try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows)).fileIdentity
    }
    // Inconsistent run_id.
    #expect(try classify([
        dqcMinimalRow(schema: schema, digest: digest, run: "run_a", review: "pending_confirmation"),
        dqcMinimalRow(schema: schema, digest: digest, run: "run_b", review: "pending_confirmation"),
    ]) == .malformedIdentity)
    // Inconsistent review_state.
    #expect(try classify([
        dqcMinimalRow(schema: schema, digest: digest, run: "run_a", review: "pending_confirmation"),
        dqcMinimalRow(schema: schema, digest: digest, run: "run_a", review: "review_only"),
    ]) == .malformedIdentity)
    // Inconsistent schema_version.
    #expect(try classify([
        dqcMinimalRow(schema: schema, digest: digest, run: "run_a", review: "pending_confirmation"),
        dqcMinimalRow(schema: "manual_annotation_csv_v9", digest: digest, run: "run_a", review: "pending_confirmation"),
    ]) == .malformedIdentity)
}

@Test func dqcUnknownSchemaVersionIsUnsupportedAndReviewOnly() throws {
    let row = dqcMinimalRow(schema: "manual_annotation_csv_v9", digest: dqcDigest(dqcDatasetA), run: "", review: "")
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV([row]))
    #expect(imported.fileIdentity == .unsupportedSchema)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .unsupportedSchema)
    #expect(gated.authority == .reviewOnly)
    #expect(throws: ManualAnnotationCSVAuthorityError.self) { _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated)) }
}

@Test func dqcHandlesLeadingBOM() throws {
    let csv = "\u{FEFF}" + dqcBoundExport([dqcAnnotation()])
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.annotations.count == 1)
    guard case .bound = imported.fileIdentity else { Issue.record("BOM broke identity"); return }
    #expect(imported.envelope.datasetDigest == dqcDigest(dqcDatasetA))
}

@Test func dqcHandlesCRLFRecords() throws {
    let row = dqcMinimalRow(
        schema: ManualAnnotationCSVExporter.identitySchemaVersion,
        digest: dqcDigest(dqcDatasetA), run: "run_x", review: "pending_confirmation"
    )
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV([row], separator: "\r\n"))
    #expect(imported.annotations.count == 1)
    guard case .bound = imported.fileIdentity else { Issue.record("CRLF broke identity"); return }
}

@Test func dqcHandlesLoneCRRecords() throws {
    let row = dqcMinimalRow(
        schema: ManualAnnotationCSVExporter.identitySchemaVersion,
        digest: dqcDigest(dqcDatasetA), run: "run_x", review: "pending_confirmation"
    )
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV([row], separator: "\r"))
    #expect(imported.annotations.count == 1)
    guard case .bound = imported.fileIdentity else { Issue.record("lone-CR broke identity"); return }
}

@Test func dqcDuplicateIdentityColumnIsRejected() throws {
    let header = dqcMinimalHeader + ["dataset_digest"]
    let row = dqcMinimalRow(
        schema: ManualAnnotationCSVExporter.identitySchemaVersion,
        digest: dqcDigest(dqcDatasetA), run: "", review: ""
    ) + [dqcDigest(dqcDatasetA)]
    let csv = ([header] + [row]).map { $0.joined(separator: ",") }.joined(separator: "\r\n") + "\r\n"
    #expect(throws: ManualAnnotationCSVImporter.ImportError.self) {
        _ = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    }
}

@Test func dqcMatchingIdentityButIncompatibleGeometryCannotBecomeEligible() throws {
    // Matching digest, but time bounds are outside the train => geometry-incompatible.
    let csv = dqcBoundExport([dqcAnnotation(startISI: nil, endISI: nil, start: 5.0, end: 6.0)])
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)   // identity matches...
    #expect(gated.authority == .reviewOnly)       // ...but geometry gate keeps it review-only
    #expect(throws: ManualAnnotationCSVAuthorityError.self) { _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated)) }
}

@Test func dqcReviewStateCannotBypassExplicitApproval() throws {
    let csv = dqcBoundExport([dqcAnnotation()], reviewState: .pendingConfirmation)
    // Matching bound file: eligible only, NOT auto-authoritative — authority needs the explicit call.
    let matching = ManualAnnotationCSVImporter.gate(
        try ManualAnnotationCSVImporter.importIdentityBound(contents: csv), activeDataset: dqcDatasetA
    )
    #expect(matching.authority == .eligibleAfterExplicitConfirmation)
    // Same review_state against a mismatched dataset grants nothing.
    let mismatched = ManualAnnotationCSVImporter.gate(
        try ManualAnnotationCSVImporter.importIdentityBound(contents: csv), activeDataset: dqcDatasetB
    )
    #expect(mismatched.identity == .datasetMismatch)
    #expect(mismatched.authority == .reviewOnly)
    #expect(throws: ManualAnnotationCSVAuthorityError.self) { _ = try mismatched.authoritativeAnnotations(approval: dqcApproval(mismatched)) }
}

// MARK: - Phase 2.2C-A.1: authority-gap corrections (A–G)

// Correction A: every physical data row participates in file-level identity validation. A single
// syntactically invalid data row (malformed quoting that could hide a conflicting dataset_digest)
// fails the whole file closed as malformedIdentity — it can never be a clean bound file.
@Test func dqcA_invalidDataRowPoisonsIdentityBoundFile() throws {
    let digest = dqcDigest(dqcDatasetA)
    let schema = ManualAnnotationCSVExporter.identitySchemaVersion
    let header = dqcMinimalHeader.joined(separator: ",")
    let goodRow = dqcRow(digest: digest).joined(separator: ",")
    // The stray quote after the non-empty `g` field makes this row syntactically invalid.
    let brokenRow = ["g\"broken", "tonic", "0.1", "0.3", "2", "3", schema, digest, "", "pending_confirmation"]
        .joined(separator: ",")
    let csv = ([header, goodRow, brokenRow]).joined(separator: "\r\n") + "\r\n"

    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.fileIdentity == .malformedIdentity)   // poisoned by the invalid row
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .malformedIdentity)
    #expect(gated.authority == .reviewOnly)
    #expect(gated.blockers.contains(.malformedIdentity))
    #expect(throws: ManualAnnotationCSVAuthorityError.self) {
        _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    }
}

// Correction B: a matching, geometry-compatible file whose importer SKIPPED a (CSV-valid) row cannot
// be authoritative — partial parsing may never become authoritative. The reason is a typed blocker.
@Test func dqcB_skippedRowBlocksEligibilityWithTypedBlocker() throws {
    let digest = dqcDigest(dqcDatasetA)
    let rows = [
        dqcRow(digest: digest),                       // one good tonic annotation (keeps import non-empty)
        dqcRow(start: "nan", digest: digest),         // CSV-valid but non-finite bounds -> importer skips
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    #expect(imported.skippedRowCount == 1)
    #expect(imported.annotations.count == 1)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)        // identity + geometry are fine...
    #expect(gated.authority == .reviewOnly)            // ...but a skipped row blocks eligibility
    #expect(gated.blockers.contains(.skippedRows(1)))
    #expect(throws: ManualAnnotationCSVAuthorityError.self) {
        _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    }
}

// Correction B: an unsupported label row (kept out of the active set) is partial-parse loss and blocks
// eligibility via a typed blocker, even though identity matches and the supported annotation resolves.
@Test func dqcB_unsupportedLabelBlocksEligibilityWithTypedBlocker() throws {
    let digest = dqcDigest(dqcDatasetA)
    let rows = [
        dqcRow(digest: digest),                        // supported tonic
        dqcRow(label: "not_hf", digest: digest),       // unknown label -> unsupportedLabels
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    #expect(imported.unsupportedLabels == ["not_hf"])
    #expect(imported.annotations.count == 1)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)
    #expect(gated.authority == .reviewOnly)
    #expect(gated.blockers.contains(.unsupportedLabels(["not_hf"])))
}

// Correction C: `review_only` is PERMANENTLY review-only — a matching, geometry-compatible file with
// review_state=review_only can never be promoted, even by an otherwise-matching explicit approval.
@Test func dqcC_reviewOnlyStateIsPermanentlyReviewOnly() throws {
    let csv = dqcBoundExport([dqcAnnotation()], reviewState: .reviewOnly)
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)
    #expect(gated.authority == .reviewOnly)
    #expect(gated.blockers.contains(.reviewOnlyState))
    do {
        _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
        Issue.record("review_only import must never be promotable")
    } catch ManualAnnotationCSVAuthorityError.notEligibleForAuthority {
        // expected: explicit confirmation cannot promote a review_only import
    } catch {
        Issue.record("expected notEligibleForAuthority, got \(error)")
    }
}

// Correction C: a bound file that OMITS the review_state column defaults to review_only (never eligible).
@Test func dqcC_missingReviewStateDefaultsToReviewOnly() throws {
    let schema = ManualAnnotationCSVExporter.identitySchemaVersion
    let digest = dqcDigest(dqcDatasetA)
    // Identity-bound header WITHOUT a review_state column (schema_version + dataset_digest still bind it).
    let header = ["train_id", "label", "start_sec", "end_sec", "start_isi_index", "end_isi_index",
                  "schema_version", "dataset_digest", "run_id"]
    let row = ["g", "tonic", "0.1", "0.3", "2", "3", schema, digest, ""]
    let csv = ([header, row]).map { $0.joined(separator: ",") }.joined(separator: "\r\n") + "\r\n"

    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    guard case .bound = imported.fileIdentity else { Issue.record("expected .bound"); return }
    #expect(imported.envelope.reviewState == nil)      // column genuinely absent
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)
    #expect(gated.authority == .reviewOnly)            // missing review_state == review_only
    #expect(gated.blockers.contains(.reviewOnlyState))
}

// Correction D: a valid, auditable approval whose digests match promotes an eligible import and returns
// exactly the geometry-resolved annotations.
@Test func dqcD_matchingApprovalPromotesEligibleImport() throws {
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcBoundExport([dqcAnnotation()]))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    let authoritative = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    #expect(authoritative.count == 1)
    #expect(authoritative[0].label == .tonic)
    #expect(authoritative[0].trainID == "g")
}

// Correction D: an approval whose source-file digest does not match the gated import throws
// (an approval of a DIFFERENT file can never promote this one), even when everything else is eligible.
@Test func dqcD_mismatchedSourceFileDigestThrows() throws {
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcBoundExport([dqcAnnotation()]))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    let wrongFileApproval = ManualAnnotationCSVApproval(
        sourceFileDigest: String(repeating: "0", count: 64),   // nonempty but wrong
        activeDatasetDigest: gated.activeDatasetDigest,
        approver: "Dr. Reviewer",
        approvedAt: Date(timeIntervalSince1970: 1_000)
    )!
    do {
        _ = try gated.authoritativeAnnotations(approval: wrongFileApproval)
        Issue.record("mismatched source-file digest must throw")
    } catch ManualAnnotationCSVAuthorityError.sourceFileDigestMismatch {
        // expected
    } catch {
        Issue.record("expected sourceFileDigestMismatch, got \(error)")
    }
}

// Correction D: an approval whose active-dataset digest does not match the gated import throws
// (approving against a DIFFERENT dataset can never promote), even with a matching source-file digest.
@Test func dqcD_mismatchedDatasetDigestThrows() throws {
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcBoundExport([dqcAnnotation()]))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    let wrongDatasetApproval = ManualAnnotationCSVApproval(
        sourceFileDigest: gated.sourceFileDigest,
        activeDatasetDigest: dqcDigest(dqcDatasetB),            // nonempty but a different dataset
        approver: "Dr. Reviewer",
        approvedAt: Date(timeIntervalSince1970: 1_000)
    )!
    do {
        _ = try gated.authoritativeAnnotations(approval: wrongDatasetApproval)
        Issue.record("mismatched dataset digest must throw")
    } catch ManualAnnotationCSVAuthorityError.datasetDigestMismatch {
        // expected
    } catch {
        Issue.record("expected datasetDigestMismatch, got \(error)")
    }
}

// Correction D: an approval cannot be constructed with an empty/whitespace approver — an approval object
// can never exist without an approver identity.
@Test func dqcD_approvalWithEmptyApproverCannotBeConstructed() throws {
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcBoundExport([dqcAnnotation()]))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(ManualAnnotationCSVApproval(
        sourceFileDigest: gated.sourceFileDigest,
        activeDatasetDigest: gated.activeDatasetDigest,
        approver: "   ",                                        // whitespace-only
        approvedAt: Date(timeIntervalSince1970: 1_000)
    ) == nil)
    // And an approval also cannot exist with an empty digest.
    #expect(ManualAnnotationCSVApproval(
        sourceFileDigest: "",
        activeDatasetDigest: gated.activeDatasetDigest,
        approver: "Dr. Reviewer",
        approvedAt: Date(timeIntervalSince1970: 1_000)
    ) == nil)
}

// Correction D: a non-eligible import ALWAYS throws notEligibleForAuthority, even when the approval's
// digests are internally consistent with the gated import. Matching digests can never rescue a
// non-eligible import (here: a dataset mismatch).
@Test func dqcD_nonEligibleImportAlwaysThrowsEvenWithConsistentApproval() throws {
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(
        contents: dqcBoundExport([dqcAnnotation()], dataset: dqcDatasetA)
    )
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetB)   // mismatch
    #expect(gated.identity == .datasetMismatch)
    #expect(gated.authority == .reviewOnly)
    // Build an approval whose digests match the gated import exactly — still cannot promote.
    let consistentApproval = ManualAnnotationCSVApproval(
        sourceFileDigest: gated.sourceFileDigest,
        activeDatasetDigest: gated.activeDatasetDigest,
        approver: "Dr. Reviewer",
        approvedAt: Date(timeIntervalSince1970: 1_000)
    )!
    do {
        _ = try gated.authoritativeAnnotations(approval: consistentApproval)
        Issue.record("non-eligible import must throw regardless of approval consistency")
    } catch ManualAnnotationCSVAuthorityError.notEligibleForAuthority(let identity, let blockers) {
        #expect(identity == .datasetMismatch)
        #expect(blockers.contains(.datasetMismatch))
    } catch {
        Issue.record("expected notEligibleForAuthority, got \(error)")
    }
}

// Correction E: the legacy parse-only API fails closed on an identity-bound (21-column) file, so an
// identity envelope can never be silently ignored.
@Test func dqcE_legacyImportRejectsIdentityBoundFile() throws {
    let csv = dqcBoundExport([dqcAnnotation()])   // 21-column identity-bound
    do {
        _ = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
        Issue.record("legacy importAnnotations must reject identity-bound files")
    } catch ManualAnnotationCSVImporter.ImportError.identityColumnsPresent {
        // expected: caller is directed to importIdentityBound
    } catch {
        Issue.record("expected identityColumnsPresent, got \(error)")
    }
}

// Correction E: importIdentityBound does not depend on the public legacy bypass — the SAME identity-
// bound file that the legacy API rejects parses cleanly through the identity-bound path.
@Test func dqcE_identityBoundImportDoesNotDependOnLegacyBypass() throws {
    let csv = dqcBoundExport([dqcAnnotation()])
    #expect(throws: ManualAnnotationCSVImporter.ImportError.self) {
        _ = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    }
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)   // must NOT throw
    #expect(imported.annotations.count == 1)
    guard case .bound = imported.fileIdentity else { Issue.record("expected .bound"); return }
}

// Correction E: a legacy 17-column file (no identity columns) still parses through the legacy API.
@Test func dqcE_legacyImportStillAcceptsLegacyFile() throws {
    let csv = ManualAnnotationCSVExporter.csv(annotations: [dqcAnnotation()])   // 17-column legacy
    let result = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
    #expect(result.annotations.count == 1)
    #expect(result.skippedRowCount == 0)
}

// Correction F: authoritative annotations are geometry-resolved against the ACTIVE dataset — cached
// indices in the file are recomputed from authoritative time, not trusted. A stale cached index never
// survives promotion.
@Test func dqcF_authoritativeAnnotationsRecomputeCachedIndices() throws {
    // The file carries deliberately-wrong cached ISI indices (99/99) but correct time (0.1..0.3 -> 2..3).
    let csv = dqcBoundExport([dqcAnnotation(startISI: 99, endISI: 99, start: 0.1, end: 0.3)])
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.annotations[0].startISIIndex == 99)   // raw parse preserves the file's cached index
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    let authoritative = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    #expect(authoritative[0].startISIIndex == 2)           // recomputed from authoritative time
    #expect(authoritative[0].endISIIndex == 3)
}

// Correction F: a matching-identity file whose annotation belongs to a train missing from the active
// dataset fails the geometry gate (typed blocker) and stays review-only.
@Test func dqcF_missingTrainGeometryIsReviewOnly() throws {
    let csv = dqcBoundExport([dqcAnnotation(train: "ghost", start: 0.1, end: 0.3)])
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)           // digest still matches the active dataset
    #expect(gated.authority == .reviewOnly)               // ...but the train is absent
    #expect(gated.blockers.contains(.geometryIncompatible))
}

// Correction G: an empty identity-bound file (header only, zero data rows) parses as an empty review-
// only import — it never crashes, never becomes eligible, and is not misclassified as legacy authority.
@Test func dqcG_emptyIdentityBoundFileIsReviewOnlyAndNeverEligible() throws {
    let csv = dqcMinimalHeader.joined(separator: ",") + "\r\n"   // header only, no data rows
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.annotations.isEmpty)
    #expect(imported.fileIdentity != .legacyUnbound)      // declared identity columns are not legacy
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity != .matchingDataset)           // cannot bind without a dataset_digest value
    #expect(gated.authority == .reviewOnly)
    #expect(gated.blockers.contains(.emptyImport))
    #expect(throws: ManualAnnotationCSVAuthorityError.self) {
        _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    }
}

// LF (\n) record terminators bind identity just like CRLF and lone-CR.
@Test func dqcHandlesLFRecords() throws {
    let row = dqcMinimalRow(
        schema: ManualAnnotationCSVExporter.identitySchemaVersion,
        digest: dqcDigest(dqcDatasetA), run: "run_x", review: "pending_confirmation"
    )
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV([row], separator: "\n"))
    #expect(imported.annotations.count == 1)
    guard case .bound = imported.fileIdentity else { Issue.record("LF broke identity"); return }
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
}

// Correction E boundary: identity-column detection triggers on ANY ONE of the four identity columns,
// not only the full four. A file carrying only `run_id` still fails the legacy API closed, and the
// identity-bound path classifies it malformed (no dataset_digest anchor) — it can never be authoritative.
@Test func dqcE_singleIdentityColumnStillFailsClosedBothPaths() throws {
    let csv = """
    train_id,label,start_sec,end_sec,run_id\r
    g,tonic,0.1,0.3,run_x\r
    """
    do {
        _ = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
        Issue.record("legacy import must reject a file carrying any identity column")
    } catch ManualAnnotationCSVImporter.ImportError.identityColumnsPresent {
        // expected
    } catch {
        Issue.record("expected identityColumnsPresent, got \(error)")
    }
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.fileIdentity == .malformedIdentity)   // declared identity, but no dataset_digest anchor
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.authority == .reviewOnly)
}

// Correction E boundary: a QUOTED identity header cell is still an identity column after CSV unquoting,
// so the legacy API cannot be slipped an identity-bound file by quoting the header name.
@Test func dqcE_quotedIdentityHeaderStillRejectedByLegacyImport() throws {
    let csv = "train_id,label,start_sec,end_sec,\"dataset_digest\"\r\ng,tonic,0.1,0.3,"
        + dqcDigest(dqcDatasetA) + "\r\n"
    do {
        _ = try ManualAnnotationCSVImporter.importAnnotations(contents: csv)
        Issue.record("legacy import must reject a quoted identity header cell")
    } catch ManualAnnotationCSVImporter.ImportError.identityColumnsPresent {
        // expected
    } catch {
        Issue.record("expected identityColumnsPresent, got \(error)")
    }
}

// Determinism contract: the approval-bound source-file digest is a pure function of the exact bytes, and
// the value carried through the import and the gate matches `sourceFileDigest(_:)` computed on the same
// bytes — so an approval built from those bytes always matches the gated import.
@Test func dqcSourceFileDigestIsDeterministicAndCarriedThroughGate() throws {
    let csv = dqcBoundExport([dqcAnnotation()])
    let expected = ManualAnnotationCSVImporter.sourceFileDigest(csv)
    #expect(expected.count == 64)                                   // SHA-256 hex, never empty
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    #expect(imported.sourceFileDigest == expected)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.sourceFileDigest == expected)
    #expect(ManualAnnotationCSVImporter.sourceFileDigest(csv) == expected)   // pure: recompute is stable
}

// MARK: - Phase 2.2C-A.1.1: consistent identity envelope + approval-timestamp hardening

// A second in-range annotation for dataset A (ISI 1..2), distinct from dqcRow's default (ISI 2..3), so a
// multi-row identity-bound file carries two genuine annotations that both resolve.
private func dqcSecondRow(digest: String, run: String = "", review: String = "pending_confirmation") -> [String] {
    dqcRow(start: "0.0", end: "0.2", startISI: "1", endISI: "2", digest: digest, run: run, review: review)
}

// Correction (envelope consistency): a review_state that is pending on one physical row and blank on
// another is an INCONSISTENT envelope — one populated row must not speak for a blank row. Fails closed.
@Test func dqcMixedBlankAndPendingReviewStateFailsClosed() throws {
    let digest = dqcDigest(dqcDatasetA)
    let rows = [
        dqcRow(digest: digest, review: "pending_confirmation"),
        dqcSecondRow(digest: digest, review: ""),   // blank review_state on the second row
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    #expect(imported.fileIdentity == .malformedIdentity)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .malformedIdentity)
    #expect(gated.authority == .reviewOnly)
    #expect(throws: ManualAnnotationCSVAuthorityError.self) {
        _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    }
}

// Correction (envelope consistency): a uniformly-blank review_state across rows is a valid MISSING value
// that defaults to review_only (NOT malformed) — the identity may still match, but it is never eligible.
@Test func dqcAllBlankReviewStateDefaultsToReviewOnlyAcrossRows() throws {
    let digest = dqcDigest(dqcDatasetA)
    let rows = [
        dqcRow(digest: digest, review: ""),
        dqcSecondRow(digest: digest, review: ""),
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    guard case .bound = imported.fileIdentity else { Issue.record("uniformly-blank optional must stay bound"); return }
    #expect(imported.envelope.reviewState == nil)          // uniformly blank -> missing, not malformed
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)
    #expect(gated.authority == .reviewOnly)
    #expect(gated.blockers.contains(.reviewOnlyState))     // missing review_state defaults to review_only
}

// Positive control: a review_state that is uniformly pending_confirmation across every row stays eligible.
@Test func dqcUniformPendingReviewStateAcrossRowsRemainsEligible() throws {
    let digest = dqcDigest(dqcDatasetA)
    let rows = [
        dqcRow(digest: digest, review: "pending_confirmation"),
        dqcSecondRow(digest: digest, review: "pending_confirmation"),
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    #expect(imported.annotations.count == 2)
    guard case .bound = imported.fileIdentity else { Issue.record("expected .bound"); return }
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .matchingDataset)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    #expect(try gated.authoritativeAnnotations(approval: dqcApproval(gated)).count == 2)
}

// Correction (envelope consistency): the same mixed-blank rule applies to the optional run_id column —
// a run_id present on one row and blank on another is a malformed (inconsistent) envelope, fail-closed.
@Test func dqcMixedBlankAndNonblankRunIDFailsClosed() throws {
    let digest = dqcDigest(dqcDatasetA)
    let rows = [
        dqcRow(digest: digest, run: "run_x", review: "pending_confirmation"),
        dqcSecondRow(digest: digest, run: "", review: "pending_confirmation"),   // blank run_id
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    #expect(imported.fileIdentity == .malformedIdentity)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.identity == .malformedIdentity)
    #expect(gated.authority == .reviewOnly)
    #expect(throws: ManualAnnotationCSVAuthorityError.self) {
        _ = try gated.authoritativeAnnotations(approval: dqcApproval(gated))
    }
}

// Correction (envelope consistency): a uniformly-blank run_id is a valid optional missing value and does
// NOT by itself block an otherwise-eligible pending_confirmation import. run_id is not an authority gate.
@Test func dqcAllBlankRunIDDoesNotBlockEligibility() throws {
    let digest = dqcDigest(dqcDatasetA)
    let rows = [
        dqcRow(digest: digest, run: "", review: "pending_confirmation"),
        dqcSecondRow(digest: digest, run: "", review: "pending_confirmation"),
    ]
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcMinimalCSV(rows))
    #expect(imported.envelope.runID == nil)                // uniformly-blank optional -> valid missing
    guard case .bound = imported.fileIdentity else { Issue.record("expected .bound"); return }
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)   // run_id absence does not gate
}

// Approval hardening: a non-finite approval timestamp (NaN/±infinity) cannot construct an approval
// record; a normal finite timestamp still constructs and promotes an eligible import.
@Test func dqcApprovalRejectsNonFiniteTimestamp() throws {
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(contents: dqcBoundExport([dqcAnnotation()]))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    func approval(_ at: Date) -> ManualAnnotationCSVApproval? {
        ManualAnnotationCSVApproval(
            sourceFileDigest: gated.sourceFileDigest,
            activeDatasetDigest: gated.activeDatasetDigest,
            approver: "Dr. Reviewer",
            approvedAt: at
        )
    }
    #expect(approval(Date(timeIntervalSinceReferenceDate: .nan)) == nil)
    #expect(approval(Date(timeIntervalSinceReferenceDate: .infinity)) == nil)
    #expect(approval(Date(timeIntervalSinceReferenceDate: -.infinity)) == nil)
    let finite = try #require(approval(Date(timeIntervalSince1970: 1_000)))   // finite still accepted
    #expect(try gated.authoritativeAnnotations(approval: finite).count == 1)
}

// MARK: - Phase 2.2C-B1: raw-byte-bound manual annotation CSV ingestion

// Required #1: sourceFileDigest(Data) is the lowercase SHA-256 hex over the exact supplied bytes.
@Test func b1RawByteDigestIsLowercaseSHA256OfExactBytes() {
    // FIPS 180-2 SHA-256 known-answer vectors (no CryptoKit dependency needed in tests).
    #expect(ManualAnnotationCSVImporter.sourceFileDigest(Data()) ==
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    #expect(ManualAnnotationCSVImporter.sourceFileDigest(Data("abc".utf8)) ==
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    let digest = ManualAnnotationCSVImporter.sourceFileDigest(Data("abc".utf8))
    #expect(digest == digest.lowercased() && digest.count == 64) // lowercase, fixed 64-hex width
    // The String overload delegates to the raw-byte overload over Data(contents.utf8) (behavior unchanged).
    #expect(ManualAnnotationCSVImporter.sourceFileDigest("abc") ==
        ManualAnnotationCSVImporter.sourceFileDigest(Data("abc".utf8)))
}

// Required #2: a BOM and non-BOM CSV parse to equivalent annotations + identity envelope, yet their
// raw-byte source digests differ (the BOM bytes are part of the raw digest, never normalized away).
@Test func b1BOMAndNonBOMParseEquivalentlyButRawDigestsDiffer() throws {
    let csv = dqcBoundExport([dqcAnnotation()], runID: "run_x")
    let plainData = Data(csv.utf8)
    let bomData = Data([0xEF, 0xBB, 0xBF]) + plainData

    let plain = try ManualAnnotationCSVImporter.importIdentityBound(data: plainData)
    let bom = try ManualAnnotationCSVImporter.importIdentityBound(data: bomData)

    // Equivalent parsed content and identity envelope (the decoded leading BOM is stripped for parsing).
    #expect(plain.annotations == bom.annotations)
    #expect(plain.fileIdentity == bom.fileIdentity)
    #expect(plain.envelope == bom.envelope)
    #expect(plain.unsupportedLabels == bom.unsupportedLabels)
    #expect(plain.skippedRowCount == bom.skippedRowCount)
    // But the raw-byte digests differ, and each binds its own exact bytes.
    #expect(plain.sourceFileDigest != bom.sourceFileDigest)
    #expect(plain.sourceFileDigest == ManualAnnotationCSVImporter.sourceFileDigest(plainData))
    #expect(bom.sourceFileDigest == ManualAnnotationCSVImporter.sourceFileDigest(bomData))
}

// Required #3: LF and CRLF versions parse equivalently but have different raw-byte digests.
@Test func b1LFAndCRLFParseEquivalentlyButRawDigestsDiffer() throws {
    let digest = dqcDigest(dqcDatasetA)
    let row = dqcRow(digest: digest, run: "run_x", review: "pending_confirmation")
    let lf = dqcMinimalCSV([row], separator: "\n")
    let crlf = dqcMinimalCSV([row], separator: "\r\n")
    #expect(lf != crlf) // genuinely byte-different sources

    let lfImport = try ManualAnnotationCSVImporter.importIdentityBound(data: Data(lf.utf8))
    let crlfImport = try ManualAnnotationCSVImporter.importIdentityBound(data: Data(crlf.utf8))

    #expect(lfImport.annotations == crlfImport.annotations)
    #expect(lfImport.fileIdentity == crlfImport.fileIdentity)
    #expect(lfImport.envelope == crlfImport.envelope)
    #expect(lfImport.sourceFileDigest != crlfImport.sourceFileDigest)
    #expect(lfImport.sourceFileDigest == ManualAnnotationCSVImporter.sourceFileDigest(Data(lf.utf8)))
    #expect(crlfImport.sourceFileDigest == ManualAnnotationCSVImporter.sourceFileDigest(Data(crlf.utf8)))
}

// Required #4: invalid UTF-8 fails closed with the typed error and produces NO import object (no lossy
// replacement decoding).
@Test func b1InvalidUTF8FailsClosedWithTypedError() {
    let invalid = Data([0x74, 0x72, 0x61, 0x69, 0x6E, 0xFF, 0xFE]) // 0xFF/0xFE are never valid UTF-8
    do {
        _ = try ManualAnnotationCSVImporter.importIdentityBound(data: invalid)
        Issue.record("invalid UTF-8 must fail closed, not produce an import object")
    } catch ManualAnnotationCSVImporter.ImportError.invalidUTF8 {
        // expected: strict decode failed closed
    } catch {
        Issue.record("expected ImportError.invalidUTF8, got \(error)")
    }
}

// Required #5: the exact raw-byte digest survives import and gate unchanged.
@Test func b1RawByteDigestSurvivesImportAndGate() throws {
    let data = Data(dqcBoundExport([dqcAnnotation()]).utf8)
    let expected = ManualAnnotationCSVImporter.sourceFileDigest(data)
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(data: data)
    #expect(imported.sourceFileDigest == expected)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.sourceFileDigest == expected)
}

// Required #6: an approval carrying the exact raw-byte digest promotes an otherwise-eligible import.
@Test func b1ApprovalWithExactRawDigestPromotesEligibleImport() throws {
    let data = Data(dqcBoundExport([dqcAnnotation()]).utf8)
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(data: data)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    let approval = ManualAnnotationCSVApproval(
        sourceFileDigest: ManualAnnotationCSVImporter.sourceFileDigest(data),
        activeDatasetDigest: gated.activeDatasetDigest,
        approver: "Dr. Reviewer",
        approvedAt: Date(timeIntervalSince1970: 1_000)
    )!
    let authoritative = try gated.authoritativeAnnotations(approval: approval)
    #expect(authoritative.count == 1)
}

// Required #7: an approval using the String-derived digest for a byte-different (BOM) file is rejected
// with sourceFileDigestMismatch — the raw-bytes-bound import cannot be promoted by a decoded-string digest.
@Test func b1StringDerivedDigestForByteDifferentFileIsRejected() throws {
    let csv = dqcBoundExport([dqcAnnotation()])
    let bomData = Data([0xEF, 0xBB, 0xBF]) + Data(csv.utf8)
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(data: bomData)
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: dqcDatasetA)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    // The String-derived digest of the BOM-less content does NOT bind the raw (BOM-including) bytes.
    let stringDerived = ManualAnnotationCSVImporter.sourceFileDigest(csv)
    #expect(stringDerived != gated.sourceFileDigest)
    let mismatched = ManualAnnotationCSVApproval(
        sourceFileDigest: stringDerived,
        activeDatasetDigest: gated.activeDatasetDigest,
        approver: "Dr. Reviewer",
        approvedAt: Date(timeIntervalSince1970: 1_000)
    )!
    do {
        _ = try gated.authoritativeAnnotations(approval: mismatched)
        Issue.record("a String-derived digest for a byte-different file must not promote a raw-bytes-bound import")
    } catch ManualAnnotationCSVAuthorityError.sourceFileDigestMismatch {
        // expected: the approval does not bind the exact ingested bytes
    } catch {
        Issue.record("expected sourceFileDigestMismatch, got \(error)")
    }
}

// Required #8: existing String-import behavior and legacy review-only behavior remain intact. For plain
// (BOM-less) bytes the String and raw-byte entry points are identical, including the digest.
@Test func b1StringAndRawPathsAgreeForPlainBytesAndLegacyStaysReviewOnly() throws {
    let csv = dqcBoundExport([dqcAnnotation()], runID: "run_x")
    let viaString = try ManualAnnotationCSVImporter.importIdentityBound(contents: csv)
    let viaData = try ManualAnnotationCSVImporter.importIdentityBound(data: Data(csv.utf8))
    #expect(viaString == viaData) // identical import (annotations, identity, envelope, digest)
    #expect(viaString.sourceFileDigest == ManualAnnotationCSVImporter.sourceFileDigest(Data(csv.utf8)))

    // Legacy (17-column, no identity columns) file stays review-only through BOTH entry points.
    let legacy = ManualAnnotationCSVExporter.csv(annotations: [dqcAnnotation()])
    let legacyString = try ManualAnnotationCSVImporter.importIdentityBound(contents: legacy)
    let legacyData = try ManualAnnotationCSVImporter.importIdentityBound(data: Data(legacy.utf8))
    #expect(legacyString.fileIdentity == .legacyUnbound)
    #expect(legacyData.fileIdentity == .legacyUnbound)
    #expect(ManualAnnotationCSVImporter.gate(legacyString, activeDataset: dqcDatasetA).authority == .reviewOnly)
    #expect(ManualAnnotationCSVImporter.gate(legacyData, activeDataset: dqcDatasetA).authority == .reviewOnly)
}
