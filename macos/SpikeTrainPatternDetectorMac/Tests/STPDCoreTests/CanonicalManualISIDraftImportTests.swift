@testable import STPDCore
import Testing

@Suite("Canonical manual ISI draft import")
struct CanonicalManualISIDraftImportTests {
    @Test("Identity-bound draft reconstructs exact geometry and independent tracks")
    func draftReconstructsExactRows() throws {
        let context = try makeContext()
        let imported = try CanonicalManualISIDraftImport.decode(
            tables: [table(context: context)]
        )

        let draft = try imported.validatedDraft(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            confirmationRecordDigest: context.confirmationRecordDigest
        )
        let rows = try CanonicalManualISILabelProjector.rows(
            dataset: context.dataset,
            fingerprint: context.fingerprint,
            draft: draft
        )
        #expect(rows.count == 2)
        #expect(rows[0].stateLabel == .tonic)
        #expect(rows[0].eventLabel == .highFrequencyBurst)
        #expect(rows[1].otherLabel == .other)
    }

    @Test("A changed exact timestamp is rejected before a draft can be installed")
    func changedGeometryFailsClosed() throws {
        let context = try makeContext()
        var rows = table(context: context).rows
        rows[0][8] = "1" // left_timestamp_us
        let imported = try CanonicalManualISIDraftImport.decode(
            tables: [ManualISIExportTable(
                headers: CanonicalManualISIDraftCSVExporter.headers,
                rows: rows
            )]
        )

        #expect(throws: CanonicalManualISIDraftImportError.geometryMismatch(
            trainID: "unit_a",
            isiIndex: 1
        )) {
            try imported.validatedDraft(
                dataset: context.dataset,
                fingerprint: context.fingerprint,
                confirmationRecordDigest: context.confirmationRecordDigest
            )
        }
    }

    private func table(context: Context) -> ManualISIExportTable {
        let metadata = [
            CanonicalManualISIDraftCSVExporter.schemaContractID,
            CanonicalManualISIDraftCSVExporter.schemaContractDigest,
            context.fingerprint.schemaContractID,
            context.fingerprint.schemaContractDigest,
            context.fingerprint.datasetDigest,
            context.confirmationRecordDigest,
        ]
        return ManualISIExportTable(
            headers: CanonicalManualISIDraftCSVExporter.headers,
            rows: [
                metadata + [
                    "unit_a", "1", "0", "10000", "10000",
                    "tonic", "high_frequency_burst", "", "manually_labeled",
                    CanonicalManualISIDraftCSVExporter.authorityStatus,
                ],
                metadata + [
                    "unit_a", "2", "10000", "25000", "15000",
                    "", "", "other", "manually_labeled",
                    CanonicalManualISIDraftCSVExporter.authorityStatus,
                ],
            ]
        )
    }

    private func makeContext() throws -> Context {
        let trainID = ScientificSpikeTrainID(try ScientificSemanticID(validating: "unit_a"))
        let groupID = ScientificEventScopeGroupID(try ScientificSemanticID(validating: "group_a"))
        let dataset = CanonicalScientificDataset(
            recordingSegment: ConfirmedRecordingSegment(
                semanticID: ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: "recording_a")
                ),
                regime: .continuousUntrialed,
                importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
                observationBounds: .unknownOrUnavailable
            ),
            activityMode: .putativeSingleUnit,
            spikeTrains: [CanonicalSpikeTrain(
                semanticID: trainID,
                rawTimestamps: [
                    MicrosecondTick(microseconds: 0),
                    MicrosecondTick(microseconds: 10_000),
                    MicrosecondTick(microseconds: 25_000),
                ]
            )],
            eventScopeGroups: [CanonicalEventScopeGroup(
                semanticID: groupID,
                timeBasis: .recordingElapsed,
                spikeTrainReferences: [trainID],
                eventDefinitions: []
            )],
            scientificAttributeDefinitions: []
        )
        return Context(
            dataset: dataset,
            fingerprint: try CanonicalScientificDatasetFingerprinter.fingerprint(dataset),
            confirmationRecordDigest: String(repeating: "a", count: 64)
        )
    }

    private struct Context {
        let dataset: CanonicalScientificDataset
        let fingerprint: CanonicalScientificDatasetFingerprint
        let confirmationRecordDigest: String
    }
}
