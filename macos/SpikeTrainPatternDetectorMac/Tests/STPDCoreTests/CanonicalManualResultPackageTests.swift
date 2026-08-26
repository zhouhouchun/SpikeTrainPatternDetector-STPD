import Foundation
@testable import STPDCore
import Testing

@Suite("Canonical manual result package", .serialized)
struct CanonicalManualResultPackageTests {
    @Test("Partial canonical manual review exports identity-bound CSV without detector authority")
    func partialDraftExportsIdentityBoundCSV() async throws {
        try await withPersistedBase { persisted in
            let shadow = try CanonicalScientificImportProjector.project(
                persisted.baseConfirmation.validatedImport
            )
            let train = try #require(shadow.dataset.spikeTrains.first)
            let draft = CanonicalManualISILabelDraft(
                canonicalFingerprint: shadow.fingerprint
            ).applying(
                label: .tonic,
                trainID: train.semanticID,
                isiIndices: [1]
            )

            let csv = try CanonicalManualISIDraftCSVExporter.csv(
                persistedImport: persisted,
                draft: draft
            )
            let rows = try CanonicalManualISILabelProjector.rows(
                dataset: shadow.dataset,
                fingerprint: shadow.fingerprint,
                draft: draft
            )
            let lines = csv.components(separatedBy: "\r\n").filter { !$0.isEmpty }
            #expect(lines.count == rows.count + 1)
            #expect(lines[0].contains("state_pattern,event_pattern,other_pattern"))
            #expect(lines[0].contains("review_status,authority_status"))
            #expect(csv.contains(shadow.fingerprint.datasetDigest))
            #expect(csv.contains(persisted.confirmationRecordDigest))
            #expect(csv.contains(",tonic,,,manually_labeled,"))
            #expect(csv.contains(",,,,unreviewed,") || rows.allSatisfy(\.isReviewed))
            #expect(lines.dropFirst().allSatisfy {
                $0.contains(CanonicalManualISIDraftCSVExporter.authorityStatus)
            })
        }
    }

    @Test("Builder emits exact microsecond dual-track rows from persisted canonical identity")
    func builderEmitsSealedDualTrackRows() async throws {
        try await withPersistedBase { persisted in
            let shadow = try CanonicalScientificImportProjector.project(
                persisted.baseConfirmation.validatedImport
            )
            let train = try #require(shadow.dataset.spikeTrains.first)
            let indices = Set(1..<train.rawTimestamps.count)
            var draft = CanonicalManualISILabelDraft(
                canonicalFingerprint: shadow.fingerprint
            )
            draft = draft.applying(
                label: .highFrequencySpiking,
                trainID: train.semanticID,
                isiIndices: indices
            )
            draft = draft.applying(
                label: .highFrequencyBurst,
                trainID: train.semanticID,
                isiIndices: indices
            )
            let confirmation = try ConfirmedCanonicalManualISILabels.confirmComplete(
                draft: draft,
                dataset: shadow.dataset,
                fingerprint: shadow.fingerprint,
                reviewer: "Reviewer",
                confirmedAt: Date(timeIntervalSince1970: 123.25)
            )

            let package = try CanonicalManualResultPackageBuilder.build(
                persistedImport: persisted,
                confirmedLabels: confirmation
            )
            try CanonicalManualResultPackageBuilder.validate(package)
            #expect(package.manifest.sourceMode == "complete_manual_review")
            #expect(package.manifest.canonicalDatasetDigest
                == shadow.fingerprint.datasetDigest)
            #expect(package.manifest.manualDecisionDigest
                == confirmation.decisionDigest)
            #expect(package.manifest.files.first?.rowCount == indices.count)
            let csv = try #require(String(data: package.isiLabelsCSV, encoding: .utf8))
            #expect(csv.contains("left_timestamp_us,right_timestamp_us,isi_us"))
            #expect(csv.contains("high_frequency_spiking,high_frequency_burst"))
            #expect(csv.contains("sealed_complete_manual_review"))
            #expect(!csv.contains("timestamp_sec"))
        }
    }

    @Test("Package validation detects altered table bytes")
    func validationDetectsAlteredTable() async throws {
        try await withPersistedBase { persisted in
            let shadow = try CanonicalScientificImportProjector.project(
                persisted.baseConfirmation.validatedImport
            )
            let train = try #require(shadow.dataset.spikeTrains.first)
            let draft = CanonicalManualISILabelDraft(
                canonicalFingerprint: shadow.fingerprint
            ).applying(
                label: .other,
                trainID: train.semanticID,
                isiIndices: Set(1..<train.rawTimestamps.count)
            )
            let confirmation = try ConfirmedCanonicalManualISILabels.confirmComplete(
                draft: draft,
                dataset: shadow.dataset,
                fingerprint: shadow.fingerprint,
                reviewer: "Reviewer",
                confirmedAt: Date(timeIntervalSince1970: 100)
            )
            let package = try CanonicalManualResultPackageBuilder.build(
                persistedImport: persisted,
                confirmedLabels: confirmation
            )
            var altered = package.isiLabelsCSV
            altered.append(0x0A)
            let tampered = CanonicalManualResultPackage(
                manifest: package.manifest,
                isiLabelsCSV: altered
            )
            #expect(throws: CanonicalManualResultPackageError.confirmationDigestMismatch) {
                try CanonicalManualResultPackageBuilder.validate(tampered)
            }
        }
    }

    @Test("Shared result writer publishes manual package with no-replace semantics")
    func writerPublishesCompleteManualPackage() async throws {
        try await withPersistedBase { persisted in
            let package = try completeOtherPackage(persisted)
            let parent = FileManager.default.temporaryDirectory
                .appendingPathComponent("CanonicalManualWriterTests", isDirectory: true)
                .appendingPathComponent(UUID().uuidString, isDirectory: true)
            try FileManager.default.createDirectory(
                at: parent,
                withIntermediateDirectories: true,
                attributes: [.posixPermissions: 0o700]
            )
            defer { try? FileManager.default.removeItem(at: parent) }
            let destination = parent.appendingPathComponent(
                "manual.stpdresult",
                isDirectory: true
            )
            try STPDResultPackageWriter.writeCanonicalManualResult(
                package,
                to: destination
            )
            let csv = try Data(contentsOf: destination.appendingPathComponent(
                CanonicalManualResultPackageBuilder.isiLabelsFileName
            ))
            #expect(csv == package.isiLabelsCSV)
            let manifest = try Data(contentsOf: destination.appendingPathComponent(
                CanonicalManualResultPackageBuilder.manifestFileName
            ))
            #expect(manifest == (try package.manifest.encodedData()))
            let readback = try STPDResultPackageReader.readCanonicalManualResult(
                packageAt: destination
            )
            #expect(readback.manifest == package.manifest)
            #expect(readback.rows.count == package.manifest.files[0].rowCount)
            #expect(readback.rows.allSatisfy { $0.otherPattern == "other" })
            #expect(throws: STPDResultPackageError.self) {
                try STPDResultPackageWriter.writeCanonicalManualResult(
                    package,
                    to: destination
                )
            }
        }
    }

    @Test("Strict readback rejects a digest-resynchronized Other and State contradiction")
    func readerRejectsResynchronizedTrackContradiction() async throws {
        try await withPersistedBase { persisted in
            let package = try completeOtherPackage(persisted)
            let original = try #require(String(
                data: package.isiLabelsCSV,
                encoding: .utf8
            ))
            let rewritten = original.replacingOccurrences(
                of: ",,other,Reviewer",
                with: "tonic,,other,Reviewer"
            )
            #expect(rewritten != original)
            let tableBytes = Data(rewritten.utf8)
            let oldFile = try #require(package.manifest.files.first)
            let newFile = CanonicalManualResultPackageFile(
                fileName: oldFile.fileName,
                sha256: STPDStableIdentifier.digest(tableBytes),
                byteCount: tableBytes.count,
                rowCount: oldFile.rowCount
            )
            let old = package.manifest
            let newManifest = CanonicalManualResultPackageManifest(
                schemaContractID: old.schemaContractID,
                schemaContractDigest: old.schemaContractDigest,
                canonicalSchemaContractID: old.canonicalSchemaContractID,
                canonicalSchemaContractDigest: old.canonicalSchemaContractDigest,
                canonicalDatasetDigest: old.canonicalDatasetDigest,
                confirmedImportRecordDigest: old.confirmedImportRecordDigest,
                manualDecisionDigest: old.manualDecisionDigest,
                reviewer: old.reviewer,
                confirmedAtUnixSeconds: old.confirmedAtUnixSeconds,
                sourceMode: old.sourceMode,
                files: [newFile]
            )
            let resynchronized = CanonicalManualResultPackage(
                manifest: newManifest,
                isiLabelsCSV: tableBytes
            )

            let parent = FileManager.default.temporaryDirectory
                .appendingPathComponent("CanonicalManualReaderTests", isDirectory: true)
                .appendingPathComponent(UUID().uuidString, isDirectory: true)
            try FileManager.default.createDirectory(
                at: parent,
                withIntermediateDirectories: true,
                attributes: [.posixPermissions: 0o700]
            )
            defer { try? FileManager.default.removeItem(at: parent) }
            let destination = parent.appendingPathComponent(
                "contradictory.stpdresult",
                isDirectory: true
            )
            try STPDResultPackageWriter.writeCanonicalManualResult(
                resynchronized,
                to: destination
            )
            #expect(throws: STPDResultPackageReaderError.self) {
                try STPDResultPackageReader.readCanonicalManualResult(
                    packageAt: destination
                )
            }
        }
    }

    private func completeOtherPackage(
        _ persisted: PersistedConfirmedScientificImportManifest
    ) throws -> CanonicalManualResultPackage {
        let shadow = try CanonicalScientificImportProjector.project(
            persisted.baseConfirmation.validatedImport
        )
        let train = try #require(shadow.dataset.spikeTrains.first)
        let draft = CanonicalManualISILabelDraft(
            canonicalFingerprint: shadow.fingerprint
        ).applying(
            label: .other,
            trainID: train.semanticID,
            isiIndices: Set(1..<train.rawTimestamps.count)
        )
        let confirmation = try ConfirmedCanonicalManualISILabels.confirmComplete(
            draft: draft,
            dataset: shadow.dataset,
            fingerprint: shadow.fingerprint,
            reviewer: "Reviewer",
            confirmedAt: Date(timeIntervalSince1970: 100)
        )
        return try CanonicalManualResultPackageBuilder.build(
            persistedImport: persisted,
            confirmedLabels: confirmation
        )
    }

    private func withPersistedBase(
        _ body: (PersistedConfirmedScientificImportManifest) async throws -> Void
    ) async throws {
        let parent = FileManager.default.temporaryDirectory
            .appendingPathComponent("CanonicalManualResultPackageTests", isDirectory: true)
            .appendingPathComponent(UUID().uuidString, isDirectory: true)
        try FileManager.default.createDirectory(at: parent, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: parent) }
        let store = ConfirmedScientificImportManifestStore(
            rootDirectory: parent.appendingPathComponent("store", isDirectory: true)
        )
        let saved = try await store.save(
            baseConfirmation: simpleConfirmedManifest()
        )
        try await body(saved.manifest)
    }
}
