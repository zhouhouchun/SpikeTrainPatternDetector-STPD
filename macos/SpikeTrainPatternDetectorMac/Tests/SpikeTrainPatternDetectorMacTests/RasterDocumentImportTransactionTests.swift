import Foundation
@testable import STPDCore
@testable import SpikeTrainPatternDetectorMac
import Testing

@Suite("RasterDocument CSV import transaction", .serialized)
@MainActor
struct RasterDocumentImportTransactionTests {
    @Test("A newly installed dataset shows every spike train in the raster")
    func newDatasetShowsAllRasterTrains() throws {
        try withTemporaryDirectory { directory in
            let sourceURL = directory.appendingPathComponent("twelve-trains.csv")
            let headers = (1...12).map { "train_\($0)" }.joined(separator: ",")
            let firstRow = Array(repeating: "0.0", count: 12).joined(separator: ",")
            let secondRow = Array(repeating: "1.0", count: 12).joined(separator: ",")
            try "\(headers)\n\(firstRow)\n\(secondRow)\n".write(
                to: sourceURL,
                atomically: true,
                encoding: .utf8
            )

            let document = RasterDocument()
            document.loadCSV(
                from: sourceURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )

            let dataset = try #require(document.dataset)
            #expect(dataset.trains.count == 12)
            #expect(document.selectedTrainIDs == Set(dataset.trains.map(\.id)))
            #expect(document.requestedVisibleTrainCount == 12)
            #expect(!document.rasterVisibleWindowUserLocked)
        }
    }

    @Test("Raster train labels use their measured width instead of a fixed canvas fraction")
    func rasterLabelWidthUsesVisibleNames() {
        let shortWidth = RasterLabelWidthResolver.width(
            trainNames: ["unit_a"],
            fontSize: 11,
            containerWidth: 1_800
        )
        let screenshotWidth = RasterLabelWidthResolver.width(
            trainNames: ["LT1D10.003_SPK 01a", "RT1D9.781_SPK 01b"],
            fontSize: 11,
            containerWidth: 1_800
        )
        let longWidth = RasterLabelWidthResolver.width(
            trainNames: [String(repeating: "long_train_name_", count: 4)],
            fontSize: 11,
            containerWidth: 1_800
        )

        #expect(shortWidth == RasterLabelWidthResolver.minimumWidth)
        #expect(screenshotWidth < 220)
        #expect(longWidth > screenshotWidth)
        #expect(longWidth <= RasterLabelWidthResolver.maximumWidth)
    }

    @Test("Malformed CSV preserves an active document")
    func malformedCSVPreservesActiveDocument() async throws {
        try await withTemporaryDirectory { directory in
            let activeURL = directory.appendingPathComponent("active.csv")
            try Self.activeCSV.write(to: activeURL, atomically: true, encoding: .utf8)
            let malformedURL = directory.appendingPathComponent("malformed.csv")
            try "unit_a\n0.10\nBAD_TOKEN\n0.30\n".write(
                to: malformedURL,
                atomically: true,
                encoding: .utf8
            )

            let document = RasterDocument()
            document.loadCSV(
                from: activeURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )
            try await populateActiveState(in: document)

            document.statusMessage = "Active document ready."
            document.lastErrorMessage = "Previous non-destructive notice."
            let stateBeforeFailure = nonMessageStorageSnapshot(of: document)

            document.loadCSV(
                from: malformedURL,
                unit: .milliseconds,
                hasHeader: true,
                duplicatePolicy: .collapseExact
            )

            #expect(document.statusMessage == "CSV load failed. Existing dataset preserved.")
            #expect(document.lastErrorMessage?.contains("BAD_TOKEN") == true)
            #expect(nonMessageStorageSnapshot(of: document) == stateBeforeFailure)
        }
    }

    @Test("Missing CSV preserves an active document")
    func missingFilePreservesActiveDocument() throws {
        try withTemporaryDirectory { directory in
            let activeURL = directory.appendingPathComponent("active.csv")
            try Self.activeCSV.write(to: activeURL, atomically: true, encoding: .utf8)

            let document = RasterDocument()
            document.loadCSV(
                from: activeURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )
            document.focusedClassicAnchorCandidateID = "preserved-focus"
            document.classicAnchorFocusRequestID = 41
            document.detectorStatusMessage = "Preserved detector status."
            document.detectorLastRunDate = Date(timeIntervalSince1970: 1_234)
            document.selectedTrainIDs = ["fast_train"]
            document.isiSelectedTrainIDs = ["slower_train"]
            document.isiStateSpaceSelectedTrainIDs = ["fast_train"]
            document.statusMessage = "Active document ready."
            document.lastErrorMessage = nil
            let stateBeforeFailure = nonMessageStorageSnapshot(of: document)

            let missingURL = directory.appendingPathComponent("does-not-exist.csv")
            document.loadCSV(
                from: missingURL,
                unit: .seconds,
                hasHeader: false,
                duplicatePolicy: .warnKeep
            )

            #expect(document.statusMessage == "CSV load failed. Existing dataset preserved.")
            #expect(document.lastErrorMessage != nil)
            #expect(nonMessageStorageSnapshot(of: document) == stateBeforeFailure)
        }
    }

    @Test("Empty CSV failure leaves an empty document intact")
    func emptyCSVFailureLeavesEmptyDocumentIntact() throws {
        try withTemporaryDirectory { directory in
            let emptyURL = directory.appendingPathComponent("empty.csv")
            try "".write(to: emptyURL, atomically: true, encoding: .utf8)

            let document = RasterDocument()
            document.rawImportUnit = .milliseconds
            document.rawCSVHasHeader = false
            document.rasterVisibleWindowSeconds = 17
            document.statusMessage = "No dataset sentinel."
            document.lastErrorMessage = "Previous notice."
            let stateBeforeFailure = nonMessageStorageSnapshot(of: document)

            document.loadCSV(
                from: emptyURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .collapseExact
            )

            #expect(document.dataset == nil)
            #expect(document.statusMessage == "CSV load failed.")
            #expect(document.lastErrorMessage == "The CSV file is empty.")
            #expect(nonMessageStorageSnapshot(of: document) == stateBeforeFailure)
        }
    }

    @Test("Successful CSV import still replaces the active dataset")
    func successfulCSVStillReplacesActiveDataset() throws {
        try withTemporaryDirectory { directory in
            let firstURL = directory.appendingPathComponent("first.csv")
            try Self.activeCSV.write(to: firstURL, atomically: true, encoding: .utf8)
            let replacementURL = directory.appendingPathComponent("replacement.csv")
            try "replacement_train\n5.0\n5.5\n6.0\n".write(
                to: replacementURL,
                atomically: true,
                encoding: .utf8
            )

            let document = RasterDocument()
            document.loadCSV(
                from: firstURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )
            let originalDatasetID = try #require(document.dataset?.id)
            document.classicAnchorDetectionRun = ClassicAnchorDetectionPipeline.run(
                dataset: try #require(document.dataset)
            )
            document.focusedClassicAnchorCandidateID = "old-candidate"
            document.classicAnchorReviewStatuses = ["old-candidate": .accepted]
            document.classicAnchorReviewInputs = [
                "old-candidate": STPDCandidateReviewInput(
                    sourceCandidateID: "old-candidate",
                    status: .accepted,
                    reviewer: "Reviewer",
                    reviewedRunID: "old-run"
                )
            ]
            document.manualAnnotationsByTrain = [
                "fast_train": [
                    ManualAnnotation(
                        trainID: "fast_train",
                        label: .burst,
                        startSec: 0.1,
                        endSec: 0.2
                    )
                ]
            ]
            document.detectorLastRunDate = Date(timeIntervalSince1970: 2_345)
            document.detectorStatusMessage = "Old detector result."

            document.loadCSV(
                from: replacementURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .warnKeep
            )

            let replacement = try #require(document.dataset)
            #expect(replacement.id != originalDatasetID)
            #expect(replacement.name == "replacement")
            #expect(replacement.trains.map(\.id) == ["replacement_train"])
            #expect(replacement.trains[0].timestampsSec == [5.0, 5.5, 6.0])
            #expect(document.classicAnchorDetectionRun == nil)
            #expect(document.focusedClassicAnchorCandidateID == nil)
            #expect(document.classicAnchorReviewStatuses.isEmpty)
            #expect(document.classicAnchorReviewInputs.isEmpty)
            #expect(document.manualAnnotationsByTrain.isEmpty)
            #expect(document.detectorLastRunDate == nil)
            #expect(document.detectorStatusMessage == "Detector has not run.")
            #expect(document.selectedTrainIDs == ["replacement_train"])
            #expect(document.isiSelectedTrainIDs == ["replacement_train"])
            #expect(document.isiStateSpaceSelectedTrainIDs == ["replacement_train"])
            #expect(document.duplicateTimestampPolicy == .warnKeep)
            #expect(document.statusMessage.contains("已加载 1 条序列、3 个 spike"))
            #expect(document.lastErrorMessage == nil)
        }
    }

    @Test("Legacy CSV may be explored but cannot mint a sealed scientific result")
    func legacyCSVRemainsNonAuthoritative() async throws {
        try await withTemporaryDirectory { directory in
            let sourceURL = directory.appendingPathComponent("legacy.csv")
            try Self.activeCSV.write(to: sourceURL, atomically: true, encoding: .utf8)
            let document = RasterDocument()

            document.loadCSV(
                from: sourceURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )
            #expect(document.activeDatasetScientificStanding == .legacyUnreviewedImport)
            #expect(document.statusMessage.contains("尚未经过科学确认"))

            document.runAdaptiveClassicAnchorDetection()
            let clock = ContinuousClock()
            let deadline = clock.now.advanced(by: .seconds(10))
            while document.isDetectorRunning, clock.now < deadline {
                try await Task.sleep(for: .milliseconds(10))
            }

            _ = try #require(document.classicAnchorDetectionRun)
            #expect(document.detectorStatusMessage.contains("非权威"))
            #expect(!document.canExportCurrentResultPackage)
            #expect(!document.canExportClassicAnchorEventsCSV)
            #expect(!document.canExportHFSBurstArbitrationAuditCSV)
            do {
                _ = try document.currentResultPackageInput()
                Issue.record("Expected sealed result construction to require canonical confirmation")
            } catch let error as ActiveDatasetScientificStandingError {
                #expect(error == .canonicalConfirmationRequired)
            }
            do {
                _ = try document.currentResultPackageExportSeed()
                Issue.record("Expected the low-level export seed to require canonical confirmation")
            } catch let error as ActiveDatasetScientificStandingError {
                #expect(error == .canonicalConfirmationRequired)
            }

            document.exportClassicAnchorEventsCSVWithPanel()
            #expect(document.statusMessage == "Detector CSV export blocked.")
            #expect(document.lastErrorMessage?.contains("仅供探索") == true)

            document.exportHFSBurstArbitrationAuditCSVWithPanel()
            #expect(document.statusMessage == "Detector audit CSV export blocked.")
            #expect(document.lastErrorMessage?.contains("仅供探索") == true)
        }
    }

    @Test("Bundled sample is installed only with explicit demo standing")
    func bundledSampleHasDemoStanding() throws {
        let document = RasterDocument()

        document.loadBundledSample()

        _ = try #require(document.dataset)
        #expect(document.activeDatasetScientificStanding == .nonAuthoritativeDemo)
        #expect(document.statusMessage.contains("仅供演示——非权威"))
        #expect(!document.canExportCurrentResultPackage)
        #expect(!document.canImportAuthoritativeManualAnnotations)
    }

    @Test("Focusing a structural candidate prepares its timestamp raster in the background")
    func candidateFocusPreparesRaster() async throws {
        try await withTemporaryDirectory { directory in
            let sourceURL = directory.appendingPathComponent("focus.csv")
            try Self.activeCSV.write(to: sourceURL, atomically: true, encoding: .utf8)

            let document = RasterDocument()
            document.loadCSV(
                from: sourceURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )
            // Train durations are 0.20 s and 1.86 s: mean 1.03 s -> integer 1 s.
            #expect(document.rasterVisibleWindowSeconds == 1)
            #expect(document.rasterVisibleWindowUnit == .seconds)
            #expect(!document.rasterVisibleWindowUserLocked)
            document.runAdaptiveClassicAnchorDetection()

            let clock = ContinuousClock()
            let deadline = clock.now.advanced(by: .seconds(10))
            while document.isDetectorRunning, clock.now < deadline {
                try await Task.sleep(for: .milliseconds(10))
            }

            let run = try #require(document.classicAnchorDetectionRun)
            let annotation = try #require(
                document.classicAnchorCandidateAuditAnnotations.first(where: { annotation in
                    document.dataset?.trains.contains(where: { $0.id == annotation.trainID }) == true
                })
            )
            let candidate = try #require(
                run.candidates.first(where: { $0.id == annotation.candidateID })
            )
            let requestBefore = document.classicAnchorFocusRequestID
            document.selectedTrainIDs = []
            document.rasterVisibleWindowSeconds = 17

            document.focusClassicAnchorCandidate(candidate.id, adjustRasterReviewWindow: true)

            #expect(document.focusedClassicAnchorCandidateID == candidate.id)
            #expect(document.focusedClassicAnchorCandidate?.id == candidate.id)
            #expect(document.classicAnchorFocusRequestID == requestBefore + 1)
            #expect(document.selectedTrainIDsIncludingFocusedCandidate(for: .raster).contains(annotation.trainID))
            #expect(document.rasterVisibleWindowSeconds < 17)
        }
    }

    private func populateActiveState(in document: RasterDocument) async throws {
        document.runAdaptiveClassicAnchorDetection()
        let clock = ContinuousClock()
        let deadline = clock.now.advanced(by: .seconds(10))
        while document.isDetectorRunning, clock.now < deadline {
            try await Task.sleep(for: .milliseconds(10))
        }
        #expect(!document.isDetectorRunning)
        _ = try #require(document.classicAnchorDetectionRun)

        let annotation = ManualAnnotation(
            id: UUID(uuidString: "11111111-1111-1111-1111-111111111111")!,
            trainID: "fast_train",
            label: .burst,
            startSec: 0.1,
            endSec: 0.2,
            note: "Preserved manual evidence",
            createdAt: Date(timeIntervalSince1970: 100),
            updatedAt: Date(timeIntervalSince1970: 200)
        )
        let digest = String(repeating: "a", count: 64)
        let settingsDigest = String(repeating: "b", count: 64)
        let receipt = try #require(ManualAnnotationCSVApprovalReceipt(
            sourceFileDigest: digest,
            activeDatasetDigest: digest,
            approvedRunID: "preserved-run",
            approvedSettingsDigest: settingsDigest,
            approver: "Preserved Reviewer",
            approvedAt: Date(timeIntervalSince1970: 300),
            sourceSchemaVersion: ManualAnnotationCSVExporter.identitySchemaVersion,
            sourceRunID: "source-run",
            sourceReviewState: ManualAnnotationCSVReviewState.pendingConfirmation.rawValue,
            annotations: [annotation]
        ))
        let approvedBatch = try #require(ManualAnnotationCSVApprovedBatch(
            annotations: [annotation],
            approvalReceipt: receipt
        ))

        document.selectedTrainIDs = ["fast_train"]
        document.isiSelectedTrainIDs = ["slower_train"]
        document.isiStateSpaceSelectedTrainIDs = ["fast_train"]
        document.focusedClassicAnchorCandidateID = "preserved-candidate"
        document.classicAnchorFocusRequestID = 73
        document.classicAnchorReviewStatuses = ["preserved-candidate": .accepted]
        document.classicAnchorReviewInputs = [
            "preserved-candidate": STPDCandidateReviewInput(
                sourceCandidateID: "preserved-candidate",
                status: .accepted,
                reviewer: "Reviewer",
                note: "Preserve this review",
                reviewedAt: Date(timeIntervalSince1970: 400),
                reviewedRunID: "preserved-run"
            )
        ]
        document.manualAnnotationsByTrain = ["fast_train": [annotation]]
        document.approvedManualAnnotationImports = [approvedBatch]
        document.detectorStatusMessage = "Preserved detector result."
        document.detectorLastRunDate = Date(timeIntervalSince1970: 500)
        document.rawImportUnit = .milliseconds
        document.rawCSVHasHeader = false
        document.duplicateTimestampPolicy = .warnKeep
        document.requestedVisibleTrainCount = 1
        document.requestedISIVisibleTrainCount = 1
        document.requestedISIStateSpaceVisibleTrainCount = 1
        document.rasterVisibleWindowSeconds = 23
        document.rasterVisibleWindowUserLocked = true
        document.rasterSpikeTickHeightPx = 71
        document.loadedResultPackageURL = URL(fileURLWithPath: "/preserved/result.stpdresult")
        document.resultPackageReadbackErrorMessage = "Preserved readback notice."
        document.resultPackageReadRequestToken = 19
        document.resultPackageLoadCompletionID = 29
        document.isResultPackageExporting = true
        document.isManualAnnotationImporting = true
        document.isResultPackageReading = true
        document.isDetectorRunning = true
    }

    /// A transaction failure is allowed to change only the two user-facing import messages.
    /// Reflection intentionally includes private storage such as the loaded source URL, detector
    /// generation, and annotation cache so the regression test covers the complete document state.
    private func nonMessageStorageSnapshot(of document: RasterDocument) -> [String: String] {
        var snapshot: [String: String] = [:]
        for child in Mirror(reflecting: document).children {
            guard let label = child.label else { continue }
            let logicalLabel = label.drop(while: { $0 == "_" })
            guard logicalLabel != "statusMessage", logicalLabel != "lastErrorMessage" else {
                continue
            }
            snapshot[label] = String(reflecting: child.value)
        }
        return snapshot
    }

    private func withTemporaryDirectory<T>(
        _ body: (URL) throws -> T
    ) throws -> T {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("RasterDocumentImportTransactionTests-")
            .appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(
            at: directory,
            withIntermediateDirectories: true
        )
        defer { try? FileManager.default.removeItem(at: directory) }
        return try body(directory)
    }

    private func withTemporaryDirectory<T>(
        _ body: (URL) async throws -> T
    ) async throws -> T {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("RasterDocumentImportTransactionTests-")
            .appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(
            at: directory,
            withIntermediateDirectories: true
        )
        defer { try? FileManager.default.removeItem(at: directory) }
        return try await body(directory)
    }

    private static let activeCSV = """
        fast_train,slower_train
        0.000,0.000
        0.100,0.300
        0.106,0.320
        0.112,0.340
        0.200,0.360
        ,0.860
        ,1.360
        ,1.860
        """
}
