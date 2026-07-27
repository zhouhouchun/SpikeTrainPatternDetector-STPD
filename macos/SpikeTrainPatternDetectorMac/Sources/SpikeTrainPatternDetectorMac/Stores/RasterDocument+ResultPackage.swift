import AppKit
import Foundation
import STPDCore
import UniformTypeIdentifiers

extension RasterDocument {
    /// Cheap UI readiness check. The export action still performs the complete, fail-closed
    /// geometry, settings, authority, and table validation.
    var canExportCurrentResultPackage: Bool {
        guard dataset != nil, let run = classicAnchorDetectionRun else {
            return false
        }
        let activeCandidateIDs = Set(run.candidates.map(\.id))
        return classicAnchorReviewStatuses.allSatisfy { candidateID, status in
            guard activeCandidateIDs.contains(candidateID) else {
                return false
            }
            switch status {
            case .unreviewed, .needsReview:
                return true
            case .accepted, .rejected:
                guard let authored = classicAnchorReviewInputs[candidateID],
                      authored.reviewedRunID == run.runIdentity.runID else {
                    return false
                }
                let reviewer = authored.reviewer
                    .trimmingCharacters(in: .whitespacesAndNewlines)
                let reviewedAt = authored.reviewedAt?.timeIntervalSince1970
                return authored.status.rawValue == status.rawValue
                    && !reviewer.isEmpty
                    && reviewedAt?.isFinite == true
            }
        }
    }

    /// Freezes the exact public detector/review snapshot currently shown by the app.
    ///
    /// Core is the sole projection authority. The app supplies user-authored evidence, then Core
    /// deterministically recomputes and validates final events, ISI labels, and causal links.
    func currentResultPackageInput() throws -> STPDResultPackageInput {
        try currentResultPackageExportSeed().snapshot()
    }

    func currentResultPackage() throws -> STPDResultPackage {
        try STPDResultPackageBuilder.build(currentResultPackageInput())
    }

    func exportResultPackageWithPanel() {
        guard !isResultPackageExporting else {
            return
        }

        let seed: ResultPackageExportSeed
        do {
            seed = try currentResultPackageExportSeed()
        } catch {
            statusMessage = "Result-package export blocked."
            lastErrorMessage = error.localizedDescription
            return
        }

        let panel = NSSavePanel()
        let packageType =
            UTType(filenameExtension: "stpdresult", conformingTo: .package)
            ?? .package
        panel.allowedContentTypes = [packageType]
        panel.canCreateDirectories = true
        panel.isExtensionHidden = false
        panel.nameFieldStringValue = defaultResultPackageExportFileName(
            datasetName: seed.dataset.name,
            runID: seed.run.runIdentity.runID
        )
        panel.message = "Export a normalized, provenance-bound detector result package. Existing packages are never overwritten."

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        isResultPackageExporting = true
        statusMessage = "Exporting result package…"
        lastErrorMessage = nil

        Task { [weak self] in
            let outcome = await Task.detached(priority: .userInitiated) {
                do {
                    // Validation, authoritative projection, normalized table construction,
                    // hashing, and disk I/O all run away from the MainActor.
                    let input = try seed.snapshot()
                    let package = try STPDResultPackageBuilder.build(input)
                    try STPDResultPackageWriter.write(package, to: url)
                    return ResultPackageExportOutcome.success(
                        tableCount: package.manifest.tables.count,
                        sourceMode: package.manifest.sourceMode
                    )
                } catch {
                    return ResultPackageExportOutcome.failure(
                        message: error.localizedDescription
                    )
                }
            }.value

            guard let self else {
                return
            }
            self.isResultPackageExporting = false
            switch outcome {
            case let .success(tableCount, sourceMode):
                self.statusMessage =
                    "Exported \(tableCount)-table \(sourceMode) result package " +
                    "to \(url.lastPathComponent)."
                self.lastErrorMessage = nil
            case let .failure(message):
                self.statusMessage = "Result-package export failed."
                self.lastErrorMessage = message
            }
        }
    }

    /// Captures only immutable, Sendable source evidence on the MainActor.
    ///
    /// The expensive authority projection and package snapshot are intentionally
    /// deferred to `ResultPackageExportSeed.snapshot()` on a detached task.
    private func currentResultPackageExportSeed() throws -> ResultPackageExportSeed {
        guard let dataset, let run = classicAnchorDetectionRun else {
            throw STPDResultPackageError.invalidInput(
                "run detection before exporting a result package"
            )
        }
        let runtime = try ResultPackageRuntimeState.resolve(
            document: self,
            dataset: dataset
        )

        let candidateReviews = try classicAnchorReviewStatuses
            .sorted { $0.key < $1.key }
            .compactMap { candidateID, status -> STPDCandidateReviewInput? in
                let packageStatus: STPDCandidateReviewStatus
                switch status {
                case .unreviewed:
                    return nil
                case .accepted:
                    packageStatus = .accepted
                case .rejected:
                    packageStatus = .rejected
                case .needsReview:
                    packageStatus = .needsReview
                }
                if let authored = classicAnchorReviewInputs[candidateID],
                   authored.status == packageStatus,
                   authored.reviewedRunID == run.runIdentity.runID {
                    return authored
                }
                guard packageStatus == .needsReview else {
                    throw STPDResultPackageError.invalidInput(
                        "candidate \(candidateID) has status-only review data; " +
                            "review it again before authoritative export"
                    )
                }
                return STPDCandidateReviewInput(
                    sourceCandidateID: candidateID,
                    status: .needsReview,
                    note: "source=legacy_or_imported_status_only"
                )
            }

        let currentSettings = DetectionRunSettingsSnapshot.make(
            bandSettings: adaptiveDetectorBandSettings,
            qualitySettings: qualitySettings,
            refractoryAction: .warnOnly,
            stateTuning: stateDetectorTuning,
            detectorParameters: detectorParameterSettings,
            manualThresholdProfile: runtime.manualThresholdProfile,
            frameworkPolicy: HybridPatternDetectionFramework.plan.policy,
            useAdaptiveV2Canonicalization:
                runtime.useAdaptiveV2Canonicalization,
            manualThresholdScope: runtime.manualThresholdScope
        )
        return ResultPackageExportSeed(
            dataset: dataset,
            run: run,
            manualAnnotations: runtime.manualAnnotations,
            candidateReviews: candidateReviews,
            currentSettingsSnapshot: currentSettings
        )
    }

    private func defaultResultPackageExportFileName(
        datasetName: String,
        runID: String
    ) -> String {
        let safeRunID = resultPackageSafeExportBaseName(runID)
        let runSuffix = String(safeRunID.suffix(12))
        return "\(resultPackageSafeExportBaseName(datasetName))_\(runSuffix)_results.stpdresult"
    }

    private func resultPackageSafeExportBaseName(_ value: String) -> String {
        let sanitized = value
            .replacingOccurrences(of: "/", with: "_")
            .replacingOccurrences(of: ":", with: "_")
        return sanitized.isEmpty ? "spike_train_events" : sanitized
    }
}

private struct ResultPackageExportSeed: Sendable {
    let dataset: SpikeDataset
    let run: ClassicAnchorDetectionRun
    let manualAnnotations: [ManualAnnotation]
    let candidateReviews: [STPDCandidateReviewInput]
    let currentSettingsSnapshot: DetectionRunSettingsSnapshot

    func snapshot() throws -> STPDResultPackageInput {
        try STPDResultPackageBuilder.validateExportPreflight(
            dataset: dataset,
            run: run,
            currentSettingsSnapshot: currentSettingsSnapshot
        )
        return try STPDResultPackageInput.snapshot(
            dataset: dataset,
            run: run,
            manualAnnotations: manualAnnotations,
            candidateReviews: candidateReviews
        )
    }
}

private enum ResultPackageExportOutcome: Sendable {
    case success(tableCount: Int, sourceMode: String)
    case failure(message: String)
}

private struct ResultPackageRuntimeState: Sendable {
    let manualAnnotations: [ManualAnnotation]
    let manualThresholdProfile: ManualThresholdProfile
    let manualThresholdScope: ManualThresholdScope
    let useAdaptiveV2Canonicalization: Bool

    @MainActor
    static func resolve(
        document: RasterDocument,
        dataset: SpikeDataset
    ) throws -> ResultPackageRuntimeState {
        let annotationsByTrain = document.manualAnnotationsByTrain
        let adaptiveV2 = document.useAdaptiveV2Canonicalization
        let burstMode = document.manualBurstMode
        let hfsMode = document.manualHFSMode
        let hfTonicMode = document.manualHFTonicMode
        let tonicMode = document.manualTonicMode
        let pauseMode = document.manualPauseMode

        func isi(
            _ milliseconds: Double,
            mode: ThresholdMode
        ) -> ManualISIThreshold {
            return mode != .automatic && milliseconds > 0
                ? ManualISIThreshold(
                    mode: mode,
                    valueSec: milliseconds / 1000
                )
                : .automatic
        }
        func count(
            _ value: Int,
            mode: ThresholdMode
        ) -> ManualSpikeCountThreshold {
            return mode == .hardGate && value > 0
                ? ManualSpikeCountThreshold(mode: .hardGate, value: value)
                : .automatic
        }

        let profile = ManualThresholdProfile(
            burst: BurstManualThresholds(
                seedUpperISI: isi(
                    document.manualBurstSeedMaxISIMs,
                    mode: burstMode
                ),
                bridgeUpperISI: isi(
                    document.manualBurstBridgeMaxISIMs,
                    mode: burstMode
                ),
                minSpikes: count(
                    document.manualBurstMinSpikes,
                    mode: burstMode
                )
            ),
            hfs: HFSManualThresholds(
                minSpikes: count(document.manualHFSMinSpikes, mode: hfsMode),
                minDurationSec: isi(
                    document.manualHFSMinDurationMs,
                    mode: hfsMode
                )
            ),
            hfTonic: HFTonicManualThresholds(
                minSpikes: count(
                    document.manualHFTonicMinSpikes,
                    mode: hfTonicMode
                ),
                isiFloor: isi(
                    document.manualHFTonicMinISIMs,
                    mode: hfTonicMode
                ),
                isiUpper: isi(
                    document.manualHFTonicMaxISIMs,
                    mode: hfTonicMode
                )
            ),
            tonic: TonicManualThresholds(
                minSpikes: count(
                    document.manualTonicMinSpikes,
                    mode: tonicMode
                ),
                isiLower: isi(
                    document.manualTonicMinISIMs,
                    mode: tonicMode
                ),
                isiUpper: isi(
                    document.manualTonicMaxISIMs,
                    mode: tonicMode
                )
            ),
            pause: PauseManualThresholds(
                isiLower: isi(
                    document.manualPauseMinISIMs,
                    mode: pauseMode
                )
            )
        )
        let scope = ManualThresholdScope.resolve(
            kind: document.manualThresholdScopeKind,
            focusedTrainID: document.focusedClassicAnchorCandidate?.trainID,
            selectedTrainIDs: document.selectedTrainIDs,
            allTrainIDs: dataset.trains.map(\.id)
        )
        let annotations = annotationsByTrain.keys.sorted().flatMap {
            annotationsByTrain[$0, default: []]
        }
        return ResultPackageRuntimeState(
            manualAnnotations: annotations,
            manualThresholdProfile: profile,
            manualThresholdScope: scope,
            useAdaptiveV2Canonicalization: adaptiveV2
        )
    }
}

enum ResultPackageAppBuildIdentity {
    static var current: String {
        let environment = ProcessInfo.processInfo.environment["STPD_BUILD_IDENTIFIER"]
        let bundled = Bundle.main.object(
            forInfoDictionaryKey: "STPDBuildIdentifier"
        ) as? String
        for value in [bundled, environment] {
            let normalized = value?
                .trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
            if !normalized.isEmpty,
               normalized != DetectionRunIdentity.unavailable {
                return normalized
            }
        }
        return DetectionRunIdentity.unavailable
    }
}

enum ResultPackageAppReviewerIdentity {
    static var current: String {
        for value in [NSFullUserName(), NSUserName()] {
            let normalized = value
                .trimmingCharacters(in: .whitespacesAndNewlines)
            if !normalized.isEmpty {
                return normalized
            }
        }
        return ""
    }
}
