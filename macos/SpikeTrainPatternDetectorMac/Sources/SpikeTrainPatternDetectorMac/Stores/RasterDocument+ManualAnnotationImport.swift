import AppKit
import Foundation
import STPDCore
import UniformTypeIdentifiers

private enum ManualAnnotationAppImportError: LocalizedError {
    case activeDatasetChanged
    case activeRunChanged
    case authorityContextChanged
    case detectorRunRequired
    case detectorRunInProgress
    case importAlreadyInProgress
    case invalidApprover
    case manualAnnotationStateChanged
    case resultPackageExportInProgress

    var errorDescription: String? {
        switch self {
        case .activeDatasetChanged:
            return "The active dataset changed after the file was checked. Import it again for the current dataset."
        case .activeRunChanged:
            return "The detector run changed after the file was checked. Import it again for the current run."
        case .authorityContextChanged:
            return "Candidate reviews or detector settings changed while the import was being checked. Import it again for the current authority context."
        case .detectorRunRequired:
            return "Run detection before importing authoritative manual annotations."
        case .detectorRunInProgress:
            return "Wait for the active detector run to finish before importing authoritative manual annotations."
        case .importAlreadyInProgress:
            return "A manual annotation import is already being validated."
        case .invalidApprover:
            return "An approver name is required before manual annotations can become authoritative."
        case .manualAnnotationStateChanged:
            return "The current manual annotations changed while the import was being checked. Import the file again before replacing them."
        case .resultPackageExportInProgress:
            return "Wait for the active result-package export to finish before importing authoritative manual annotations."
        }
    }
}

private enum ManualAnnotationImportPreflightOutcome: Sendable {
    case success
    case failure(message: String)
}

private enum ManualAnnotationImportLoadOutcome: Sendable {
    case success(ManualAnnotationCSVGatedImport)
    case failure(message: String)
}

@MainActor
extension RasterDocument {
    var canImportAuthoritativeManualAnnotations: Bool {
        activeDatasetScientificStanding.permitsSealedResultExport
            && dataset != nil
            && classicAnchorDetectionRun != nil
            && !isDetectorRunning
            && !isManualAnnotationImporting
            && !isResultPackageExporting
    }

    /// Reads the selected file as raw bytes, gates it against the active dataset, and requires a
    /// separate human approval action before replacing the authoritative in-memory annotations.
    func importManualAnnotationsWithPanel() {
        guard !isManualAnnotationImporting else {
            statusMessage = "Manual annotation import is already in progress."
            lastErrorMessage =
                ManualAnnotationAppImportError.importAlreadyInProgress
                    .localizedDescription
            return
        }
        guard activeDatasetScientificStanding.permitsSealedResultExport else {
            statusMessage = "Manual annotation import blocked."
            lastErrorMessage = ActiveDatasetScientificStandingError
                .canonicalConfirmationRequired.localizedDescription
            return
        }
        guard !isDetectorRunning else {
            statusMessage = "Manual annotation import blocked while detection is running."
            lastErrorMessage =
                ManualAnnotationAppImportError.detectorRunInProgress
                    .localizedDescription
            return
        }
        guard !isResultPackageExporting else {
            statusMessage =
                "Manual annotation import blocked while a result package is being exported."
            lastErrorMessage =
                ManualAnnotationAppImportError.resultPackageExportInProgress
                    .localizedDescription
            return
        }
        guard dataset != nil else {
            statusMessage =
                "Load a dataset before importing manual annotations."
            lastErrorMessage = nil
            return
        }
        guard classicAnchorDetectionRun != nil else {
            statusMessage =
                "Run detection before importing manual annotations."
            lastErrorMessage =
                ManualAnnotationAppImportError.detectorRunRequired
                    .localizedDescription
            return
        }

        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = false
        panel.canChooseDirectories = false
        panel.allowedContentTypes = [.commaSeparatedText, .plainText]
        panel.message = "Choose an identity-bound manual annotation CSV for the active dataset."

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        // The panel may stay open while another window starts an operation.
        // Recheck and reserve the import slot before reading a single byte.
        guard !isManualAnnotationImporting else {
            statusMessage = "Manual annotation import is already in progress."
            lastErrorMessage =
                ManualAnnotationAppImportError.importAlreadyInProgress
                    .localizedDescription
            return
        }
        guard !isDetectorRunning else {
            statusMessage =
                "Manual annotation import blocked while detection is running."
            lastErrorMessage =
                ManualAnnotationAppImportError.detectorRunInProgress
                    .localizedDescription
            return
        }
        guard !isResultPackageExporting else {
            statusMessage =
                "Manual annotation import blocked while a result package is being exported."
            lastErrorMessage =
                ManualAnnotationAppImportError.resultPackageExportInProgress
                    .localizedDescription
            return
        }
        guard let activeDataset = dataset,
              let activeRun = classicAnchorDetectionRun else {
            statusMessage =
                "Run detection before importing manual annotations."
            lastErrorMessage =
                ManualAnnotationAppImportError.detectorRunRequired
                    .localizedDescription
            return
        }

        let activeDatasetDigest =
            DetectionDatasetSnapshot.make(dataset: activeDataset).digest
        let activeRunID = activeRun.runIdentity.runID
        let activeSettingsDigest =
            activeRun.runIdentity.settingsDigest

        isManualAnnotationImporting = true
        statusMessage =
            "Reading and validating \(url.lastPathComponent)…"
        lastErrorMessage = nil

        Task { @MainActor [weak self] in
            guard let self else {
                return
            }
            defer {
                self.isManualAnnotationImporting = false
            }

            let loadOutcome = await Task.detached(
                priority: .userInitiated
            ) {
                do {
                    // Authority is bound to the exact selected bytes. File I/O,
                    // strict UTF-8 decoding, CSV parsing, geometry resolution,
                    // and the potentially quadratic gate all stay off MainActor.
                    let data = try Data(contentsOf: url)
                    let imported = try ManualAnnotationCSVImporter
                        .importIdentityBound(data: data)
                    let gated = ManualAnnotationCSVImporter.gate(
                        imported,
                        activeDataset: activeDataset
                    )
                    return ManualAnnotationImportLoadOutcome.success(
                        gated
                    )
                } catch {
                    return ManualAnnotationImportLoadOutcome.failure(
                        message: error.localizedDescription
                    )
                }
            }.value

            do {
                let gated: ManualAnnotationCSVGatedImport
                switch loadOutcome {
                case .success(let value):
                    gated = value
                case .failure(let message):
                    throw STPDResultPackageError.invalidInput(message)
                }

                guard let currentDataset = self.dataset,
                      DetectionDatasetSnapshot.make(
                        dataset: currentDataset
                      ).digest == activeDatasetDigest,
                      activeDatasetDigest
                        == gated.activeDatasetDigest else {
                    throw ManualAnnotationAppImportError
                        .activeDatasetChanged
                }
                guard !self.isDetectorRunning else {
                    throw ManualAnnotationAppImportError
                        .detectorRunInProgress
                }
                guard !self.isResultPackageExporting else {
                    throw ManualAnnotationAppImportError
                        .resultPackageExportInProgress
                }
                guard let currentRun = self.classicAnchorDetectionRun,
                      currentRun.runIdentity.runID == activeRunID,
                      currentRun.runIdentity.settingsDigest
                        == activeSettingsDigest else {
                    throw ManualAnnotationAppImportError.activeRunChanged
                }

                guard gated.authority
                        == .eligibleAfterExplicitConfirmation else {
                    self.presentReviewOnlyManualAnnotationImport(
                        gated,
                        fileName: url.lastPathComponent
                    )
                    self.statusMessage =
                        "Manual annotations from \(url.lastPathComponent) were not applied."
                    self.lastErrorMessage =
                        self.manualAnnotationImportBlockerSummary(
                            gated.blockers
                        )
                    return
                }

                guard let approver =
                        self.requestManualAnnotationImportApproval(
                            gated,
                            fileName: url.lastPathComponent,
                            datasetName: activeDataset.name
                        ) else {
                    self.statusMessage =
                        "Manual annotation import cancelled; no annotations were changed."
                    self.lastErrorMessage = nil
                    return
                }

                guard let approval = ManualAnnotationCSVApproval(
                    sourceFileDigest: gated.sourceFileDigest,
                    activeDatasetDigest: gated.activeDatasetDigest,
                    approvedRunID: activeRunID,
                    approvedSettingsDigest: activeSettingsDigest,
                    approver: approver,
                    approvedAt: Date()
                ) else {
                    throw ManualAnnotationAppImportError.invalidApprover
                }
                let approvedBatch = try gated.authoritativeBatch(
                    approval: approval
                )
                let replacedAnnotations =
                    self.manualAnnotationsByTrain
                let replacedApprovedImports =
                    self.approvedManualAnnotationImports
                let preflightSeed = try self
                    .currentManualAnnotationImportPreflightSeed(
                        approvedBatch
                    )

                self.statusMessage =
                    "Validating approved manual annotations against the current result package…"
                let preflightOutcome = await Task.detached(
                    priority: .userInitiated
                ) {
                    do {
                        let input = try preflightSeed.snapshot()
                        _ = try STPDResultPackageBuilder.build(input)
                        return ManualAnnotationImportPreflightOutcome
                            .success
                    } catch {
                        return ManualAnnotationImportPreflightOutcome
                            .failure(
                                message: error.localizedDescription
                            )
                    }
                }.value

                switch preflightOutcome {
                case .success:
                    break
                case .failure(let message):
                    throw STPDResultPackageError.invalidInput(message)
                }
                guard !self.isDetectorRunning else {
                    throw ManualAnnotationAppImportError
                        .detectorRunInProgress
                }
                guard !self.isResultPackageExporting else {
                    throw ManualAnnotationAppImportError
                        .resultPackageExportInProgress
                }
                guard self.manualAnnotationsByTrain
                        == replacedAnnotations,
                      self.approvedManualAnnotationImports
                        == replacedApprovedImports else {
                    throw ManualAnnotationAppImportError
                        .manualAnnotationStateChanged
                }
                let currentSeed = try self
                    .currentManualAnnotationImportPreflightSeed(
                        approvedBatch
                    )
                guard preflightSeed.hasSameAuthorityContext(
                    as: currentSeed
                ) else {
                    throw ManualAnnotationAppImportError
                        .authorityContextChanged
                }

                // The sealed batch is the only authoritative in-memory form of
                // imported annotations. Never mirror its payload into the bare
                // local-authoring dictionary, where the receipt could be lost.
                self.manualAnnotationsByTrain = [:]
                self.approvedManualAnnotationImports = [
                    approvedBatch
                ]
                self.statusMessage =
                    "Applied \(approvedBatch.annotationCount) approved manual annotation(s) from \(url.lastPathComponent)."
                self.lastErrorMessage = nil
            } catch {
                self.statusMessage =
                    "Manual annotation import failed; no annotations were changed."
                self.lastErrorMessage = error.localizedDescription
            }
        }
    }

    private func requestManualAnnotationImportApproval(
        _ gated: ManualAnnotationCSVGatedImport,
        fileName: String,
        datasetName: String
    ) -> String? {
        let localAnnotationCount: Int =
            manualAnnotationsByTrain.values.reduce(0) { $0 + $1.count }
        let approvedImportAnnotationCount: Int =
            approvedManualAnnotationImports.reduce(0) {
                $0 + $1.annotationCount
            }
        let existingCount = localAnnotationCount + approvedImportAnnotationCount
        let alert = NSAlert()
        alert.alertStyle = .warning
        alert.messageText = "Approve manual annotation import?"
        alert.informativeText = """
        File: \(fileName)
        Active dataset: \(datasetName)
        Imported annotations: \(gated.annotationCount)
        Current annotations to replace: \(existingCount)
        Source SHA-256: \(gated.sourceFileDigest)
        Dataset SHA-256: \(gated.activeDatasetDigest)

        The approver entry is a user-supplied audit attribution. The app does not authenticate or verify this identity.
        Approval replaces the current manual annotation set. It does not rerun or modify the detector.
        """
        alert.addButton(withTitle: "Approve and Replace")
        alert.addButton(withTitle: "Cancel")

        let approverLabel = NSTextField(
            labelWithString:
                "Approver audit attribution (not authenticated)"
        )
        approverLabel.font = .systemFont(ofSize: NSFont.smallSystemFontSize, weight: .semibold)
        let approverField = NSTextField(string: ResultPackageAppReviewerIdentity.current)
        approverField.placeholderString = "Required"
        approverField.frame.size = NSSize(width: 420, height: 24)

        let accessory = NSStackView(views: [approverLabel, approverField])
        accessory.orientation = .vertical
        accessory.alignment = .leading
        accessory.spacing = 6
        accessory.frame.size = NSSize(width: 420, height: 50)
        alert.accessoryView = accessory
        alert.window.initialFirstResponder = approverField

        guard alert.runModal() == .alertFirstButtonReturn else {
            return nil
        }
        let approver = approverField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !approver.isEmpty else {
            return ""
        }
        return approver
    }

    private func presentReviewOnlyManualAnnotationImport(
        _ gated: ManualAnnotationCSVGatedImport,
        fileName: String
    ) {
        let alert = NSAlert()
        alert.alertStyle = .warning
        alert.messageText = "Manual annotations were not applied"
        alert.informativeText = """
        File: \(fileName)
        Parsed annotations: \(gated.annotationCount)
        Identity: \(manualAnnotationImportIdentityDescription(gated.identity))
        Authority: review only
        Blockers: \(manualAnnotationImportBlockerSummary(gated.blockers))
        Source SHA-256: \(gated.sourceFileDigest)
        Active dataset SHA-256: \(gated.activeDatasetDigest)

        Review-only imports cannot alter authoritative manual annotations.
        """
        alert.addButton(withTitle: "OK")
        alert.runModal()
    }

    private func manualAnnotationImportIdentityDescription(
        _ identity: ManualAnnotationCSVIdentity
    ) -> String {
        switch identity {
        case .matchingDataset: return "matching active dataset"
        case .datasetMismatch: return "dataset mismatch"
        case .legacyUnbound: return "legacy file without dataset identity"
        case .malformedIdentity: return "malformed identity envelope"
        case .unsupportedSchema: return "unsupported schema"
        }
    }

    private func manualAnnotationImportBlockerSummary(
        _ blockers: [ManualAnnotationCSVAuthorityBlocker]
    ) -> String {
        guard !blockers.isEmpty else {
            return "none"
        }
        return blockers.map { blocker in
            switch blocker {
            case .legacyUnbound:
                return "legacy file is not identity-bound"
            case .malformedIdentity:
                return "identity envelope is malformed"
            case .unsupportedSchema:
                return "schema version is unsupported"
            case .datasetMismatch:
                return "file dataset does not match the active dataset"
            case .exactSourceBytesUnavailable:
                return "the import was decoded before hashing, so the exact selected file bytes are unavailable"
            case .reviewOnlyState:
                return "file review_state is review_only or missing"
            case .geometryIncompatible:
                return "one or more annotations are incompatible with the active dataset geometry"
            case .skippedRows(let count):
                return "\(count) row(s) were skipped"
            case .unsupportedLabels(let labels):
                return "unsupported label(s): \(labels.sorted().joined(separator: ", "))"
            case .emptyImport:
                return "the file contains no importable annotations"
            case .nonFiniteEditTimestamps(let ids):
                return "\(ids.count) annotation(s) have non-finite edit timestamps"
            case .conflictingLatestRevisions(let ids):
                return "\(ids.count) annotation ID(s) have conflicting latest revisions"
            case .ambiguousEqualTimestampOverlap:
                return "equal-timestamp overlapping edits are ambiguous"
            case .missingAnnotatorIdentity(let ids):
                return "\(ids.count) annotation(s) lack a known original annotator identity"
            }
        }.joined(separator: "; ")
    }

}
