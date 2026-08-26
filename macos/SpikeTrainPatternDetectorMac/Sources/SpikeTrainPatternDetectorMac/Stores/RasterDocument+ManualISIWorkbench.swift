import AppKit
import Foundation
import STPDCore
import STPDTabularIO
import UniformTypeIdentifiers

enum RasterManualAnnotationEditMode: String, CaseIterable, Hashable {
    case apply
    case erase

    var displayNameZH: String {
        switch self {
        case .apply: "标记"
        case .erase: "擦除"
        }
    }
}

enum ManualISIUndoSnapshot {
    case canonical(CanonicalManualISILabelDraft)
    case local([String: [ManualAnnotation]])
}

private enum CanonicalManualISIDraftLoadOutcome: Sendable {
    case success(CanonicalManualISIDraftImport, CanonicalManualISILabelDraft)
    case failure(String)
}

enum CanonicalManualWorkbenchError: Error, LocalizedError {
    case noPersistedConfirmation
    case activityModeNotSupported
    case temporalContractNotEligible
    case projectionIdentityMismatch
    case unknownTrain

    var errorDescription: String? {
        switch self {
        case .noPersistedConfirmation:
            return "请先确认并保存科学导入，然后再进入规范人工审核。"
        case .activityModeNotSupported:
            return "生物学模式人工标记目前要求\(ScientificDatasetActivityMode.putativeSingleUnit.activityModeDisplaySource)数据。"
        case .temporalContractNotEligible:
            return "人工模式审核要求连续、无试次结构，并确认所有 spike-train 数据流覆盖整个导入片段。"
        case .projectionIdentityMismatch:
            return "重新构建的数据集身份与已保存的确认记录不一致。"
        case .unknownTrain:
            return "所选 spike train 不属于当前规范数据集。"
        }
    }
}

@MainActor
extension RasterDocument {
    /// Enables raster authoring without creating a second source of truth. A persisted canonical
    /// import is first projected into its identity-bound manual draft; legacy/demo data keeps using
    /// the explicitly non-authoritative local sidecar.
    func setRasterManualAnnotationModeEnabled(_ enabled: Bool) {
        guard enabled else {
            manualAnnotationModeEnabled = false
            return
        }
        if hasPersistedCanonicalManualSource,
           (canonicalManualDataset == nil || canonicalManualISILabelDraft == nil),
           !prepareCanonicalManualWorkbench() {
            manualAnnotationModeEnabled = false
            return
        }
        manualAnnotationModeEnabled = true
    }

    var hasPersistedCanonicalManualSource: Bool {
        scientificImportCoordinator.persistedConfirmation != nil
    }

    @discardableResult
    func prepareCanonicalManualWorkbench() -> Bool {
        do {
            guard let persisted = scientificImportCoordinator.persistedConfirmation else {
                throw CanonicalManualWorkbenchError.noPersistedConfirmation
            }
            let base = persisted.baseConfirmation
            guard base.activityMode == .putativeSingleUnit else {
                throw CanonicalManualWorkbenchError.activityModeNotSupported
            }
            guard base.recordingSegment.regime == .continuousUntrialed,
                  base.recordingSegment.importedExcerptCoverage
                    == .allSpikeTrainsFullImportedExcerpt else {
                throw CanonicalManualWorkbenchError.temporalContractNotEligible
            }
            let shadow = try CanonicalScientificImportProjector.project(
                base.validatedImport
            )
            guard shadow.fingerprint == persisted.canonicalFingerprint else {
                throw CanonicalManualWorkbenchError.projectionIdentityMismatch
            }
            let matchingDraft = canonicalManualISILabelDraft?.canonicalFingerprint
                == shadow.fingerprint
            if !matchingDraft { manualISIUndoStack.removeAll() }
            let retainedDraft = matchingDraft
                ? canonicalManualISILabelDraft
                : CanonicalManualISILabelDraft(canonicalFingerprint: shadow.fingerprint)
            let retainedConfirmation = matchingDraft
                ? confirmedCanonicalManualLabels
                : nil

            let expectedStanding = ActiveDatasetScientificStanding
                .canonicalConfirmedExploration(
                    datasetDigest: shadow.fingerprint.datasetDigest
                )
            if activeDatasetScientificStanding != expectedStanding {
                installCanonicalDatasetForExploration(
                    shadow.dataset,
                    fingerprint: shadow.fingerprint
                )
            }
            canonicalManualDataset = shadow.dataset
            canonicalManualISILabelDraft = retainedDraft
            confirmedCanonicalManualLabels = retainedConfirmation
            let isiCount = shadow.dataset.spikeTrains.reduce(0) {
                $0 + max(0, $1.rawTimestamps.count - 1)
            }
            statusMessage = "已进入人工 ISI 分析：\(shadow.dataset.spikeTrains.count) 条 spike train，共 \(isiCount) 个 ISI。"
            lastErrorMessage = nil
            return true
        } catch {
            canonicalManualDataset = nil
            canonicalManualISILabelDraft = nil
            confirmedCanonicalManualLabels = nil
            statusMessage = "无法进入人工 ISI 分析。"
            lastErrorMessage = error.localizedDescription
            return false
        }
    }

    func canonicalManualISIRows(
        for trainID: String
    ) -> [CanonicalManualISILabelRow] {
        guard let dataset = canonicalManualDataset,
              let persisted = scientificImportCoordinator.persistedConfirmation,
              let draft = canonicalManualISILabelDraft,
              dataset.spikeTrains.contains(where: {
                  $0.semanticID.semanticID.canonicalText == trainID
              }) else { return [] }
        return (try? CanonicalManualISILabelProjector.rows(
            dataset: dataset,
            fingerprint: persisted.canonicalFingerprint,
            draft: draft
        ).filter { $0.trainID.semanticID.canonicalText == trainID }) ?? []
    }

    @discardableResult
    func applyCanonicalManualISILabel(
        _ label: ManualAnnotationLabel,
        trainID: String,
        isiIndices: Set<Int>
    ) -> Bool {
        guard let dataset = canonicalManualDataset,
              let train = dataset.spikeTrains.first(where: {
                  $0.semanticID.semanticID.canonicalText == trainID
              }),
              let draft = canonicalManualISILabelDraft,
              !isiIndices.isEmpty,
              validateManualAuthoringSelection(
                  label: label,
                  isiIndices: isiIndices,
                  maximumISIIndex: max(train.rawTimestamps.count - 1, 0)
              ) else { return false }
        let updated = draft.applying(
            label: label,
            trainID: train.semanticID,
            isiIndices: isiIndices
        )
        guard updated != draft else { return false }
        recordManualISIUndo(.canonical(draft))
        canonicalManualISILabelDraft = updated
        confirmedCanonicalManualLabels = nil
        statusMessage = "已将 \(label.displayNameZH) 应用于 \(isiIndices.count) 个 ISI。"
        lastErrorMessage = nil
        return true
    }

    func clearCanonicalManualISILabel(
        track: ManualAnnotationSemanticTrack,
        trainID: String,
        isiIndices: Set<Int>
    ) {
        guard let dataset = canonicalManualDataset,
              let train = dataset.spikeTrains.first(where: {
                  $0.semanticID.semanticID.canonicalText == trainID
              }),
              let draft = canonicalManualISILabelDraft,
              !isiIndices.isEmpty else { return }
        let updated = draft.clearing(
            track: track,
            trainID: train.semanticID,
            isiIndices: isiIndices
        )
        guard updated != draft else { return }
        recordManualISIUndo(.canonical(draft))
        canonicalManualISILabelDraft = updated
        confirmedCanonicalManualLabels = nil
        statusMessage = "已清除 \(isiIndices.count) 个 ISI 上的\(track.displayNameZH)标记。"
        lastErrorMessage = nil
    }

    var canonicalManualUnreviewedISICount: Int {
        guard let dataset = canonicalManualDataset,
              let persisted = scientificImportCoordinator.persistedConfirmation,
              let draft = canonicalManualISILabelDraft else { return 0 }
        let rows = try? CanonicalManualISILabelProjector.rows(
            dataset: dataset,
            fingerprint: persisted.canonicalFingerprint,
            draft: draft
        )
        return rows?.lazy.filter { !$0.isReviewed }.count ?? 0
    }

    func confirmCompleteCanonicalManualReview() {
        do {
            guard let dataset = canonicalManualDataset,
                  let persisted = scientificImportCoordinator.persistedConfirmation,
                  let draft = canonicalManualISILabelDraft else {
                throw CanonicalManualWorkbenchError.noPersistedConfirmation
            }
            guard let reviewer = localManualAnnotator else {
                throw CanonicalManualISILabelReviewError.emptyReviewer
            }
            confirmedCanonicalManualLabels = try ConfirmedCanonicalManualISILabels
                .confirmComplete(
                    draft: draft,
                    dataset: dataset,
                    fingerprint: persisted.canonicalFingerprint,
                    reviewer: reviewer
                )
            statusMessage = "Canonical manual review confirmed. It is ready for result-package sealing."
            lastErrorMessage = nil
        } catch {
            confirmedCanonicalManualLabels = nil
            statusMessage = "Manual review confirmation blocked."
            lastErrorMessage = error.localizedDescription
        }
    }

    var canExportCanonicalManualResultPackage: Bool {
        guard let persisted = scientificImportCoordinator.persistedConfirmation,
              let confirmed = confirmedCanonicalManualLabels else { return false }
        return persisted.canonicalFingerprint == confirmed.canonicalFingerprint
            && !isResultPackageExporting
    }

    var canExportCanonicalManualISILabelDraft: Bool {
        guard let persisted = scientificImportCoordinator.persistedConfirmation,
              let draft = canonicalManualISILabelDraft,
              canonicalManualDataset != nil else { return false }
        return persisted.canonicalFingerprint == draft.canonicalFingerprint
            && !isResultPackageExporting
            && !isCanonicalManualISIDraftImporting
    }

    var canImportCanonicalManualISILabelDraft: Bool {
        canonicalManualDataset != nil
            && scientificImportCoordinator.persistedConfirmation != nil
            && canonicalManualISILabelDraft != nil
            && !isCanonicalManualISIDraftImporting
            && !isResultPackageExporting
    }

    /// Imports only this app's identity-bound, one-sheet-per-train XLSX draft. A full Core
    /// reconstruction must match the current persisted import before the user can replace a draft.
    func importCanonicalManualISILabelDraftWithPanel() {
        guard canImportCanonicalManualISILabelDraft,
              let persisted = scientificImportCoordinator.persistedConfirmation,
              let dataset = canonicalManualDataset,
              let existingDraft = canonicalManualISILabelDraft else {
            statusMessage = "当前没有可接收人工 ISI 草稿的已确认数据集。"
            return
        }
        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = false
        panel.canChooseDirectories = false
        panel.allowedContentTypes = [UTType(filenameExtension: "xlsx") ?? .data]
        panel.message = "选择本应用导出的人工 ISI 草稿 XLSX。导入前会核对数据集、确认记录、每个 ISI 与精确微秒时间戳。"
        guard panel.runModal() == .OK, let url = panel.url else { return }

        let expectedFingerprint = persisted.canonicalFingerprint
        let expectedConfirmationDigest = persisted.confirmationRecordDigest
        isCanonicalManualISIDraftImporting = true
        statusMessage = "正在验证人工 ISI 草稿 \(url.lastPathComponent)…"
        lastErrorMessage = nil

        Task { @MainActor [weak self] in
            guard let self else { return }
            defer { self.isCanonicalManualISIDraftImporting = false }
            let outcome = await Task.detached(priority: .userInitiated) {
                () -> CanonicalManualISIDraftLoadOutcome in
                do {
                    let data = try Data(contentsOf: url, options: [.mappedIfSafe])
                    let imported = try ManualISIResultXLSXImporter.import(data: data)
                    let draft = try imported.validatedDraft(
                        dataset: dataset,
                        persistedImport: persisted
                    )
                    return .success(imported, draft)
                } catch {
                    return .failure(error.localizedDescription)
                }
            }.value

            guard self.scientificImportCoordinator.persistedConfirmation?.canonicalFingerprint
                    == expectedFingerprint,
                  self.scientificImportCoordinator.persistedConfirmation?
                    .confirmationRecordDigest == expectedConfirmationDigest,
                  self.canonicalManualDataset == dataset,
                  self.canonicalManualISILabelDraft == existingDraft else {
                self.statusMessage = "人工 ISI 草稿导入已取消。"
                self.lastErrorMessage = "验证期间当前数据或人工草稿发生变化；请重新导入。"
                return
            }
            switch outcome {
            case .failure(let message):
                self.statusMessage = "人工 ISI 草稿未导入。"
                self.lastErrorMessage = message
            case .success(let imported, let replacement):
                let alert = NSAlert()
                alert.messageText = "替换当前人工 ISI 草稿？"
                alert.informativeText = "已验证 \(imported.rowCount) 个真实 ISI；其中 \(imported.labeledISIIndexCount) 个 ISI 带有人工标签（共 \(imported.decisions.count) 条轨道标签）。这会替换当前草稿，但可通过“撤销上一步”恢复。"
                alert.addButton(withTitle: "替换当前草稿")
                alert.addButton(withTitle: "取消")
                guard alert.runModal() == .alertFirstButtonReturn else {
                    self.statusMessage = "已取消人工 ISI 草稿导入。"
                    return
                }
                self.recordManualISIUndo(.canonical(existingDraft))
                self.canonicalManualISILabelDraft = replacement
                self.confirmedCanonicalManualLabels = nil
                self.statusMessage = "已导入人工 ISI 草稿：\(imported.labeledISIIndexCount) 个已标记 ISI。"
                self.lastErrorMessage = nil
            }
        }
    }

    func canonicalManualISILabelDraftCSV() throws -> String {
        guard let persisted = scientificImportCoordinator.persistedConfirmation,
              let draft = canonicalManualISILabelDraft else {
            throw CanonicalManualWorkbenchError.noPersistedConfirmation
        }
        return try CanonicalManualISIDraftCSVExporter.csv(
            persistedImport: persisted,
            draft: draft
        )
    }

    private func canonicalManualISIExportContent(
        persisted: PersistedConfirmedScientificImportManifest,
        draft: CanonicalManualISILabelDraft
    ) throws -> ManualISIResultExportContent {
        guard let dataset = canonicalManualDataset else {
            throw CanonicalManualWorkbenchError.noPersistedConfirmation
        }
        let table = try CanonicalManualISIDraftCSVExporter.table(
            persistedImport: persisted,
            draft: draft
        )
        let rows = try CanonicalManualISILabelProjector.rows(
            dataset: dataset,
            fingerprint: persisted.canonicalFingerprint,
            draft: draft
        )
        let rowsByTrain = Dictionary(grouping: rows, by: \.trainID)
        let nexTrains = dataset.spikeTrains.map { train in
            ManualISINEXTrain(
                trainID: train.semanticID.semanticID.canonicalText,
                trainName: train.semanticID.semanticID.canonicalText,
                spikeTimestampsMicroseconds: train.rawTimestamps.map(\.microseconds),
                rows: (rowsByTrain[train.semanticID] ?? []).map { row in
                    ManualISINEXRow(
                        isiIndex: row.isiIndex,
                        leftTimestampMicroseconds: row.leftTick.microseconds,
                        rightTimestampMicroseconds: row.rightTick.microseconds,
                        statePattern: row.stateLabel?.rawValue ?? "",
                        eventPattern: row.eventLabel?.rawValue ?? "",
                        otherPattern: row.otherLabel?.rawValue ?? "",
                        authorityStatus: CanonicalManualISIDraftCSVExporter.authorityStatus
                    )
                }
            )
        }
        return ManualISIResultExportContent(table: table, nexTrains: nexTrains)
    }

    func exportCanonicalManualISILabelDraftCSVWithPanel() {
        guard let persisted = scientificImportCoordinator.persistedConfirmation,
              let draft = canonicalManualISILabelDraft,
              canExportCanonicalManualISILabelDraft else {
            statusMessage = "当前没有可导出的人工 ISI 草稿。"
            return
        }
        let segment = persisted.baseConfirmation.recordingSegment.semanticID
            .semanticID.canonicalText
        do {
            let content = try canonicalManualISIExportContent(
                persisted: persisted,
                draft: draft
            )
            guard let url = try ManualISIResultExportPanel.save(
                content: content,
                suggestedStem: "\(safeFileStem(segment))_manual_isi_labels_draft",
                message: "选择 CSV、XLSX 或 NEX。CSV 会生成一个 ZIP，其中每条 spike train 对应一个独立 CSV；XLSX 中每条 spike train 对应一个工作表；NEX 保存 spike、逐 ISI marker 与合并后的模式区间。该草稿未封存。"
            ) else { return }
            statusMessage = "已导出人工 ISI 草稿：\(url.lastPathComponent)"
            lastErrorMessage = nil
        } catch {
            statusMessage = "人工 ISI 草稿导出失败。"
            lastErrorMessage = error.localizedDescription
        }
    }

    /// New format-neutral name. Keep the old entry point above for existing commands/tests.
    func exportCanonicalManualISILabelDraftWithPanel() {
        exportCanonicalManualISILabelDraftCSVWithPanel()
    }
    func exportCanonicalManualResultPackageWithPanel() {
        guard let persisted = scientificImportCoordinator.persistedConfirmation,
              let confirmed = confirmedCanonicalManualLabels,
              !isResultPackageExporting else {
            statusMessage = "Confirm the complete canonical manual review before export."
            return
        }

        let panel = NSSavePanel()
        let packageType = UTType(
            filenameExtension: "stpdresult",
            conformingTo: .package
        ) ?? .package
        panel.allowedContentTypes = [packageType]
        panel.canCreateDirectories = true
        panel.isExtensionHidden = false
        let segment = persisted.baseConfirmation.recordingSegment.semanticID
            .semanticID.canonicalText
        panel.nameFieldStringValue =
            "\(safeFileStem(segment))_manual_\(confirmed.decisionDigest.prefix(12)).stpdresult"
        panel.message = "Export a detector-independent canonical manual result package. The package contains exact integer-microsecond ISIs and separate State, Event, and Other columns. Existing packages are never overwritten."

        guard panel.runModal() == .OK, let url = panel.url else { return }
        guard scientificImportCoordinator.persistedConfirmation?
                .confirmationRecordDigest == persisted.confirmationRecordDigest,
              confirmedCanonicalManualLabels?.decisionDigest
                == confirmed.decisionDigest else {
            statusMessage = "Manual result-package export blocked."
            lastErrorMessage = "The confirmed import or manual review changed while the save panel was open."
            return
        }

        isResultPackageExporting = true
        defer { isResultPackageExporting = false }
        do {
            let package = try CanonicalManualResultPackageBuilder.build(
                persistedImport: persisted,
                confirmedLabels: confirmed
            )
            try STPDResultPackageWriter.writeCanonicalManualResult(
                package,
                to: url
            )
            statusMessage = "Exported canonical manual result package to \(url.lastPathComponent)."
            lastErrorMessage = nil
        } catch {
            statusMessage = "Manual result-package export failed."
            lastErrorMessage = error.localizedDescription
        }
    }

    private func safeFileStem(_ value: String) -> String {
        let sanitized = value
            .replacingOccurrences(of: "/", with: "_")
            .replacingOccurrences(of: ":", with: "_")
        return sanitized.isEmpty ? "spike_train" : sanitized
    }

    var canExportManualISILabelDraft: Bool {
        guard let dataset else { return false }
        return dataset.trains.contains { $0.timestampsSec.count >= 2 }
    }

    /// Audit-only statistics derived from the current compatible manual ranges. The summary does
    /// not affect detection until the reviewer explicitly applies its learned proposal.
    var manualCalibrationSummary: ManualAnnotationCalibrationSummary? {
        guard let dataset else { return nil }
        return ManualAnnotationCalibrationSummarizer.summarize(
            trains: dataset.trains,
            annotations: rasterManualAnnotations,
            minValidISISeconds: qualitySettings.artifactThresholdSec
        )
    }

    var learnedThresholdProposal: LearnedThresholdProposal? {
        guard let summary = manualCalibrationSummary else { return nil }
        return LearnedManualThresholdBuilder.build(from: summary)
    }

    private var currentManualThresholdFieldState: ManualThresholdFieldState {
        ManualThresholdFieldState(
            burstMode: manualBurstMode,
            burstSeedMaxISIMs: manualBurstSeedMaxISIMs,
            burstBridgeMaxISIMs: manualBurstBridgeMaxISIMs,
            tonicMode: manualTonicMode,
            tonicMinISIMs: manualTonicMinISIMs,
            tonicMaxISIMs: manualTonicMaxISIMs,
            hfTonicMode: manualHFTonicMode,
            hfTonicMinISIMs: manualHFTonicMinISIMs,
            hfTonicMaxISIMs: manualHFTonicMaxISIMs,
            pauseMode: manualPauseMode,
            pauseMinISIMs: manualPauseMinISIMs
        )
    }

    var learnedThresholdExplanations: [LearnedThresholdFieldExplanation] {
        guard let proposal = learnedThresholdProposal,
              !proposal.isAllAutomatic else { return [] }
        return LearnedManualThresholdApplier.explain(
            proposal: proposal,
            current: currentManualThresholdFieldState
        )
    }

    private var currentTonicGateThresholds: TonicGateThresholds {
        let minimumSpikes = manualTonicMode != .automatic && manualTonicMinSpikes > 0
            ? manualTonicMinSpikes
            : detectorTonicMinSpikes
        return TonicGateThresholds(
            cvMax: detectorTonicCVMax,
            cv2Max: detectorTonicCV2Max,
            lvMax: detectorTonicLVMax,
            minSpikes: minimumSpikes
        )
    }

    var focusedSegmentSensitivityPreview: SegmentSensitivityPreview? {
        guard let candidate = focusedClassicAnchorCandidate,
              candidate.startISIIndex <= candidate.endISIIndex,
              let train = dataset?.trains.first(where: { $0.id == candidate.trainID }) else {
            return nil
        }
        return SegmentSensitivityPreviewer.preview(
            isiSec: train.isiSec,
            segment: candidate.startISIIndex...candidate.endISIIndex,
            gates: currentTonicGateThresholds
        )
    }

    var pinnedTonicSegmentSensitivity: PinnedTonicSensitivity? {
        guard let pinned = pinnedISIDiagnostic,
              let candidates = classicAnchorDetectionRun?.result(for: pinned.trainID)?.candidates,
              let candidate = SegmentSensitivityPreviewer.coveringSelectedTonicCandidate(
                isiIndex: pinned.isiIndex,
                candidates: candidates
              ),
              candidate.startISIIndex <= candidate.endISIIndex,
              let train = dataset?.trains.first(where: { $0.id == candidate.trainID }),
              let preview = SegmentSensitivityPreviewer.preview(
                isiSec: train.isiSec,
                segment: candidate.startISIIndex...candidate.endISIIndex,
                gates: currentTonicGateThresholds
              ) else { return nil }
        return PinnedTonicSensitivity(
            preview: preview,
            candidateID: candidate.id,
            trainName: candidate.trainName,
            startISIIndex: candidate.startISIIndex,
            endISIIndex: candidate.endISIIndex
        )
    }

    func applyLearnedThresholds() {
        guard let proposal = learnedThresholdProposal,
              !proposal.isAllAutomatic else {
            lastLearnedApplyResult = nil
            return
        }
        let result = LearnedManualThresholdApplier.apply(
            proposal: proposal,
            to: currentManualThresholdFieldState
        )
        let state = result.state
        manualBurstMode = state.burstMode
        manualBurstSeedMaxISIMs = state.burstSeedMaxISIMs
        manualBurstBridgeMaxISIMs = state.burstBridgeMaxISIMs
        manualTonicMode = state.tonicMode
        manualTonicMinISIMs = state.tonicMinISIMs
        manualTonicMaxISIMs = state.tonicMaxISIMs
        manualHFTonicMode = state.hfTonicMode
        manualHFTonicMinISIMs = state.hfTonicMinISIMs
        manualHFTonicMaxISIMs = state.hfTonicMaxISIMs
        manualPauseMode = state.pauseMode
        manualPauseMinISIMs = state.pauseMinISIMs
        lastLearnedApplyResult = result
        statusMessage = result.didApplyAnything
            ? "已将人工标注学习值作为 Soft 辅助阈值写入；请重新运行检测。"
            : "没有可应用的人工学习阈值。"
    }

    func resetManualThresholdFields() {
        let defaults = ManualThresholdFields.automaticDefaults
        manualBurstMode = defaults.burstMode
        manualBurstSeedMaxISIMs = defaults.burstSeedMaxISIMs
        manualBurstBridgeMaxISIMs = defaults.burstBridgeMaxISIMs
        manualBurstMinSpikes = defaults.burstMinSpikes
        manualHFSMode = defaults.hfsMode
        manualHFSMinSpikes = defaults.hfsMinSpikes
        manualHFSMinDurationMs = defaults.hfsMinDurationMs
        manualHFTonicMode = defaults.hfTonicMode
        manualHFTonicMinISIMs = defaults.hfTonicMinISIMs
        manualHFTonicMaxISIMs = defaults.hfTonicMaxISIMs
        manualHFTonicMinSpikes = defaults.hfTonicMinSpikes
        manualTonicMode = defaults.tonicMode
        manualTonicMinISIMs = defaults.tonicMinISIMs
        manualTonicMaxISIMs = defaults.tonicMaxISIMs
        manualTonicMinSpikes = defaults.tonicMinSpikes
        manualPauseMode = defaults.pauseMode
        manualPauseMinISIMs = defaults.pauseMinISIMs
        manualThresholdScopeKind = .allTrains
        lastLearnedApplyResult = nil
    }

    var canCreateManualAnnotationFromFocusedCandidate: Bool {
        guard let candidate = focusedClassicAnchorCandidate else { return false }
        return ManualAnnotationLabel(autoLabel: candidate.finalLabel) != nil
    }

    func createManualAnnotationFromFocusedCandidate() {
        guard let candidate = focusedClassicAnchorCandidate,
              let label = ManualAnnotationLabel(autoLabel: candidate.finalLabel) else {
            statusMessage = "请先选择可映射为人工模式的结构候选。"
            return
        }
        let indices = Set(candidate.startISIIndex...candidate.endISIIndex)
        applyRasterManualISILabel(label, trainID: candidate.trainID, isiIndices: indices)
    }

    /// All locally authored manual ranges in deterministic display/export order.
    var manualAnnotationsAll: [ManualAnnotation] {
        manualAnnotationsByTrain
            .sorted { $0.key < $1.key }
            .flatMap { $0.value.sorted { $0.createdAt < $1.createdAt } }
    }

    /// The raster always renders the current manual source of truth. Canonical decisions remain
    /// exact integer-microsecond values and are converted to seconds only for this display layer.
    var rasterManualAnnotations: [ManualAnnotation] {
        guard let canonicalManualDataset,
              let persisted = scientificImportCoordinator.persistedConfirmation,
              let draft = canonicalManualISILabelDraft,
              draft.canonicalFingerprint == persisted.canonicalFingerprint,
              let rows = try? CanonicalManualISILabelProjector.rows(
                dataset: canonicalManualDataset,
                fingerprint: persisted.canonicalFingerprint,
                draft: draft
              ) else {
            return manualAnnotationsAll
        }

        return rows.flatMap { row -> [ManualAnnotation] in
            let labels = [row.stateLabel, row.eventLabel, row.otherLabel].compactMap { $0 }
            return labels.map { label in
                ManualAnnotation(
                    trainID: row.trainID.semanticID.canonicalText,
                    label: label,
                    startSec: Double(row.leftTick.microseconds) / 1_000_000,
                    endSec: Double(row.rightTick.microseconds) / 1_000_000,
                    startISIIndex: row.isiIndex,
                    endISIIndex: row.isiIndex,
                    startSpikeIndex: row.isiIndex - 1,
                    endSpikeIndex: row.isiIndex,
                    note: "canonical-display-projection"
                )
            }
        }
    }

    func manualISIRows(for trainID: String) -> [ManualISIDualTrackExportRow] {
        guard let train = dataset?.trains.first(where: { $0.id == trainID }) else {
            return []
        }
        return ManualISIDualTrackProjector.project(
            train: train,
            annotations: manualAnnotationsByTrain[trainID] ?? []
        ).rows
    }

    @discardableResult
    func applyManualISILabel(
        _ label: ManualAnnotationLabel,
        trainID: String,
        isiIndices: Set<Int>
    ) -> Bool {
        guard label.polarity == .positive,
              let train = dataset?.trains.first(where: { $0.id == trainID }),
              !isiIndices.isEmpty,
              validateManualAuthoringSelection(
                  label: label,
                  isiIndices: isiIndices,
                  maximumISIIndex: train.isiSec.count
              ) else {
            return false
        }
        let current = manualAnnotationsByTrain[trainID] ?? []
        let updated = ManualISILabelDraftEditor.applying(
            label: label,
            toISIIndices: isiIndices,
            train: train,
            existingAnnotations: current,
            annotator: localManualAnnotator
        )
        guard updated != current else { return false }
        recordManualISIUndo(.local(manualAnnotationsByTrain))
        manualAnnotationsByTrain[trainID] = updated
        saveManualAnnotations()
        statusMessage = "已在 \(train.name) 的 \(isiIndices.count) 个 ISI 上标记 \(label.displayNameZH)。"
        lastErrorMessage = nil
        return true
    }

    func clearManualISILabel(
        track: ManualAnnotationSemanticTrack,
        trainID: String,
        isiIndices: Set<Int>
    ) {
        guard let train = dataset?.trains.first(where: { $0.id == trainID }),
              !isiIndices.isEmpty else {
            return
        }
        let current = manualAnnotationsByTrain[trainID] ?? []
        let updated = ManualISILabelDraftEditor.clearing(
            track: track,
            atISIIndices: isiIndices,
            train: train,
            existingAnnotations: current,
            annotator: localManualAnnotator
        )
        guard updated != current else { return }
        recordManualISIUndo(.local(manualAnnotationsByTrain))
        if updated.isEmpty {
            manualAnnotationsByTrain.removeValue(forKey: trainID)
        } else {
            manualAnnotationsByTrain[trainID] = updated
        }
        saveManualAnnotations()
        statusMessage = "已清除 \(train.name) 的 \(isiIndices.count) 个 ISI 上的\(track.displayNameZH)标记。"
        lastErrorMessage = nil
    }

    /// Shared authoring entry point for the main raster. Canonical imports update only their exact
    /// identity-bound draft; legacy/exploration datasets update the dataset-scoped local sidecar.
    func applyRasterManualISILabel(
        _ label: ManualAnnotationLabel,
        trainID: String,
        isiIndices: Set<Int>
    ) {
        if canonicalManualDataset != nil,
           canonicalManualISILabelDraft != nil,
           canonicalManualDataset?.spikeTrains.contains(where: {
               $0.semanticID.semanticID.canonicalText == trainID
           }) == true {
            applyCanonicalManualISILabel(label, trainID: trainID, isiIndices: isiIndices)
        } else {
            applyManualISILabel(label, trainID: trainID, isiIndices: isiIndices)
        }
    }

    /// Shared eraser entry point for the main raster. It removes only the requested semantic track,
    /// preserving a coexisting State/Event decision on the same ISI.
    func clearRasterManualISILabel(
        track: ManualAnnotationSemanticTrack,
        trainID: String,
        isiIndices: Set<Int>
    ) {
        if canonicalManualDataset != nil,
           canonicalManualISILabelDraft != nil,
           canonicalManualDataset?.spikeTrains.contains(where: {
               $0.semanticID.semanticID.canonicalText == trainID
           }) == true {
            clearCanonicalManualISILabel(
                track: track,
                trainID: trainID,
                isiIndices: isiIndices
            )
        } else {
            clearManualISILabel(track: track, trainID: trainID, isiIndices: isiIndices)
        }
    }

    var canUndoManualISIEdit: Bool { !manualISIUndoStack.isEmpty }

    func undoLastManualISIEdit() {
        guard let snapshot = manualISIUndoStack.popLast() else {
            statusMessage = "当前没有可撤销的手工标记。"
            return
        }
        switch snapshot {
        case .canonical(let draft):
            guard canonicalManualISILabelDraft?.canonicalFingerprint
                    == draft.canonicalFingerprint else {
                manualISIUndoStack.removeAll()
                statusMessage = "数据集身份已经改变，旧的撤销记录已清除。"
                return
            }
            canonicalManualISILabelDraft = draft
            confirmedCanonicalManualLabels = nil
        case .local(let annotations):
            manualAnnotationsByTrain = annotations
            saveManualAnnotations()
        }
        statusMessage = "已撤销上一步手工标记。"
        lastErrorMessage = nil
    }

    private func recordManualISIUndo(_ snapshot: ManualISIUndoSnapshot) {
        manualISIUndoStack.append(snapshot)
        let excess = manualISIUndoStack.count - 50
        if excess > 0 { manualISIUndoStack.removeFirst(excess) }
    }

    /// Each authored pattern must be supported by one contiguous run, not an aggregate of distant
    /// ISIs. An ISI run of length `n` spans `n + 1` source spikes.
    private func validateManualAuthoringSelection(
        label: ManualAnnotationLabel,
        isiIndices: Set<Int>,
        maximumISIIndex: Int
    ) -> Bool {
        guard label.polarity == .positive else { return false }
        let indices = isiIndices.sorted()
        guard !indices.isEmpty,
              indices.allSatisfy({ 1...maximumISIIndex ~= $0 }) else {
            let message = "所选 ISI 不属于当前 spike train，未创建手工标记。"
            statusMessage = message
            lastErrorMessage = message
            return false
        }

        let minimumSpikes = label.minimumManualAuthoringSpikeCount
        guard minimumSpikes > 1 else { return true }
        var runLength = 1
        var previous = indices[0]
        for index in indices.dropFirst() {
            if index == previous + 1 {
                runLength += 1
            } else {
                if runLength + 1 < minimumSpikes {
                    return reportInsufficientManualAuthoringSupport(
                        label: label,
                        minimumSpikes: minimumSpikes
                    )
                }
                runLength = 1
            }
            previous = index
        }
        guard runLength + 1 >= minimumSpikes else {
            return reportInsufficientManualAuthoringSupport(
                label: label,
                minimumSpikes: minimumSpikes
            )
        }
        return true
    }

    private func reportInsufficientManualAuthoringSupport(
        label: ManualAnnotationLabel,
        minimumSpikes: Int
    ) -> Bool {
        let message = "\(label.displayNameZH) 的每段连续标记至少需要 \(minimumSpikes) 个 spike，未创建手工标记。"
        statusMessage = message
        lastErrorMessage = message
        return false
    }

    @discardableResult
    func addManualAnnotation(_ annotation: ManualAnnotation) -> Bool {
        guard let dataset,
              let resolved = ManualAnnotationGeometryResolver
                .resolvingIndicesIfCompatible(annotation, in: dataset.trains) else {
            return false
        }
        var annotations = manualAnnotationsByTrain[resolved.trainID] ?? []
        if let index = annotations.firstIndex(where: { $0.id == resolved.id }) {
            annotations[index] = resolved
        } else {
            annotations.append(resolved)
        }
        manualAnnotationsByTrain[resolved.trainID] = annotations
        saveManualAnnotations()
        return true
    }

    @discardableResult
    func updateManualAnnotation(_ annotation: ManualAnnotation) -> Bool {
        addManualAnnotation(annotation)
    }

    func removeManualAnnotation(id: UUID, trainID: String) {
        guard var annotations = manualAnnotationsByTrain[trainID] else { return }
        annotations.removeAll { $0.id == id }
        if annotations.isEmpty {
            manualAnnotationsByTrain.removeValue(forKey: trainID)
        } else {
            manualAnnotationsByTrain[trainID] = annotations
        }
        if selectedManualAnnotationID == id { selectedManualAnnotationID = nil }
        saveManualAnnotations()
    }

    func loadManualAnnotations() {
        manualISIUndoStack.removeAll()
        guard let dataset else {
            manualAnnotationsByTrain = [:]
            return
        }
        do {
            let stored = try manualAnnotationPersistence.load(
                datasetKey: ManualAnnotationPersistence.datasetKey(for: dataset)
            )
            var restored: [String: [ManualAnnotation]] = [:]
            for annotations in stored.values {
                for annotation in annotations {
                    if let resolved = ManualAnnotationGeometryResolver
                        .resolvingIndicesIfCompatible(annotation, in: dataset.trains) {
                        restored[resolved.trainID, default: []].append(resolved)
                    }
                }
            }
            manualAnnotationsByTrain = restored
        } catch {
            manualAnnotationsByTrain = [:]
            lastErrorMessage = "手工标注恢复失败：\(error.localizedDescription)"
        }
    }

    func saveManualAnnotations() {
        guard let dataset else { return }
        do {
            try manualAnnotationPersistence.save(
                manualAnnotationsByTrain,
                datasetKey: ManualAnnotationPersistence.datasetKey(for: dataset),
                dataset: dataset
            )
        } catch {
            lastErrorMessage = "手工标注自动保存失败：\(error.localizedDescription)"
        }
    }

    func manualISILabelDraftCSV() -> String? {
        guard let dataset else { return nil }
        let projection = ManualISIDualTrackProjector.project(
            dataset: dataset,
            annotationsByTrain: manualAnnotationsByTrain
        )
        return ManualISIDualTrackCSVExporter.csv(rows: projection.rows)
    }

    private func manualISIExportContent(dataset: SpikeDataset) throws
        -> ManualISIResultExportContent {
        let projection = ManualISIDualTrackProjector.project(
            dataset: dataset,
            annotationsByTrain: manualAnnotationsByTrain
        )
        let rowsByTrain = Dictionary(grouping: projection.rows, by: \.trainID)
        let nexTrains = try dataset.trains.map { train in
            let ticks = try train.timestampsSec.map {
                try NeuroExplorerNEXCodec.wholeMicroseconds(
                    seconds: $0,
                    trainID: train.id
                )
            }
            return ManualISINEXTrain(
                trainID: train.id,
                trainName: train.name,
                spikeTimestampsMicroseconds: ticks,
                rows: try (rowsByTrain[train.id] ?? []).map { row in
                    ManualISINEXRow(
                        isiIndex: row.isiIndex,
                        leftTimestampMicroseconds: try NeuroExplorerNEXCodec
                            .wholeMicroseconds(seconds: row.leftTimestampSec, trainID: train.id),
                        rightTimestampMicroseconds: try NeuroExplorerNEXCodec
                            .wholeMicroseconds(seconds: row.rightTimestampSec, trainID: train.id),
                        statePattern: row.statePattern,
                        eventPattern: row.eventPattern,
                        otherPattern: row.otherPattern,
                        stateAnnotationID: row.stateAnnotationID,
                        eventAnnotationID: row.eventAnnotationID,
                        otherAnnotationID: row.otherAnnotationID,
                        burstVeto: row.burstVeto,
                        reviewNote: row.reviewNote,
                        authorityStatus: row.authorityStatus
                    )
                }
            )
        }
        return ManualISIResultExportContent(
            table: ManualISIDualTrackCSVExporter.table(rows: projection.rows),
            nexTrains: nexTrains
        )
    }

    func exportManualISILabelDraftCSVWithPanel() {
        guard let dataset, canExportManualISILabelDraft else {
            statusMessage = "No ISIs are available for manual-label export."
            lastErrorMessage = nil
            return
        }

        do {
            let content = try manualISIExportContent(dataset: dataset)
            let stem = defaultManualISILabelFileName(datasetName: dataset.name)
                .replacingOccurrences(of: ".csv", with: "")
            guard let url = try ManualISIResultExportPanel.save(
                content: content,
                suggestedStem: stem,
                message: "选择 CSV、XLSX 或 NEX。CSV 会生成一个 ZIP，其中每条 spike train 对应一个独立 CSV；XLSX 中每条 spike train 对应一个工作表；NEX 保存 spike、逐 ISI marker 与合并后的模式区间。该文件是本地未封存人工草稿。"
            ) else { return }
            statusMessage = "已导出人工 ISI 草稿：\(url.lastPathComponent)"
            lastErrorMessage = nil
        } catch {
            statusMessage = "Manual ISI export failed."
            lastErrorMessage = error.localizedDescription
        }
    }

    func exportManualISILabelDraftWithPanel() {
        exportManualISILabelDraftCSVWithPanel()
    }

    private var localManualAnnotator: String? {
        for value in [NSFullUserName(), NSUserName()] {
            let trimmed = value.trimmingCharacters(in: .whitespacesAndNewlines)
            if !trimmed.isEmpty { return trimmed }
        }
        return nil
    }

    private func defaultManualISILabelFileName(datasetName: String) -> String {
        let allowed = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "-_") )
        let stem = datasetName.unicodeScalars.map { allowed.contains($0) ? Character(String($0)) : "_" }
        let sanitized = String(stem).trimmingCharacters(in: CharacterSet(charactersIn: "_"))
        return "\(sanitized.isEmpty ? "dataset" : sanitized)_manual_isi_labels.csv"
    }
}
