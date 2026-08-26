import STPDCore
import SwiftUI

struct ManualISIWorkbenchView: View {
    @Bindable var document: RasterDocument

    var body: some View {
        if document.hasPersistedCanonicalManualSource {
            CanonicalManualISIWorkbenchView(document: document)
        } else {
            LegacyManualISIWorkbenchView(document: document)
        }
    }
}

private struct CanonicalManualISIWorkbenchView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n

    @State private var selectedTrainID: String?
    @State private var selectedRowIDs: Set<String> = []
    @State private var focusedRowID: String?
    @State private var tableCenterRequestID: UInt64 = 0
    @State private var selectedTrack: ManualAnnotationSemanticTrack = .state
    @State private var selectedLabel: ManualAnnotationLabel = .tonic
    @AppStorage("stpd_raster_shows_isi_information_panel")
    private var showsISIInformationPanel = true

    private var trains: [CanonicalSpikeTrain] {
        document.canonicalManualDataset?.spikeTrains ?? []
    }

    private var rows: [CanonicalManualISILabelRow] {
        guard let selectedTrainID else { return [] }
        return document.canonicalManualISIRows(for: selectedTrainID)
    }

    private var selectedISIIndices: Set<Int> {
        Set(rows.lazy.filter { selectedRowIDs.contains($0.id) }.map(\.isiIndex))
    }

    private var unreviewedCount: Int { document.canonicalManualUnreviewedISICount }
    private var minimumValidISISeconds: Double {
        max(0, document.artifactThresholdMs) / 1_000
    }
    private var graphicRows: [ManualISIGraphicRow] {
        rows.map {
            let isiSeconds = Double($0.intervalMicroseconds) / 1_000_000
            return ManualISIGraphicRow(
                id: $0.id,
                isiIndex: $0.isiIndex,
                leftSeconds: Double($0.leftTick.microseconds) / 1_000_000,
                rightSeconds: Double($0.rightTick.microseconds) / 1_000_000,
                stateLabel: $0.stateLabel,
                eventLabel: $0.eventLabel,
                otherLabel: $0.otherLabel,
                isInvalidISI: ManualISIQualityRule.isAbsolutelyInvalid(
                    isiSeconds: isiSeconds,
                    thresholdSeconds: minimumValidISISeconds
                )
            )
        }
    }
    private var persistedRecordDigest: String? {
        document.scientificImportCoordinator.persistedConfirmation?
            .confirmationRecordDigest
    }
    private var totalISICount: Int {
        trains.reduce(0) { $0 + max(0, $1.rawTimestamps.count - 1) }
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            header
            contractNotice
            if trains.isEmpty {
                ContentUnavailableView(
                    l10n.t("规范人工审核不可用"),
                    systemImage: "exclamationmark.triangle",
                    description: Text(document.lastErrorMessage.map(l10n.t)
                        ?? l10n.t("已持久化的导入尚不具备规范人工模式审核资格。"))
                )
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else {
                controls
                ManualISIQCNotice(
                    rows: graphicRows,
                    thresholdMilliseconds: $document.artifactThresholdMs
                )
                ManualISIThresholdAssistant(
                    rows: graphicRows,
                    minimumValidISISeconds: minimumValidISISeconds,
                    selectedRowIDs: $selectedRowIDs,
                    canUndo: document.canUndoManualISIEdit,
                    onApply: { label, isiIndices in
                        guard let selectedTrainID else { return false }
                        return document.applyCanonicalManualISILabel(
                            label,
                            trainID: selectedTrainID,
                            isiIndices: isiIndices
                        )
                    },
                    onUndo: document.undoLastManualISIEdit
                )
                ManualISIGraphicSelector(
                    rows: graphicRows,
                    selectedRowIDs: $selectedRowIDs,
                    showsISIInformationPanel: $showsISIInformationPanel,
                    onFocusedRowIDChange: {
                        focusedRowID = $0
                        tableCenterRequestID &+= 1
                    },
                    onDeletePatternRun: { patternRun in
                        deletePatternRun(patternRun)
                    }
                )
                selectionSummary
                table
            }
        }
        .padding(16)
        .onAppear {
            _ = document.prepareCanonicalManualWorkbench()
            ensureTrainSelection()
        }
        .onChange(of: trains.map { $0.semanticID.semanticID.canonicalText }) {
            _, _ in ensureTrainSelection()
        }
        .onChange(of: persistedRecordDigest) { _, _ in
            _ = document.prepareCanonicalManualWorkbench()
            ensureTrainSelection()
        }
        .onChange(of: selectedTrainID) { _, _ in
            selectedRowIDs.removeAll()
            focusedRowID = nil
        }
        .onChange(of: selectedTrack) { _, track in
            selectedLabel = ManualAnnotationLabel.positiveLabels(for: track).first ?? .other
        }
    }

    private var header: some View {
        HStack(spacing: 10) {
            Text(l10n.t("纯手工 ISI 标记（无需模式检测）"))
                .font(.title3.weight(.semibold))
            Text(l10n.t(document.confirmedCanonicalManualLabels == nil
                ? "身份绑定草稿" : "审核已确认"))
                .font(.caption.weight(.semibold))
                .foregroundStyle(document.confirmedCanonicalManualLabels == nil
                    ? Color.orange : Color.green)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background((document.confirmedCanonicalManualLabels == nil
                    ? Color.orange : Color.green).opacity(0.12), in: Capsule())
            Spacer()
            if let confirmation = document.confirmedCanonicalManualLabels {
                Text(String(confirmation.decisionDigest.prefix(12)))
                    .font(.caption.monospaced())
                    .foregroundStyle(.secondary)
            }
            Button {
                document.importCanonicalManualISILabelDraftWithPanel()
            } label: {
                Label(l10n.t("导入手工 ISI 草稿"), systemImage: "square.and.arrow.down")
            }
            .disabled(!document.canImportCanonicalManualISILabelDraft)
            Button {
                document.exportCanonicalManualISILabelDraftWithPanel()
            } label: {
                Label(l10n.t("导出 CSV / XLSX / NEX"), systemImage: "square.and.arrow.up")
            }
            .disabled(!document.canExportCanonicalManualISILabelDraft)
            Button {
                document.confirmCompleteCanonicalManualReview()
            } label: {
                Label(l10n.t("确认完整审核"), systemImage: "checkmark.seal.fill")
            }
            .disabled(totalISICount == 0 || unreviewedCount > 0)
            Button {
                document.exportCanonicalManualResultPackageWithPanel()
            } label: {
                Label(l10n.t("导出 .stpdresult"), systemImage: "shippingbox")
            }
            .disabled(!document.canExportCanonicalManualResultPackage)
        }
    }

    private var contractNotice: some View {
        HStack(alignment: .top, spacing: 9) {
            Image(systemName: "info.circle")
                .foregroundStyle(.secondary)
            Text(l10n.t("时间图和表格都直接来自已持久化的规范整数微秒数据集，不读取任何自动检测候选。可在时间图拖动选择连续 ISI，也可在表格精确多选；任何阶段均可选择导出 CSV、XLSX 或 NeuroExplorer NEX，未标记项会保留为空。只有确认完整审核并导出 .stpdresult 时，才要求每个 ISI 都归入状态、事件或其他。状态与事件保持独立，因此 HFS 可与内嵌 HFB 共存。"))
                .font(.callout)
                .foregroundStyle(.secondary)
        }
        .padding(10)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.quaternary.opacity(0.45), in: RoundedRectangle(cornerRadius: 8))
    }

    private var controls: some View {
        HStack(spacing: 12) {
            Picker(l10n.t("Spike train"), selection: $selectedTrainID) {
                ForEach(trains, id: \.semanticID) { train in
                    let id = train.semanticID.semanticID.canonicalText
                    Text(id).tag(Optional(id))
                }
            }
            .frame(minWidth: 180, idealWidth: 240)

            Picker(l10n.t("轨道"), selection: $selectedTrack) {
                ForEach(ManualAnnotationSemanticTrack.allCases, id: \.self) {
                    Text($0.displayName(using: l10n)).tag($0)
                }
            }
            .pickerStyle(.segmented)
            .frame(width: 240)

            Picker(l10n.t("模式"), selection: $selectedLabel) {
                ForEach(ManualAnnotationLabel.positiveLabels(for: selectedTrack), id: \.self) {
                    Text($0.displayName(using: l10n)).tag($0)
                }
            }
            .frame(minWidth: 150)

            Button(l10n.t("应用")) { apply() }
                .buttonStyle(.borderedProminent)
                .disabled(selectedISIIndices.isEmpty)
            Button(selectedTrack.clearTitle(using: l10n)) { clear() }
                .disabled(selectedISIIndices.isEmpty)
            Button {
                document.undoLastManualISIEdit()
            } label: {
                Label(l10n.t("撤销上一步"), systemImage: "arrow.uturn.backward")
            }
            .disabled(!document.canUndoManualISIEdit)
            .keyboardShortcut("z", modifiers: .command)
        }
    }

    private var selectionSummary: some View {
        HStack(spacing: 10) {
            Text(selectionCountText)
                .font(.callout.monospacedDigit())
                .foregroundStyle(.secondary)
            Button(l10n.t("全选")) { selectedRowIDs = Set(rows.map(\.id)) }
                .disabled(rows.isEmpty || selectedRowIDs.count == rows.count)
            Button(l10n.t("清除选择")) { selectedRowIDs.removeAll() }
                .disabled(selectedRowIDs.isEmpty)
            Spacer()
            Text(unreviewedCountText)
                .font(.caption.monospacedDigit())
                .foregroundStyle(unreviewedCount == 0 ? .green : .orange)
        }
    }

    private var table: some View {
        let tableRows = rows
        let ratiosByRowID = ManualISINeighborhoodRatios.indexedMicroseconds(
            tableRows.map { (id: $0.id, interval: $0.intervalMicroseconds) }
        )

        return Table(tableRows, selection: $selectedRowIDs) {
            TableColumn(l10n.t("左时间戳（s）")) { Text(seconds($0.leftTick)).monospacedDigit() }
                .width(min: 90, ideal: 110)
            TableColumn(l10n.t("左 MM")) { row in
                mmText(ratiosByRowID[row.id]?.leftMM)
                    .help(l10n.t("左 MM = 左邻 ISI ÷ 当前 ISI"))
            }
            .width(min: 64, ideal: 76, max: 92)
            TableColumn(l10n.t("当前 ISI（ms）")) {
                Text(String(format: "%.3f", Double($0.intervalMicroseconds) / 1_000))
                    .monospacedDigit()
                    .fontWeight(selectedRowIDs.contains($0.id) ? .bold : .regular)
                    .foregroundStyle(
                        ManualISIQualityRule.isAbsolutelyInvalid(
                            isiSeconds: Double($0.intervalMicroseconds) / 1_000_000,
                            thresholdSeconds: minimumValidISISeconds
                        ) ? Color.red : Color.primary
                    )
            }
            .width(min: 92, ideal: 108)
            TableColumn(l10n.t("右 MM")) { row in
                mmText(ratiosByRowID[row.id]?.rightMM)
                    .help(l10n.t("右 MM = 右邻 ISI ÷ 当前 ISI"))
            }
            .width(min: 64, ideal: 76, max: 92)
            TableColumn(l10n.t("右时间戳（s）")) { Text(seconds($0.rightTick)).monospacedDigit() }
                .width(min: 90, ideal: 110)
            TableColumn(ManualAnnotationSemanticTrack.event.displayName(using: l10n)) { pattern($0.eventLabel) }
                .width(min: 90, ideal: 130)
            TableColumn(ManualAnnotationSemanticTrack.state.displayName(using: l10n)) { pattern($0.stateLabel) }
                .width(min: 90, ideal: 130)
            TableColumn(l10n.t("备注")) { _ in
                Text("—").foregroundStyle(.tertiary)
            }
            .width(min: 90, ideal: 130)
            TableColumn(ManualAnnotationSemanticTrack.other.displayName(using: l10n)) { pattern($0.otherLabel) }
                .width(min: 70, ideal: 90)
            TableColumn(l10n.t("审核 / ISI #")) {
                Text("\(l10n.t($0.isReviewed ? "已审核" : "未审核")) · #\($0.isiIndex)")
                    .foregroundStyle($0.isReviewed ? Color.secondary : Color.orange)
                    .monospacedDigit()
            }
            .width(min: 110, ideal: 130)
        }
        .background {
            ManualISITableCenteringBridge(
                focusedRowID: focusedRowID,
                requestID: tableCenterRequestID,
                rowIDs: rows.map(\.id)
            )
            .allowsHitTesting(false)
        }
        .frame(minHeight: 320)
    }

    @ViewBuilder
    private func pattern(_ label: ManualAnnotationLabel?) -> some View {
        if let label { Text(label.displayName(using: l10n)) }
        else { Text("—").foregroundStyle(.tertiary) }
    }

    private var selectionCountText: String {
        switch l10n.language {
        case .zh:
            "共 \(rows.count) 个 ISI · 已选择 \(selectedISIIndices.count) 个"
        case .en:
            "\(rows.count) ISIs · \(selectedISIIndices.count) selected"
        case .ru:
            "Всего ISI: \(rows.count) · выбрано: \(selectedISIIndices.count)"
        }
    }

    private var unreviewedCountText: String {
        switch l10n.language {
        case .zh: "数据集中尚有 \(unreviewedCount) 个未审核 ISI"
        case .en: "\(unreviewedCount) ISIs remain unreviewed in the dataset"
        case .ru: "В наборе данных осталось непроверенных ISI: \(unreviewedCount)"
        }
    }

    private func seconds(_ tick: MicrosecondTick) -> String {
        String(format: "%.6f", Double(tick.microseconds) / 1_000_000)
    }

    @ViewBuilder
    private func mmText(_ value: Double?) -> some View {
        if let value, value.isFinite {
            Text(String(format: "%.3f", value)).monospacedDigit()
        } else {
            Text("—").foregroundStyle(.tertiary)
        }
    }

    private func ensureTrainSelection() {
        let ids = trains.map { $0.semanticID.semanticID.canonicalText }
        if let selectedTrainID, ids.contains(selectedTrainID) { return }
        selectedTrainID = ids.first
        selectedRowIDs.removeAll()
    }

    private func apply() {
        guard let selectedTrainID else { return }
        document.applyCanonicalManualISILabel(
            selectedLabel,
            trainID: selectedTrainID,
            isiIndices: selectedISIIndices
        )
    }

    private func clear() {
        guard let selectedTrainID else { return }
        document.clearCanonicalManualISILabel(
            track: selectedTrack,
            trainID: selectedTrainID,
            isiIndices: selectedISIIndices
        )
    }

    private func deletePatternRun(_ patternRun: ManualISIGraphicPatternRun) {
        guard let selectedTrainID else { return }
        document.clearCanonicalManualISILabel(
            track: patternRun.lane.semanticTrack,
            trainID: selectedTrainID,
            isiIndices: patternRun.isiIndices(in: graphicRows)
        )
    }
}
