import STPDCore
import SwiftUI

struct LegacyManualISIWorkbenchView: View {
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

    private var trains: [SpikeTrain] { document.dataset?.trains ?? [] }
    private var minimumValidISISeconds: Double {
        max(0, document.artifactThresholdMs) / 1_000
    }

    private var rows: [ManualISIDualTrackExportRow] {
        guard let selectedTrainID else { return [] }
        return document.manualISIRows(for: selectedTrainID)
    }

    private var selectedISIIndices: Set<Int> {
        Set(rows.lazy.filter { selectedRowIDs.contains($0.id) }.map(\.isiIndex))
    }

    private var availableLabels: [ManualAnnotationLabel] {
        ManualAnnotationLabel.positiveLabels(for: selectedTrack)
    }

    private var automaticLabelsByISI: [Int: [ClassicAnchorLabel]] {
        guard let selectedTrainID,
              let train = trains.first(where: { $0.id == selectedTrainID }) else {
            return [:]
        }
        let annotations = document.classicAnchorEventAnnotations
            + document.classicAnchorStateAnnotations
        let availableISIIndices = Set(rows.map(\.isiIndex))
        var labelsByISI: [Int: [ClassicAnchorLabel]] = [:]
        for annotation in annotations where annotation.trainID == train.id {
            guard let covered = annotation.coveredISIIndices(in: train) else { continue }
            for isiIndex in covered where availableISIIndices.contains(isiIndex) {
                if labelsByISI[isiIndex, default: []].contains(annotation.label) == false {
                    labelsByISI[isiIndex, default: []].append(annotation.label)
                }
            }
        }
        return labelsByISI.mapValues { $0.sorted { $0.rawValue < $1.rawValue } }
    }

    private var graphicRows: [ManualISIGraphicRow] {
        let labelsByISI = automaticLabelsByISI
        return rows.map {
            ManualISIGraphicRow(
                id: $0.id,
                isiIndex: $0.isiIndex,
                leftSeconds: $0.leftTimestampSec,
                rightSeconds: $0.rightTimestampSec,
                stateLabel: ManualAnnotationLabel(rawValue: $0.statePattern),
                eventLabel: ManualAnnotationLabel(rawValue: $0.eventPattern),
                otherLabel: ManualAnnotationLabel(rawValue: $0.otherPattern),
                automaticLabels: labelsByISI[$0.isiIndex] ?? [],
                isInvalidISI: ManualISIQualityRule.isAbsolutelyInvalid(
                    isiSeconds: $0.isiSec,
                    thresholdSeconds: minimumValidISISeconds
                )
            )
        }
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            header
            contractNotice

            if trains.isEmpty {
                ContentUnavailableView(
                    l10n.t("尚未加载 spike train"),
                    systemImage: "tablecells",
                    description: Text(l10n.t("请先导入或打开数据集，再进行 ISI 人工标注。"))
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
                        return document.applyManualISILabel(
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
                isiTable
            }
        }
        .padding(16)
        .onAppear(perform: ensureValidSelection)
        .onChange(of: trains.map(\.id)) { _, _ in ensureValidSelection() }
        .onChange(of: selectedTrainID) { _, _ in
            selectedRowIDs.removeAll()
            focusedRowID = nil
        }
        .onChange(of: selectedTrack) { _, newTrack in
            selectedLabel = ManualAnnotationLabel.positiveLabels(for: newTrack).first ?? .other
        }
    }

    private var header: some View {
        HStack(spacing: 10) {
            Text(l10n.t("纯手工 ISI 标记（无需模式检测）"))
                .font(.title3.weight(.semibold))
            Text(l10n.t("本地草稿 · 未封存"))
                .font(.caption.weight(.semibold))
                .foregroundStyle(.orange)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(.orange.opacity(0.12), in: Capsule())
            Spacer()
            Button {
                document.exportManualISILabelDraftWithPanel()
            } label: {
                Label(l10n.t("导出已标注 ISI"), systemImage: "square.and.arrow.up")
            }
            .disabled(!document.canExportManualISILabelDraft)
        }
    }

    private var contractNotice: some View {
        HStack(alignment: .top, spacing: 9) {
            Image(systemName: "info.circle")
                .foregroundStyle(.secondary)
            Text(l10n.t("时间图和表格都只使用原始 spike 时间戳，不运行模式检测。可在时间图拖动选择连续 ISI，也可在表格精确多选。状态与事件相互独立，因此 HFS 可与其内嵌的 HFB 共存；可选择导出 CSV、XLSX 或 NeuroExplorer NEX，文件会明确标记为本地人工草稿。"))
                .font(.callout)
                .foregroundStyle(.secondary)
        }
        .padding(10)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.quaternary.opacity(0.45), in: RoundedRectangle(cornerRadius: 8))
    }

    private var controls: some View {
        HStack(alignment: .center, spacing: 12) {
            Picker(l10n.t("Spike train"), selection: $selectedTrainID) {
                ForEach(trains) { train in
                    Text(train.name).tag(Optional(train.id))
                }
            }
            .frame(minWidth: 180, idealWidth: 240)

            Picker(l10n.t("轨道"), selection: $selectedTrack) {
                ForEach(ManualAnnotationSemanticTrack.allCases, id: \.self) { track in
                    Text(track.displayName(using: l10n)).tag(track)
                }
            }
            .pickerStyle(.segmented)
            .frame(width: 240)

            Picker(l10n.t("模式"), selection: $selectedLabel) {
                ForEach(availableLabels, id: \.self) { label in
                    Text(label.displayName(using: l10n)).tag(label)
                }
            }
            .frame(minWidth: 150)

            Button(l10n.t("应用")) { applyLabel() }
                .buttonStyle(.borderedProminent)
                .disabled(selectedISIIndices.isEmpty)

            Button(selectedTrack.clearTitle(using: l10n)) { clearTrack() }
                .disabled(selectedISIIndices.isEmpty)
        }
    }

    private var selectionSummary: some View {
        HStack(spacing: 10) {
            Text(selectionCountText)
                .font(.callout.monospacedDigit())
                .foregroundStyle(.secondary)
            if selectedLabel.minimumManualAuthoringSpikeCount > 1 {
                Text(selectedLabel.minimumManualAuthoringRequirement(using: l10n))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            Button(l10n.t("全选")) { selectedRowIDs = Set(rows.map(\.id)) }
                .disabled(rows.isEmpty || selectedRowIDs.count == rows.count)
            Button(l10n.t("清除选择")) { selectedRowIDs.removeAll() }
                .disabled(selectedRowIDs.isEmpty)
            Spacer()
            Text(l10n.t("使用 Shift-单击或 Command-单击进行批量选择。"))
                .font(.caption)
                .foregroundStyle(.tertiary)
        }
    }

    private var isiTable: some View {
        let tableRows = rows
        let ratiosByRowID = ManualISINeighborhoodRatios.indexedSeconds(
            tableRows.map { (id: $0.id, interval: $0.isiSec) }
        )

        return Table(tableRows, selection: $selectedRowIDs) {
            TableColumn(l10n.t("左时间戳（s）")) { row in
                Text(number(row.leftTimestampSec)).monospacedDigit()
            }
            .width(min: 85, ideal: 105)

            TableColumn(l10n.t("左 MM")) { row in
                mmText(ratiosByRowID[row.id]?.leftMM)
                    .help(l10n.t("左 MM = 左邻 ISI ÷ 当前 ISI"))
            }
            .width(min: 64, ideal: 76, max: 92)

            TableColumn(l10n.t("当前 ISI（ms）")) { row in
                Text(number(row.isiSec * 1_000))
                    .monospacedDigit()
                    .fontWeight(selectedRowIDs.contains(row.id) ? .bold : .regular)
                    .foregroundStyle(
                        ManualISIQualityRule.isAbsolutelyInvalid(
                            isiSeconds: row.isiSec,
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

            TableColumn(l10n.t("右时间戳（s）")) { row in
                Text(number(row.rightTimestampSec)).monospacedDigit()
            }
            .width(min: 85, ideal: 105)

            TableColumn(ManualAnnotationSemanticTrack.event.displayName(using: l10n)) { row in
                patternText(row.eventPattern)
            }
            .width(min: 90, ideal: 130)

            TableColumn(ManualAnnotationSemanticTrack.state.displayName(using: l10n)) { row in
                patternText(row.statePattern)
            }
            .width(min: 90, ideal: 130)

            TableColumn(l10n.t("备注")) { row in
                Text(row.reviewNote.isEmpty ? "—" : row.reviewNote)
                    .foregroundStyle(
                        row.reviewNote.isEmpty
                            ? Color.secondary.opacity(0.55)
                            : Color.orange
                    )
            }
            .width(min: 120, ideal: 210)

            TableColumn(ManualAnnotationSemanticTrack.other.displayName(using: l10n)) { row in
                patternText(row.otherPattern)
            }
            .width(min: 70, ideal: 90)

            TableColumn("ISI #") { row in
                Text(String(row.isiIndex)).monospacedDigit()
            }
            .width(min: 52, ideal: 62, max: 80)
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
    private func patternText(_ rawValue: String) -> some View {
        if let label = ManualAnnotationLabel(rawValue: rawValue) {
            Text(label.displayName(using: l10n))
        } else {
            Text("—").foregroundStyle(.tertiary)
        }
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

    private func ensureValidSelection() {
        if let selectedTrainID, trains.contains(where: { $0.id == selectedTrainID }) {
            return
        }
        selectedTrainID = trains.first?.id
        selectedRowIDs.removeAll()
    }

    private func applyLabel() {
        guard let selectedTrainID else { return }
        document.applyManualISILabel(
            selectedLabel,
            trainID: selectedTrainID,
            isiIndices: selectedISIIndices
        )
    }

    private func clearTrack() {
        guard let selectedTrainID else { return }
        document.clearManualISILabel(
            track: selectedTrack,
            trainID: selectedTrainID,
            isiIndices: selectedISIIndices
        )
    }

    private func deletePatternRun(_ patternRun: ManualISIGraphicPatternRun) {
        guard let selectedTrainID else { return }
        document.clearManualISILabel(
            track: patternRun.lane.semanticTrack,
            trainID: selectedTrainID,
            isiIndices: patternRun.isiIndices(in: graphicRows)
        )
    }

    private func number(_ value: Double) -> String {
        value.isFinite ? String(format: "%.6f", value) : "—"
    }

    @ViewBuilder
    private func mmText(_ value: Double?) -> some View {
        if let value, value.isFinite {
            Text(String(format: "%.3f", value)).monospacedDigit()
        } else {
            Text("—").foregroundStyle(.tertiary)
        }
    }
}

extension ManualISIGraphicPatternLane {
    var semanticTrack: ManualAnnotationSemanticTrack {
        switch self {
        case .state: .state
        case .event: .event
        case .other: .other
        }
    }
}

extension ManualAnnotationSemanticTrack {
    var displayNameZH: String {
        switch self {
        case .state: "状态"
        case .event: "事件"
        case .other: "其他"
        }
    }

    func displayName(using l10n: STPDLocalizer) -> String {
        switch (self, l10n.language) {
        case (.state, .zh): "状态"
        case (.event, .zh): "事件"
        case (.other, .zh): "其他"
        case (.state, .en): "State"
        case (.event, .en): "Event"
        case (.other, .en): "Other"
        case (.state, .ru): "Состояние"
        case (.event, .ru): "Событие"
        case (.other, .ru): "Другое"
        }
    }

    func clearTitle(using l10n: STPDLocalizer) -> String {
        switch l10n.language {
        case .zh: "清除\(displayName(using: l10n))"
        case .en: "Clear \(displayName(using: l10n))"
        case .ru: "Очистить: \(displayName(using: l10n))"
        }
    }
}

extension ManualAnnotationLabel {
    var displayNameZH: String {
        switch self {
        case .burst: "Burst"
        case .highFrequencyBurst: "高频 Burst（HFB）"
        case .longBurst: "长 Burst"
        case .tonic: "Tonic"
        case .highFrequencyTonic: "高频 Tonic"
        case .highFrequencySpiking: "高频持续发放（HFS）"
        case .pause: "Pause"
        case .other: "其他"
        case .notBurst: "非 Burst（否决）"
        }
    }

    var minimumManualAuthoringRequirementZH: String {
        "\(displayNameZH) 每段至少 \(minimumManualAuthoringSpikeCount) 个 spike"
    }

    func displayName(using l10n: STPDLocalizer) -> String {
        l10n.t(displayNameZH)
    }

    func minimumManualAuthoringRequirement(using l10n: STPDLocalizer) -> String {
        switch l10n.language {
        case .zh:
            "\(displayName(using: l10n)) 每段至少 \(minimumManualAuthoringSpikeCount) 个 spike"
        case .en:
            "\(displayName(using: l10n)) requires at least \(minimumManualAuthoringSpikeCount) spikes per span"
        case .ru:
            "Для \(displayName(using: l10n)) нужно не менее \(minimumManualAuthoringSpikeCount) спайков в сегменте"
        }
    }
}
