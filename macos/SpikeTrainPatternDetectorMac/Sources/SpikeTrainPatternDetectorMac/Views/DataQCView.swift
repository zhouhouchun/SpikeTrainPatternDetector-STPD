import STPDCore
import SwiftUI

struct DataQCView: View {
    @Bindable var document: RasterDocument

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                header
                importControls

                if let dataset = document.dataset, let report = document.qualityReport {
                    summaryStrip(dataset: dataset, report: report)
                    qualityTable(report: report)
                    artifactDetails(report: report)
                    duplicateDetails(report: report)
                } else {
                    ContentUnavailableView(
                        "尚未加载数据集",
                        systemImage: "exclamationmark.shield",
                        description: Text(document.lastErrorMessage ?? "请导入并审核 CSV/XLSX 时间戳表，或加载随应用提供的演示样本。")
                    )
                    .frame(maxWidth: .infinity, minHeight: 320)
                }
            }
            .padding(18)
            .frame(maxWidth: .infinity, alignment: .topLeading)
        }
        .frame(minWidth: 720)
    }

    private var artifactThresholdBinding: Binding<Double> {
        thresholdBinding(
            unit: document.artifactThresholdUnit,
            getMilliseconds: { document.artifactThresholdMs },
            setMilliseconds: { document.artifactThresholdMs = max(0, $0) }
        )
    }

    private var refractoryThresholdBinding: Binding<Double> {
        thresholdBinding(
            unit: document.refractorySuspectThresholdUnit,
            getMilliseconds: { document.refractorySuspectThresholdMs },
            setMilliseconds: { document.refractorySuspectThresholdMs = max(0, $0) }
        )
    }

    private var header: some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Text("数据 QC")
                .font(.title3.weight(.semibold))
            Text("可用")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(.quaternary, in: Capsule())
            Spacer()
        }
    }

    private var importControls: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack(spacing: 12) {
                Label(
                    "CSV/XLSX 的数据源事实与科学含义需要分步明确确认。",
                    systemImage: "checklist"
                )
                .font(.callout)
                .foregroundStyle(.secondary)

                Spacer()

                Button {
                    document.openScientificImportWithPanel()
                } label: {
                    Label("导入数据", systemImage: "folder")
                }
                .liquidGlassButtonStyle()
                .help("打开两阶段 CSV/XLSX 导入审核")

                Button {
                    document.loadBundledSample()
                } label: {
                    Label("加载演示样本", systemImage: "arrow.clockwise")
                }
                .liquidGlassButtonStyle()
                .help("加载随应用提供的非权威演示数据集")
            }

            HStack(spacing: 24) {
                thresholdControl(
                    "绝对无效 ISI 上限",
                    value: artifactThresholdBinding,
                    unit: $document.artifactThresholdUnit,
                    pickerLabel: "绝对无效 ISI 上限的单位"
                )

                thresholdControl(
                    "疑似不应期 ISI 上限",
                    value: refractoryThresholdBinding,
                    unit: $document.refractorySuspectThresholdUnit,
                    pickerLabel: "疑似不应期 ISI 上限的单位"
                )

                HStack(spacing: 8) {
                    Text("显示单位")
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                        .fixedSize(horizontal: true, vertical: false)
                    GlassSegmentedControl(
                        options: [
                            (.milliseconds, "ms"),
                            (.seconds, "s")
                        ],
                        selection: $document.qcDisplayUnit,
                        minSegmentWidth: 30
                    )
                    .frame(width: 88)
                    .help("控制 QC 结果和消息的显示单位，不会更改导入的原始时间戳。")
                }

                Spacer()
            }
        }
        .padding(14)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private func thresholdControl(
        _ title: String,
        value: Binding<Double>,
        unit: Binding<QualityDisplayUnit>,
        pickerLabel: String
    ) -> some View {
        HStack(spacing: 8) {
            Text(title)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            DebouncedDoubleField("0", value: value, width: 96)
            GlassSegmentedControl(
                options: [
                    (.milliseconds, "ms"),
                    (.seconds, "s")
                ],
                selection: unit,
                minSegmentWidth: 30
            )
            .frame(width: 82)
            .help(pickerLabel)
        }
    }

    private func thresholdBinding(
        unit: QualityDisplayUnit,
        getMilliseconds: @escaping () -> Double,
        setMilliseconds: @escaping (Double) -> Void
    ) -> Binding<Double> {
        Binding(
            get: {
                let seconds = getMilliseconds() / 1000
                return seconds * unit.scaleFromSeconds
            },
            set: { newValue in
                let seconds = max(0, newValue) / unit.scaleFromSeconds
                setMilliseconds(seconds * 1000)
            }
        )
    }

    private func summaryStrip(dataset: SpikeDataset, report: SpikeDatasetQualityReport) -> some View {
        HStack(spacing: 10) {
            SummaryTile(title: "序列", value: "\(dataset.trains.count)")
            SummaryTile(title: "Spike", value: "\(dataset.totalSpikeCount)")
            SummaryTile(title: "错误", value: "\(report.errorCount)", level: report.errorCount > 0 ? .error : .ok)
            SummaryTile(title: "警告", value: "\(report.warningCount)", level: report.warningCount > 0 ? .warning : .ok)
            SummaryTile(title: "绝对无效 ISI", value: "\(report.artifactISICount)", level: report.artifactISICount > 0 ? .warning : .ok)
            SummaryTile(
                title: report.droppedDuplicateTimestampCount > 0 ? "已折叠重复值" : "重复时间戳",
                value: report.droppedDuplicateTimestampCount > 0 ? "\(report.droppedDuplicateTimestampCount)" : "\(report.duplicateTimestampCount)",
                level: duplicateSummaryLevel(report)
            )
        }
    }

    private func duplicateSummaryLevel(_ report: SpikeDatasetQualityReport) -> QualityWarningLevel {
        if report.duplicateTimestampCount > 0 {
            return report.errorCount > 0 ? .error : .warning
        }
        return report.droppedDuplicateTimestampCount > 0 ? .warning : .ok
    }

    private func qualityTable(report: SpikeDatasetQualityReport) -> some View {
        let rows = sortedQualityRows(report.rows)

        return VStack(alignment: .leading, spacing: 8) {
            Text("质量表")
                .font(.headline)

            VStack(alignment: .leading, spacing: 0) {
                HStack(spacing: 12) {
                    qualityHeader("状态", width: 86)
                    qualityHeader("Spike train", width: 190)
                    qualityHeader("Spike 数", width: 72)
                    qualityHeader("时长", width: 130)
                    qualityHeader("Min ISI", width: 120)
                    qualityHeader("Median ISI", width: 130)
                    qualityHeader("绝对无效", width: 74)
                    qualityHeader("疑似不应期", width: 92)
                    qualityHeader("重复", width: 86)
                }
                .padding(.horizontal, 8)
                .padding(.vertical, 7)

                Divider()

                ForEach(rows) { row in
                    qualityRow(row)
                }
            }
        }
    }

    private func qualityRow(_ row: SpikeTrainQuality) -> some View {
        VStack(alignment: .leading, spacing: 7) {
            HStack(alignment: .top, spacing: 12) {
                StatusLabel(level: row.warningLevel)
                    .frame(width: 86, alignment: .leading)
                Text(row.trainName)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .frame(width: 190, alignment: .leading)
                qualityMetric("\(row.spikeCount)", width: 72)
                qualityMetric(time(row.durationSec), width: 130)
                qualityMetric(time(row.rawMinISISec), width: 120)
                qualityMetric(time(row.medianISISec), width: 130)
                qualityMetric("\(row.artifactISICount)", width: 74)
                qualityMetric("\(row.refractorySuspectISICount)", width: 92)
                qualityMetric("\(row.duplicateTimestampCount)", width: 86)
            }

            if !row.warningMessage.isEmpty {
                HStack(alignment: .top, spacing: 12) {
                    Text("消息")
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                        .frame(width: 86, alignment: .leading)
                    Text(timestampQCMessage(row.warningMessage))
                        .font(.caption)
                        .foregroundStyle(row.warningLevel == .error ? Color.red : Color.primary)
                        .fixedSize(horizontal: false, vertical: true)
                        .textSelection(.enabled)
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
            }
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 8)
        .background(qualityRowBackground(row.warningLevel), in: RoundedRectangle(cornerRadius: 8))
    }

    private func sortedQualityRows(_ rows: [SpikeTrainQuality]) -> [SpikeTrainQuality] {
        rows.enumerated()
            .sorted { lhs, rhs in
                let lhsPriority = qualitySortPriority(lhs.element.warningLevel)
                let rhsPriority = qualitySortPriority(rhs.element.warningLevel)
                if lhsPriority != rhsPriority {
                    return lhsPriority < rhsPriority
                }
                return lhs.offset < rhs.offset
            }
            .map(\.element)
    }

    private func qualitySortPriority(_ level: QualityWarningLevel) -> Int {
        switch level {
        case .error:
            return 0
        case .warning:
            return 1
        case .ok:
            return 2
        }
    }

    private func qualityHeader(_ title: String, width: CGFloat) -> some View {
        Text(title)
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
            .frame(width: width, alignment: .leading)
    }

    private func qualityMetric(_ text: String, width: CGFloat) -> some View {
        Text(text)
            .monospacedDigit()
            .frame(width: width, alignment: .leading)
    }

    private func qualityRowBackground(_ level: QualityWarningLevel) -> Color {
        switch level {
        case .ok:
            return Color(nsColor: .controlBackgroundColor).opacity(0.55)
        case .warning:
            return Color.yellow.opacity(0.12)
        case .error:
            return Color.red.opacity(0.11)
        }
    }

    private func artifactDetails(report: SpikeDatasetQualityReport) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("低于最小值的 ISI 明细")
                .font(.headline)

            if report.artifactDetails.isEmpty {
                EmptyTableNote(text: "没有 ISI 落入当前绝对无效阈值范围。")
            } else {
                Table(report.artifactDetails) {
                    TableColumn("Spike train") { detail in
                        Text(detail.trainName)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                    TableColumn("ISI 序号") { detail in
                        Text("\(detail.isiIndex)")
                            .monospacedDigit()
                    }
                    TableColumn("左侧 spike") { detail in
                        Text(time(detail.leftSpikeTimeSec))
                            .monospacedDigit()
                    }
                    TableColumn("右侧 spike") { detail in
                        Text(time(detail.rightSpikeTimeSec))
                            .monospacedDigit()
                    }
                    TableColumn("ISI") { detail in
                        Text(time(detail.isiSec))
                            .monospacedDigit()
                    }
                    TableColumn("阈值") { detail in
                        Text(time(detail.thresholdSec))
                            .monospacedDigit()
                    }
                }
                .frame(minHeight: 160)
            }
        }
    }

    private func duplicateDetails(report: SpikeDatasetQualityReport) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("重复时间戳明细")
                .font(.headline)

            if report.duplicateDetails.isEmpty {
                EmptyTableNote(text: "保留的 spike train 中没有重复时间戳。")
            } else {
                Table(report.duplicateDetails) {
                    TableColumn("Spike train") { detail in
                        Text(detail.trainName)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                    TableColumn("时间戳") { detail in
                        Text(time(detail.timestampSec))
                            .monospacedDigit()
                    }
                    TableColumn("数量") { detail in
                        Text("\(detail.duplicateCount)")
                            .monospacedDigit()
                    }
                    TableColumn("排序后行号") { detail in
                        Text(detail.sortedRowIndices.map(String.init).joined(separator: ";"))
                            .monospacedDigit()
                    }
                    TableColumn("输入行号") { detail in
                        Text(detail.inputOrderIndices.map(String.init).joined(separator: ";"))
                            .monospacedDigit()
                    }
                    TableColumn("处理策略") { detail in
                        Text(detail.policy.rawValue)
                    }
                }
                .frame(minHeight: 160)
            }
        }
    }

    private func time(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return ""
        }

        let scaled = value * document.qcDisplayUnit.scaleFromSeconds
        let suffix = document.qcDisplayUnit.rawValue
        if abs(scaled) >= 100 {
            return "\(String(format: "%.3f", scaled)) \(suffix)"
        }
        if abs(scaled) >= 1 {
            return "\(String(format: "%.4f", scaled)) \(suffix)"
        }
        return "\(String(format: "%.6f", scaled)) \(suffix)"
    }

    /// The persisted QC model and result schema retain their legacy `artifact_*`
    /// machine names for compatibility. On timestamp-only UI surfaces, describe
    /// the observable fact instead: the ISI is below the user-defined validity
    /// floor. A short interval alone does not prove an acquisition artifact.
    private func timestampQCMessage(_ message: String) -> String {
        message
            .replacingOccurrences(of: "Artifact ISI", with: "绝对无效 ISI")
            .replacingOccurrences(of: "Artifact fraction", with: "绝对无效 ISI 比例")
            .replacingOccurrences(of: "artifact threshold", with: "绝对无效 ISI 阈值")
    }
}

private struct SummaryTile: View {
    let title: String
    let value: String
    var level: QualityWarningLevel = .ok

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(title)
                .font(.caption)
                .foregroundStyle(.secondary)
            Text(value)
                .font(.title3.weight(.semibold))
                .monospacedDigit()
        }
        .padding(12)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(backgroundStyle, in: RoundedRectangle(cornerRadius: 8))
    }

    private var backgroundStyle: Color {
        switch level {
        case .ok:
            return Color(nsColor: .controlBackgroundColor)
        case .warning:
            return Color.yellow.opacity(0.16)
        case .error:
            return Color.red.opacity(0.14)
        }
    }
}

private struct StatusLabel: View {
    let level: QualityWarningLevel

    var body: some View {
        Text(level.title)
            .font(.caption.weight(.semibold))
            .foregroundStyle(foregroundStyle)
            .padding(.horizontal, 7)
            .padding(.vertical, 3)
            .background(backgroundStyle, in: Capsule())
    }

    private var foregroundStyle: Color {
        switch level {
        case .ok:
            return .secondary
        case .warning:
            return .orange
        case .error:
            return .red
        }
    }

    private var backgroundStyle: Color {
        switch level {
        case .ok:
            return Color.secondary.opacity(0.12)
        case .warning:
            return Color.orange.opacity(0.14)
        case .error:
            return Color.red.opacity(0.14)
        }
    }
}

private struct EmptyTableNote: View {
    let text: String

    var body: some View {
        Text(text)
            .foregroundStyle(.secondary)
            .frame(maxWidth: .infinity, minHeight: 70, alignment: .center)
            .background(Color(nsColor: .controlBackgroundColor), in: RoundedRectangle(cornerRadius: 8))
    }
}
