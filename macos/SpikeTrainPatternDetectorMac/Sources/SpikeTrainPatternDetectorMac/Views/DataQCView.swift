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
                        "No Dataset",
                        systemImage: "exclamationmark.shield",
                        description: Text(document.lastErrorMessage ?? "Open a raw spike timestamp CSV or load the bundled sample.")
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
            Image(systemName: "exclamationmark.shield")
                .foregroundStyle(.secondary)
            Text("数据 QC")
                .font(.title3.weight(.semibold))
            Text("Live")
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
                GlassSegmentedControl(
                    options: [
                        (SpikeTimeUnit.seconds, "s"),
                        (.milliseconds, "ms")
                    ],
                    selection: $document.rawImportUnit,
                    minSegmentWidth: 42
                )
                .frame(width: 130)
                .help("Raw timestamp unit in the imported CSV.")

                Toggle("Header row", isOn: $document.rawCSVHasHeader)

                DuplicateTimestampPolicyControl(document: document)

                Spacer()

                Button {
                    document.openCSVWithPanel()
                } label: {
                    Label("Open CSV", systemImage: "folder")
                }
                .liquidGlassButtonStyle()

                Button {
                    document.loadBundledSample()
                } label: {
                    Label("Load Sample", systemImage: "arrow.clockwise")
                }
                .liquidGlassButtonStyle()
            }

            HStack(spacing: 24) {
                thresholdControl(
                    "Minimum valid ISI",
                    value: artifactThresholdBinding,
                    unit: $document.artifactThresholdUnit,
                    pickerLabel: "Minimum valid ISI unit"
                )

                thresholdControl(
                    "Refractory suspect",
                    value: refractoryThresholdBinding,
                    unit: $document.refractorySuspectThresholdUnit,
                    pickerLabel: "Refractory suspect threshold unit"
                )

                HStack(spacing: 8) {
                    Text("Display unit")
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
                    .help("Controls the display unit for QC results and messages. It does not change the raw imported timestamps.")
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
            SummaryTile(title: "Trains", value: "\(dataset.trains.count)")
            SummaryTile(title: "Spikes", value: "\(dataset.totalSpikeCount)")
            SummaryTile(title: "Errors", value: "\(report.errorCount)", level: report.errorCount > 0 ? .error : .ok)
            SummaryTile(title: "Warnings", value: "\(report.warningCount)", level: report.warningCount > 0 ? .warning : .ok)
            SummaryTile(title: "Below-minimum ISI", value: "\(report.artifactISICount)", level: report.artifactISICount > 0 ? .warning : .ok)
            SummaryTile(
                title: report.droppedDuplicateTimestampCount > 0 ? "Duplicates merged" : "Duplicates",
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
            Text("Quality table")
                .font(.headline)

            VStack(alignment: .leading, spacing: 0) {
                HStack(spacing: 12) {
                    qualityHeader("Status", width: 86)
                    qualityHeader("Train", width: 190)
                    qualityHeader("Spikes", width: 72)
                    qualityHeader("Duration", width: 130)
                    qualityHeader("Min ISI", width: 120)
                    qualityHeader("Median ISI", width: 130)
                    qualityHeader("Below min", width: 74)
                    qualityHeader("Refractory", width: 92)
                    qualityHeader("Duplicates", width: 86)
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
                    Text("Message")
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
            Text("Below-minimum ISI details")
                .font(.headline)

            if report.artifactDetails.isEmpty {
                EmptyTableNote(text: "No ISI falls below the current minimum-valid threshold.")
            } else {
                Table(report.artifactDetails) {
                    TableColumn("Train") { detail in
                        Text(detail.trainName)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                    TableColumn("ISI index") { detail in
                        Text("\(detail.isiIndex)")
                            .monospacedDigit()
                    }
                    TableColumn("Left") { detail in
                        Text(time(detail.leftSpikeTimeSec))
                            .monospacedDigit()
                    }
                    TableColumn("Right") { detail in
                        Text(time(detail.rightSpikeTimeSec))
                            .monospacedDigit()
                    }
                    TableColumn("ISI") { detail in
                        Text(time(detail.isiSec))
                            .monospacedDigit()
                    }
                    TableColumn("Threshold") { detail in
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
            Text("Duplicate timestamp details")
                .font(.headline)

            if report.duplicateDetails.isEmpty {
                EmptyTableNote(text: "No duplicate timestamps in the retained trains.")
            } else {
                Table(report.duplicateDetails) {
                    TableColumn("Train") { detail in
                        Text(detail.trainName)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                    TableColumn("Timestamp") { detail in
                        Text(time(detail.timestampSec))
                            .monospacedDigit()
                    }
                    TableColumn("Count") { detail in
                        Text("\(detail.duplicateCount)")
                            .monospacedDigit()
                    }
                    TableColumn("Sorted rows") { detail in
                        Text(detail.sortedRowIndices.map(String.init).joined(separator: ";"))
                            .monospacedDigit()
                    }
                    TableColumn("Input rows") { detail in
                        Text(detail.inputOrderIndices.map(String.init).joined(separator: ";"))
                            .monospacedDigit()
                    }
                    TableColumn("Policy") { detail in
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
            .replacingOccurrences(of: "Artifact ISI", with: "Below-minimum ISI")
            .replacingOccurrences(of: "Artifact fraction", with: "Below-minimum ISI fraction")
            .replacingOccurrences(of: "artifact threshold", with: "minimum-valid ISI threshold")
    }
}

private struct DuplicateTimestampPolicyControl: View {
    @Bindable var document: RasterDocument
    @State private var pendingPolicy: DuplicateTimestampPolicy = .errorKeep

    var body: some View {
        HStack(spacing: 10) {
            HStack(spacing: 6) {
                Text("Duplicate timestamps")
                    .foregroundStyle(.secondary)
                    .lineLimit(1)

                Picker("Duplicate timestamps", selection: pendingPolicyBinding) {
                    ForEach(DuplicateTimestampPolicy.allCases, id: \.self) { policy in
                        Text(policy.title).tag(policy)
                    }
                }
                .labelsHidden()
                .frame(width: 150)
            }

            Button {
                document.applyDuplicateTimestampPolicy(pendingPolicy)
                syncPendingPolicy()
            } label: {
                Label("Apply", systemImage: "checkmark.circle")
            }
            .liquidGlassButtonStyle(prominent: true)
            .disabled(!hasPendingPolicy)
            .help("Apply the selected duplicate timestamp policy to the current dataset.")
        }
        .onAppear {
            syncPendingPolicy()
        }
        .onChange(of: document.appliedDuplicateTimestampPolicy) { _, _ in
            syncPendingPolicy()
        }
        .onChange(of: document.statusMessage) { _, _ in
            syncPendingPolicy()
        }
    }

    private var pendingPolicyBinding: Binding<DuplicateTimestampPolicy> {
        Binding(
            get: { pendingPolicy },
            set: { pendingPolicy = $0 }
        )
    }

    private var hasPendingPolicy: Bool {
        guard document.dataset != nil else {
            return false
        }
        return pendingPolicy != appliedPolicy
    }

    private var appliedPolicy: DuplicateTimestampPolicy {
        document.appliedDuplicateTimestampPolicy ?? document.duplicateTimestampPolicy
    }

    private func syncPendingPolicy() {
        pendingPolicy = appliedPolicy
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
