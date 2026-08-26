import STPDCore
import SwiftUI

/// A READ-ONLY "Simulator / Preview" page. It renders one of two preview modes:
///  - Tonic regularity ladder (SIM-1F): each rung's verdict under the detector DEFAULTS vs. the app's CURRENT
///    state-pattern tuning (`SpikeTrainSimulationPreview.compare`).
///  - Burst response ladder (SIM-2D, model from SIM-2C): one burst-response train per packet-size rung, each onset's
///    coverage verdict under the DEFAULTS vs. the CURRENT tuning + detector parameters
///    (`SpikeTrainSimulationBurstPreview.compare`).
///
/// It owns only local `@State` plus immutable snapshots (`stateTuning`, `detectorParameters`): it never reads or mutates
/// the live `RasterDocument`, dataset, detection run, manual thresholds, annotations, review state, CSV export, or
/// staleness — and it never applies or copies suggested thresholds. All generated trains/runs are local preview values.
/// No R at runtime.
struct SimulatorPreviewView: View {
    @Environment(\.l10n) private var l10n

    /// SIM-1E: an IMMUTABLE, read-only snapshot of the app's current state-pattern tuning (a value-type copy — the view
    /// never holds or mutates the document). The tonic ladder and the burst-state labels are evaluated against these
    /// CURRENT gates. The default preserves the standalone initializer for previews/tests (= the detector defaults).
    let stateTuning: StatePatternDetectorTuning
    /// SIM-2D: an IMMUTABLE, read-only snapshot of the app's current detector pattern parameters (burst contrast /
    /// min-max spikes). The burst-response ladder's CURRENT verdicts are evaluated against these. Default = detector
    /// defaults (preserves the standalone initializer for previews/tests).
    let detectorParameters: PatternDetectionParameterSettings

    init(stateTuning: StatePatternDetectorTuning = StatePatternDetectorTuning(),
         detectorParameters: PatternDetectionParameterSettings = .defaults) {
        self.stateTuning = stateTuning
        self.detectorParameters = detectorParameters
    }

    // Local preview controls — independent of the document.
    @State private var mode: PatternMode = .tonic
    @State private var seedText: String = "20260624"
    @State private var durationSec: Double = 30
    @State private var meanISIMs: Double = 450
    @State private var preset: JitterPreset = .standard
    @State private var packetPreset: PacketPreset = .standard

    @State private var rows: [SpikeTrainSimulationComparisonRow] = []
    @State private var burstRows: [SpikeTrainSimulationBurstComparisonRow] = []
    @State private var selectedRowID: String?
    @State private var selectedBurstRowID: String?
    @State private var isGenerating = false
    @State private var generationID: UInt64 = 0

    enum PatternMode: String, CaseIterable, Identifiable {
        case tonic, burstResponse
        var id: String { rawValue }
        var sourceZH: String { self == .tonic ? "强直" : "爆发响应" }
    }

    enum JitterPreset: String, CaseIterable, Identifiable {
        case standard, fine, wide
        var id: String { rawValue }
        var ladder: [Double] {
            switch self {
            case .standard: return [0.02, 0.03, 0.06, 0.12, 0.24]
            case .fine: return [0.01, 0.02, 0.04, 0.08, 0.16]
            case .wide: return [0.02, 0.06, 0.12, 0.24, 0.40]
            }
        }
        var sourceZH: String {
            switch self {
            case .standard: return "标准"
            case .fine: return "精细"
            case .wide: return "宽"
            }
        }
    }

    /// SIM-2D: packet-size ladders for the burst-response mode (the rung dimension is spikes-per-burst).
    enum PacketPreset: String, CaseIterable, Identifiable {
        case standard, dense, sparse
        var id: String { rawValue }
        var ladder: [Int] {
            switch self {
            case .standard: return [2, 3, 4, 6, 9]
            case .dense: return [3, 4, 5, 6, 8]
            case .sparse: return [2, 4, 6, 9, 12]
            }
        }
        var sourceZH: String {
            switch self {
            case .standard: return "常规"
            case .dense: return "密集"
            case .sparse: return "稀疏"
            }
        }
    }

    private var config: SpikeTrainSimulationPreviewConfig {
        SpikeTrainSimulationPreviewConfig(
            baseSeed: UInt64(seedText) ?? 0,
            durationSec: durationSec,
            tonicMeanSec: meanISIMs / 1000,
            jitterLadderSec: preset.ladder,
            stateTuning: stateTuning)
    }

    private var burstConfig: SpikeTrainSimulationBurstPreviewConfig {
        SpikeTrainSimulationBurstPreviewConfig(
            baseSeed: UInt64(seedText) ?? 0,
            durationSec: durationSec,
            packetSpikeLadder: packetPreset.ladder,
            stateTuning: stateTuning,
            detectorParameters: detectorParameters)
    }

    private var selectedRow: SpikeTrainSimulationComparisonRow? {
        rows.first { $0.train.id == selectedRowID } ?? rows.first
    }

    private var selectedBurstRow: SpikeTrainSimulationBurstComparisonRow? {
        burstRows.first { $0.train.id == selectedBurstRowID } ?? burstRows.first
    }

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                header
                explanationCard
                controlsCard
                if mode == .tonic {
                    ladderCard
                    rasterCard
                } else {
                    burstLadderCard
                    burstRasterCard
                }
            }
            .padding(16)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .delayedBackgroundProgress(
            isGenerating ? l10n.t("正在生成模拟数据与预览结果…") : nil,
            alignment: .center
        )
        .task { regenerate() }
        .onChange(of: mode) { regenerate() }
        .onChange(of: seedText) { regenerate() }
        .onChange(of: durationSec) { regenerate() }
        .onChange(of: meanISIMs) { regenerate() }
        .onChange(of: preset) { regenerate() }
        .onChange(of: packetPreset) { regenerate() }
    }

    // MARK: - header + explanation

    private var header: some View {
        HStack(alignment: .firstTextBaseline, spacing: 8) {
            Text(l10n.t("模拟 / 预览"))
                .font(.title2.weight(.semibold))
            if isGenerating { ProgressView().controlSize(.small) }
            Spacer(minLength: 12)
            Picker("", selection: $mode) {
                ForEach(PatternMode.allCases) { m in
                    Text(l10n.t(m.sourceZH)).tag(m)
                }
            }
            .pickerStyle(.segmented)
            .fixedSize()
            .accessibilityLabel(Text(l10n.t("模式")))
        }
    }

    @ViewBuilder
    private var explanationCard: some View {
        VStack(alignment: .leading, spacing: 4) {
            if mode == .tonic {
                Text(l10n.t("仅预览：不修改当前数据集、检测运行或检测器参数。CV / CV2 / LV 是评估指标，不是直接的生成旋钮——抖动 SD 才是。"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
                Text("\(l10n.t("按当前检测器门限评估")) · \(l10n.t("强直门限")): "
                    + String(format: "CV≤%.2f · CV2≤%.2f · LV≤%.2f · %@ %d",
                             stateTuning.tonicCVMax, stateTuning.tonicCV2Max, stateTuning.tonicLVMax,
                             l10n.t("最少棘波数"), stateTuning.tonicMinSpikes))
                    .font(.caption2.monospacedDigit())
                    .foregroundStyle(.secondary)
            } else {
                Text(l10n.t("爆发响应预览：在每个刺激时刻放置一个爆发包，按检测器的事件覆盖逐时刻判定。"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
                Text("\(l10n.t("按当前检测器参数评估")) · \(l10n.t("爆发门限")): "
                    + String(format: "contrast≥%.1f · spikes %d–%d",
                             detectorParameters.classicBurstContrastMin,
                             detectorParameters.classicBurstMinSpikes,
                             detectorParameters.classicBurstMaxSpikes))
                    .font(.caption2.monospacedDigit())
                    .foregroundStyle(.secondary)
            }
        }
        .simCard()
    }

    // MARK: - controls

    private var controlsCard: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .firstTextBaseline, spacing: 16) {
                labeledField(l10n.t("种子"), width: 120) {
                    TextField("", text: $seedText)
                        .textFieldStyle(.roundedBorder)
                        .frame(width: 120)
                        .monospacedDigit()
                }
                labeledStepper(l10n.t("时长"), value: $durationSec, range: 5...120, step: 5, suffix: "s")
                if mode == .tonic {
                    labeledStepper(l10n.t("平均 ISI"), value: $meanISIMs, range: 50...1000, step: 10, suffix: "ms")
                }
                Spacer(minLength: 8)
                Button(l10n.t("重置")) { reset() }
                    .buttonStyle(.bordered)
            }
            HStack(alignment: .firstTextBaseline, spacing: 8) {
                if mode == .tonic {
                    Text(l10n.t("抖动阶梯"))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    Picker("", selection: $preset) {
                        ForEach(JitterPreset.allCases) { p in
                            Text(l10n.t(p.sourceZH)).tag(p)
                        }
                    }
                    .pickerStyle(.segmented)
                    .fixedSize()
                    Text(preset.ladder.map { String(format: "%.2f", $0) }.joined(separator: " · "))
                        .font(.caption2.monospacedDigit())
                        .foregroundStyle(.secondary)
                } else {
                    Text(l10n.t("包大小阶梯"))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    Picker("", selection: $packetPreset) {
                        ForEach(PacketPreset.allCases) { p in
                            Text(l10n.t(p.sourceZH)).tag(p)
                        }
                    }
                    .pickerStyle(.segmented)
                    .fixedSize()
                    Text(packetPreset.ladder.map(String.init).joined(separator: " · "))
                        .font(.caption2.monospacedDigit())
                        .foregroundStyle(.secondary)
                }
            }
        }
        .simCard()
    }

    private func labeledField<Content: View>(_ title: String, width: CGFloat, @ViewBuilder _ content: () -> Content)
        -> some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(title).font(.caption).foregroundStyle(.secondary)
            content()
        }
    }

    private func labeledStepper(_ title: String, value: Binding<Double>, range: ClosedRange<Double>, step: Double,
                                suffix: String) -> some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(title).font(.caption).foregroundStyle(.secondary)
            Stepper(value: value, in: range, step: step) {
                Text("\(Int(value.wrappedValue)) \(suffix)").monospacedDigit().font(.callout)
            }
            .fixedSize()
        }
    }

    // MARK: - tonic ladder table

    private var ladderCard: some View {
        VStack(alignment: .leading, spacing: 6) {
            Text(l10n.t("生成的强直序列（按抖动从小到大）"))
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 14, verticalSpacing: 6) {
                GridRow {
                    headerCell(l10n.t("抖动 SD"))
                    headerCell(l10n.t("棘波 / ISI"))
                    headerCell(l10n.t("平均 ISI"))
                    headerCell("CV"); headerCell("CV2"); headerCell("LV")
                    headerCell(l10n.t("默认"))
                    headerCell(l10n.t("当前"))
                    headerCell(l10n.t("变化"))
                }
                ForEach(rows, id: \.train.id) { row in
                    GridRow {
                        numberCell(String(format: "%.3f", row.jitterSDSec))
                        numberCell("\(row.current.spikeCount) / \(row.current.isiCount)")
                        numberCell(row.current.meanISISec.map { String(format: "%.3f", $0) } ?? "—")
                        numberCell(fmt(row.current.cv)); numberCell(fmt(row.current.cv2)); numberCell(fmt(row.current.lv))
                        verdictChip(row.default.reason)
                        verdictChip(row.current.reason)
                        changedCell(row.changed)
                    }
                    .padding(.vertical, 2)
                    .background(rowTint(row), in: RoundedRectangle(cornerRadius: 4))
                    .contentShape(Rectangle())
                    .onTapGesture { selectedRowID = row.train.id }
                }
            }
        }
        .simCard()
    }

    // MARK: - burst-response ladder table

    private var burstLadderCard: some View {
        VStack(alignment: .leading, spacing: 6) {
            Text(l10n.t("生成的爆发响应序列（按包棘波数从小到大）"))
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Grid(alignment: .leadingFirstTextBaseline, horizontalSpacing: 14, verticalSpacing: 6) {
                GridRow {
                    headerCell(l10n.t("包棘波"))
                    headerCell(l10n.t("棘波数"))
                    headerCell(l10n.t("默认"))
                    headerCell(l10n.t("当前"))
                    headerCell(l10n.t("变化"))
                    headerCell(l10n.t("命中 / 可能 / 拆分 / 漏检"))
                }
                ForEach(burstRows, id: \.train.id) { row in
                    GridRow {
                        numberCell("\(row.packetSpikeCount)")
                        numberCell("\(row.current.spikeCount)")
                        burstVerdictChip(dominantBurstVerdict(row.default), short: true)
                        burstVerdictChip(dominantBurstVerdict(row.current), short: true)
                        changedCell(row.changed)
                        numberCell(onsetCountsString(row.current))
                    }
                    .padding(.vertical, 2)
                    .background(burstRowTint(row), in: RoundedRectangle(cornerRadius: 4))
                    .contentShape(Rectangle())
                    .onTapGesture { selectedBurstRowID = row.train.id }
                }
            }
            Text(l10n.t("每个预期爆发时刻的计数。"))
                .font(.caption2)
                .foregroundStyle(.tertiary)
        }
        .simCard()
    }

    private func headerCell(_ text: String) -> some View {
        Text(text).font(.caption2.weight(.semibold)).foregroundStyle(.secondary)
    }
    private func numberCell(_ text: String) -> some View {
        Text(text).font(.caption.monospacedDigit()).foregroundStyle(.primary)
    }
    private func fmt(_ value: Double?) -> String { value.map { String(format: "%.3f", $0) } ?? "—" }

    /// A single-line pill chip. `lineLimit(1)` + `fixedSize` keep long verdict labels on one line (no wrapping into
    /// multi-line capsules inside the narrow Grid cells).
    private func pillChip(_ text: String, tint: Color) -> some View {
        Text(text)
            .font(.caption2.weight(.medium))
            .foregroundStyle(tint)
            .lineLimit(1)
            .fixedSize(horizontal: true, vertical: false)
            .padding(.horizontal, 6)
            .padding(.vertical, 2)
            .background(tint.opacity(0.14), in: Capsule())
    }

    private func verdictChip(_ reason: SpikeTrainSimulationPreviewReason) -> some View {
        pillChip(reason.localizedLabel(language: l10n.language), tint: reason.isPass ? STPDAppTheme.accent : Color.orange)
    }

    /// Burst verdict chip. `short` uses a compact label (爆发 / 可能 / 拆分 / 漏检) for the dense ladder table; the full
    /// label is used on the roomier selected-row card.
    private func burstVerdictChip(_ verdict: SpikeTrainSimulationBurstVerdict, short: Bool = false) -> some View {
        let text = short ? l10n.t(burstVerdictShortZH(verdict)) : verdict.localizedLabel(language: l10n.language)
        return pillChip(text, tint: burstTint(verdict))
    }

    /// Compact Chinese-source label per verdict (localized via the shared dictionary). Reuses the existing 爆发 key.
    private func burstVerdictShortZH(_ verdict: SpikeTrainSimulationBurstVerdict) -> String {
        switch verdict {
        case .detectedAsBurst: return "爆发"
        case .detectedAsPossibleBurst: return "可能"
        case .splitOrPartialPacket: return "拆分"
        case .missedPacket: return "漏检"
        }
    }

    /// The most notable verdict across a burst row's onsets (worst-case precedence: missed > split > possible > burst).
    private func dominantBurstVerdict(_ row: SpikeTrainSimulationBurstPreviewRow) -> SpikeTrainSimulationBurstVerdict {
        let verdicts = row.onsetVerdicts
        if verdicts.isEmpty || verdicts.contains(.missedPacket) { return .missedPacket }
        if verdicts.contains(.splitOrPartialPacket) { return .splitOrPartialPacket }
        if verdicts.contains(.detectedAsPossibleBurst) { return .detectedAsPossibleBurst }
        return .detectedAsBurst
    }

    /// Per-onset count summary: detected-as-burst / possible / split / missed.
    private func onsetCountsString(_ row: SpikeTrainSimulationBurstPreviewRow) -> String {
        var hit = 0, possible = 0, split = 0, missed = 0
        for verdict in row.onsetVerdicts {
            switch verdict {
            case .detectedAsBurst: hit += 1
            case .detectedAsPossibleBurst: possible += 1
            case .splitOrPartialPacket: split += 1
            case .missedPacket: missed += 1
            }
        }
        return "\(hit)/\(possible)/\(split)/\(missed)"
    }

    private func burstTint(_ verdict: SpikeTrainSimulationBurstVerdict) -> Color {
        switch verdict {
        case .detectedAsBurst: return STPDAppTheme.accent
        case .detectedAsPossibleBurst: return .blue
        case .splitOrPartialPacket: return .orange
        case .missedPacket: return .red
        }
    }

    private func changedCell(_ changed: Bool) -> some View {
        HStack(spacing: 3) {
            Image(systemName: changed ? "arrow.left.arrow.right" : "equal")
                .font(.caption2)
            Text(changed ? l10n.t("变化") : l10n.t("无变化"))
                .font(.caption2.weight(.medium))
        }
        .foregroundStyle(changed ? STPDAppTheme.accent : Color.secondary)
    }

    /// Selected row → accent tint; otherwise a changed row gets a faint accent wash so the differences stand out.
    private func rowTint(_ row: SpikeTrainSimulationComparisonRow) -> Color {
        if row.train.id == selectedRow?.train.id { return STPDAppTheme.accent.opacity(0.16) }
        return row.changed ? STPDAppTheme.accent.opacity(0.07) : Color.clear
    }

    private func burstRowTint(_ row: SpikeTrainSimulationBurstComparisonRow) -> Color {
        if row.train.id == selectedBurstRow?.train.id { return STPDAppTheme.accent.opacity(0.16) }
        return row.changed ? STPDAppTheme.accent.opacity(0.07) : Color.clear
    }

    // MARK: - selected-row raster

    private var rasterCard: some View {
        VStack(alignment: .leading, spacing: 6) {
            Text(l10n.t("选中行的光栅"))
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            if let row = selectedRow {
                HStack(spacing: 8) {
                    Text(String(format: "SD %.3f", row.jitterSDSec)).font(.caption2.monospacedDigit()).foregroundStyle(.secondary)
                    Text(l10n.t("默认")).font(.caption2).foregroundStyle(.secondary)
                    verdictChip(row.default.reason)
                    Text(l10n.t("当前")).font(.caption2).foregroundStyle(.secondary)
                    verdictChip(row.current.reason)
                    changedCell(row.changed)
                }
                if !row.current.selectedLabels.isEmpty {
                    Text("\(l10n.t("检测标签")): \(row.current.selectedLabels.joined(separator: ", "))")
                        .font(.caption2.monospaced())
                        .foregroundStyle(.secondary)
                }
                MiniRaster(timestampsSec: row.train.timestampsSec, durationSec: durationSec,
                           tint: row.current.reason.isPass ? STPDAppTheme.accent : .orange)
                    .frame(height: 40)
                    .background(.quaternary.opacity(0.4), in: RoundedRectangle(cornerRadius: 6))
            } else {
                Text("—").font(.caption).foregroundStyle(.secondary)
            }
        }
        .simCard()
    }

    private var burstRasterCard: some View {
        VStack(alignment: .leading, spacing: 6) {
            Text(l10n.t("选中行的光栅"))
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            if let row = selectedBurstRow {
                HStack(spacing: 8) {
                    Text("\(row.packetSpikeCount) \(l10n.t("包棘波"))")
                        .font(.caption2.monospacedDigit()).foregroundStyle(.secondary)
                    Text(l10n.t("默认")).font(.caption2).foregroundStyle(.secondary)
                    burstVerdictChip(dominantBurstVerdict(row.default))
                    Text(l10n.t("当前")).font(.caption2).foregroundStyle(.secondary)
                    burstVerdictChip(dominantBurstVerdict(row.current))
                    changedCell(row.changed)
                }
                Text("\(l10n.t("命中 / 可能 / 拆分 / 漏检")): \(onsetCountsString(row.current))")
                    .font(.caption2.monospacedDigit())
                    .foregroundStyle(.secondary)
                if !row.current.selectedLabels.isEmpty {
                    Text("\(l10n.t("检测标签")): \(row.current.selectedLabels.joined(separator: ", "))")
                        .font(.caption2.monospaced())
                        .foregroundStyle(.secondary)
                }
                // SIM-2G: neutral tick color so the raster doesn't imply every spike is "bad" for missed/split
                // verdicts. The verdict coloring stays in the chips above.
                MiniRaster(timestampsSec: row.train.timestampsSec, durationSec: durationSec,
                           tint: Color.primary.opacity(0.7))
                    .frame(height: 40)
                    .background(.quaternary.opacity(0.4), in: RoundedRectangle(cornerRadius: 6))
            } else {
                Text("—").font(.caption).foregroundStyle(.secondary)
            }
        }
        .simCard()
    }

    // MARK: - actions

    private func reset() {
        seedText = "20260624"
        durationSec = 30
        meanISIMs = 450
        preset = .standard
        packetPreset = .standard
        // controls' onChange fires regenerate; force one in case nothing changed.
        regenerate()
    }

    private func regenerate() {
        switch mode {
        case .tonic: regenerateTonic()
        case .burstResponse: regenerateBurst()
        }
    }

    private func regenerateTonic() {
        let cfg = config
        generationID &+= 1
        let requestID = generationID
        isGenerating = true
        Task {
            let result = await Task.detached(priority: .userInitiated) {
                SpikeTrainSimulationPreview.compare(config: cfg)
            }.value
            await MainActor.run {
                guard requestID == self.generationID else { return }
                self.rows = result
                self.isGenerating = false
                if selectedRowID == nil || !result.contains(where: { $0.train.id == selectedRowID }) {
                    self.selectedRowID = result.first?.train.id
                }
            }
        }
    }

    private func regenerateBurst() {
        let cfg = burstConfig
        generationID &+= 1
        let requestID = generationID
        isGenerating = true
        Task {
            let result = await Task.detached(priority: .userInitiated) {
                SpikeTrainSimulationBurstPreview.compare(config: cfg)
            }.value
            await MainActor.run {
                guard requestID == self.generationID else { return }
                self.burstRows = result
                self.isGenerating = false
                if selectedBurstRowID == nil || !result.contains(where: { $0.train.id == selectedBurstRowID }) {
                    self.selectedBurstRowID = result.first?.train.id
                }
            }
        }
    }
}

private extension View {
    /// SIM-2G: a lighter, cleaner card surface (replaces the heavier `.thinMaterial` block) — a translucent fill with a
    /// hairline border. Layout is unchanged (full width, left-aligned, 12 pt padding, corner radius 8).
    func simCard() -> some View {
        self
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(12)
            .background(.ultraThinMaterial, in: RoundedRectangle(cornerRadius: 8))
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .strokeBorder(Color.primary.opacity(0.06), lineWidth: 1)
            )
    }
}

/// A lightweight spike-tick raster for a single preview train (no document, no detector overlay).
private struct MiniRaster: View {
    let timestampsSec: [Double]
    let durationSec: Double
    let tint: Color

    var body: some View {
        Canvas { context, size in
            guard durationSec > 0 else { return }
            let midY = size.height / 2
            let half = size.height * 0.35
            var path = Path()
            for t in timestampsSec where t >= 0 && t <= durationSec {
                let x = CGFloat(t / durationSec) * size.width
                path.move(to: CGPoint(x: x, y: midY - half))
                path.addLine(to: CGPoint(x: x, y: midY + half))
            }
            context.stroke(path, with: .color(tint), lineWidth: 1)
        }
        .padding(.horizontal, 4)
    }
}
