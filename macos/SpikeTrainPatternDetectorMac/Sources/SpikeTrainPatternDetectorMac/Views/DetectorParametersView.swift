import STPDCore
import SwiftUI

struct DetectorParametersView: View {
    @Bindable var document: RasterDocument

    @Environment(\.l10n) private var l10n

    var body: some View {
        ScrollView([.vertical, .horizontal]) {
            VStack(alignment: .leading, spacing: 16) {
                header

                if let dataset = document.dataset {
                    detectorControls(dataset: dataset)
                    eventPatternControls
                    statePatternControls
                    experimentalControls
                    manualThresholdControls

                    if let run = document.classicAnchorDetectionRun {
                        summaryStrip(run: run)
                        adaptiveBandTable(run: run)
                        candidateAuditTable(run: run)
                    } else {
                        ContentUnavailableView(
                            l10n.t("尚未运行检测"),
                            systemImage: "switch.2",
                            description: Text(document.detectorStatusMessage)
                        )
                        .frame(maxWidth: .infinity, minHeight: 320)
                    }
                } else {
                    ContentUnavailableView(
                        l10n.t("无数据集"),
                        systemImage: "switch.2",
                        description: Text(document.lastErrorMessage ?? l10n.t("打开原始棘波时间戳 CSV，或加载内置示例。"))
                    )
                    .frame(maxWidth: .infinity, minHeight: 360)
                }
            }
            .padding(18)
            .frame(minWidth: 980, maxWidth: .infinity, alignment: .topLeading)
        }
        .frame(minWidth: 720)
    }

    private var header: some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Text(l10n.t("检测器 / 参数"))
                .font(.title3.weight(.semibold))
            Text(l10n.t("实时"))
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(.quaternary, in: Capsule())
            Spacer(minLength: 28)
            if document.detectionResultsAreStale, !document.isDetectorRunning {
                // The displayed raster was computed with different Detector/Parameters values than are set now;
                // surface that so a parameter edit doesn't look ineffective (PARAM-5).
                Label(l10n.t("参数已更改 · 请重新检测"), systemImage: "exclamationmark.arrow.triangle.2.circlepath")
                    .font(.caption.weight(.semibold))
                    .labelStyle(.titleAndIcon)
                    .foregroundStyle(.orange)
                    .help(l10n.t("Detector/Parameters 的更改尚未应用到当前栅格显示，请重新运行检测以生效。"))
            }
            Button {
                document.runAdaptiveClassicAnchorDetection()
            } label: {
                Label(document.hasDetectorResults ? l10n.t("重新检测") : l10n.t("运行检测"), systemImage: "play.fill")
            }
            .liquidGlassButtonStyle(prominent: true)
            .disabled(!document.canRunAdaptiveClassicAnchorDetection)
        }
    }

    private func detectorControls(dataset: SpikeDataset) -> some View {
        HStack(alignment: .center, spacing: 18) {
            detectorMetric(l10n.t("序列数"), "\(dataset.trains.count)")
            detectorMetric(l10n.t("棘波数"), "\(dataset.totalSpikeCount)")
            detectorMetric(l10n.t("最小有效 ISI"), TimeFormatting.seconds(document.adaptiveDetectorBandSettings.minValidISISec))

            HStack(spacing: 8) {
                Text(l10n.t("直方图分箱"))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .fixedSize(horizontal: true, vertical: false)
                    .help(l10n.t("仅控制 ISI 分布汇总与自适应诊断。爆发种子频带由结构性 Burst I 锚点导出，而非来自默认直方图区间。"))
                DebouncedDoubleField(
                    "5",
                    value: histogramBinBinding,
                    width: 70
                )
                .help(l10n.t("诊断直方图分箱宽度（毫秒）。"))
                Text("ms")
                    .foregroundStyle(.secondary)
            }

            detectorMetric(l10n.t("不应期"), TimeFormatting.seconds(document.qualitySettings.refractorySuspectThresholdSec))

            Spacer(minLength: 18)

            if document.isDetectorRunning {
                ProgressView()
                    .controlSize(.small)
            }

            Text(document.detectorStatusMessage)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .truncationMode(.tail)
        }
        .padding(14)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private var histogramBinBinding: Binding<Double> {
        Binding(
            get: { document.detectorHistogramBinWidthMs },
            set: { value in
                document.detectorHistogramBinWidthMs = max(0.1, value)
            }
        )
    }

    /// Adaptive-v2 burst canonicalization toggle. The app default is ON after P6B stabilization; it binds to the
    /// document flag (which is part of `DetectionInputsSignature`, so flipping it shows the existing stale-results
    /// banner). It does NOT auto-run detection.
    private var experimentalControls: some View {
        VStack(alignment: .leading, spacing: 8) {
            sectionTitle(l10n.t("推荐功能"))
            Toggle(isOn: $document.useAdaptiveV2Canonicalization) {
                Text(l10n.t("Adaptive v2 爆发规范化（默认开启）"))
                    .font(.callout.weight(.medium))
            }
            .toggleStyle(.switch)
            Text(l10n.t("将结构上像爆发、但与该序列的爆发画像不兼容的候选降级为「可能爆发 / 复核」。不会删除候选，也不会覆盖手动语义标签。默认开启；如需与旧版逻辑比较，可暂时关闭。"))
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)
            Text(l10n.t("更改后不会自动重新检测；如已有结果，请重新运行检测以查看效果。"))
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)
        }
        .padding(14)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private var eventPatternControls: some View {
        VStack(alignment: .leading, spacing: 10) {
            sectionTitle(l10n.t("事件模式参数"))

            HStack(alignment: .top, spacing: 12) {
                parameterCard(title: l10n.t("经典爆发")) {
                    doubleParameter(l10n.t("对比度 I"), keyPath: \.detectorClassicBurstContrastMin, lower: 1, upper: 30)
                    doubleParameter(l10n.t("暂停侧翼"), keyPath: \.detectorClassicBurstFlankPauseContrastMin, lower: document.detectorClassicBurstContrastMin, upper: 50)
                    intParameter(l10n.t("最少棘波数"), keyPath: \.detectorClassicBurstMinSpikes, range: 3...100)
                    intParameter(l10n.t("最多棘波数"), keyPath: \.detectorClassicBurstMaxSpikes, range: 3...100)
                    Text(l10n.t("对比度 I 接受 Burst I。仅当边界 ISI 达到爆发内 q90 的相应倍数时，暂停侧翼才播种暂停。"))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }

                parameterCard(title: l10n.t("暂停")) {
                    doubleParameter(l10n.t("最小 ISI 毫秒"), keyPath: \.detectorPauseMinISIMs, lower: 0, upper: nil)
                    Text(l10n.t("0 = 自动。在结构性爆发侧翼与强直发放守卫计算完成后，手动值将覆盖按序列自适应的暂停下界。"))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }
            }
        }
    }

    private var statePatternControls: some View {
        VStack(alignment: .leading, spacing: 10) {
            sectionTitle(l10n.t("状态模式参数"))

            HStack(alignment: .top, spacing: 12) {
                parameterCard(title: l10n.t("强直发放")) {
                    intParameter(l10n.t("最少棘波数"), keyPath: \.detectorTonicMinSpikes, range: 3...500)
                    doubleParameter("CV max", keyPath: \.detectorTonicCVMax, lower: 0.01, upper: 5)
                    doubleParameter("CV2 max", keyPath: \.detectorTonicCV2Max, lower: 0.01, upper: 5)
                    doubleParameter("LV max", keyPath: \.detectorTonicLVMax, lower: 0.01, upper: 5)
                    doubleParameter(l10n.t("爆发占比"), keyPath: \.detectorTonicBurstSeedFractionMax, lower: 0, upper: 1)
                }

                parameterCard(title: l10n.t("高频强直发放")) {
                    intParameter(l10n.t("最少棘波数"), keyPath: \.detectorHighFrequencyTonicMinSpikes, range: 3...500)
                    doubleParameter(l10n.t("低尾占比"), keyPath: \.detectorHighFrequencyTonicLowTailFractionMax, lower: 0, upper: 1)
                    doubleParameter("CV max", keyPath: \.detectorHighFrequencyTonicCVMax, lower: 0.01, upper: 5)
                    doubleParameter("CV2 max", keyPath: \.detectorHighFrequencyTonicCV2Max, lower: 0.01, upper: 5)
                    doubleParameter("LV max", keyPath: \.detectorHighFrequencyTonicLVMax, lower: 0.01, upper: 5)
                }

                parameterCard(title: l10n.t("HFS")) {
                    intParameter(l10n.t("最少棘波数"), keyPath: \.detectorHighFrequencySpikingMinSpikes, range: 3...2000)
                    doubleParameter(l10n.t("短间隔占比"), keyPath: \.detectorHighFrequencySpikingShortFractionMin, lower: 0, upper: 1)
                    doubleParameter(l10n.t("大间隔占比"), keyPath: \.detectorHighFrequencySpikingAllowedLargeFraction, lower: 0, upper: 1)
                    intParameter(l10n.t("最大连续大间隔"), keyPath: \.detectorHighFrequencySpikingMaxConsecutiveLargeISI, range: 0...50)
                }
            }
        }
    }

    private var manualThresholdControls: some View {
        VStack(alignment: .leading, spacing: 10) {
            sectionTitle(l10n.t("手动阈值"))
            Text(l10n.t("可选的按家族 ISI / 棘波计数限制，针对每条序列依据自适应频带解析。自动 = 保持不变。硬门控仅收窄（爆发硬门控同时驱动 ISI 剖面爆发路径）；软锚点仅放宽，绝不强制候选项。数值单位为毫秒；每个已应用阈值都会记录在候选决策路径与 resolved_thresholds CSV 列中。"))
                .font(.caption)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)

            manualThresholdScopePicker

            HStack(alignment: .top, spacing: 12) {
                parameterCard(title: l10n.t("爆发")) {
                    modeParameter(family: .burst, selection: $document.manualBurstMode)
                    doubleParameter(burstSeedFieldLabel, keyPath: \.manualBurstSeedMaxISIMs, lower: 0, upper: nil)
                    doubleParameter(burstBridgeFieldLabel, keyPath: \.manualBurstBridgeMaxISIMs, lower: 0, upper: nil)
                    intParameter(l10n.t("最少棘波数"), keyPath: \.manualBurstMinSpikes, range: 0...500)
                    if document.manualBurstMode == .softAnchor {
                        burstSoftAnchorHelpNote
                    }
                }

                parameterCard(title: l10n.t("强直发放")) {
                    modeParameter(family: .tonic, selection: $document.manualTonicMode)
                    doubleParameter(l10n.t("最小 ISI 毫秒"), keyPath: \.manualTonicMinISIMs, lower: 0, upper: nil)
                    doubleParameter(l10n.t("最大 ISI 毫秒"), keyPath: \.manualTonicMaxISIMs, lower: 0, upper: nil)
                    intParameter(l10n.t("最少棘波数"), keyPath: \.manualTonicMinSpikes, range: 0...500)
                }

                parameterCard(title: l10n.t("高频强直发放")) {
                    modeParameter(family: .hfTonic, selection: $document.manualHFTonicMode)
                    doubleParameter(l10n.t("最小 ISI 毫秒"), keyPath: \.manualHFTonicMinISIMs, lower: 0, upper: nil)
                    doubleParameter(l10n.t("最大 ISI 毫秒"), keyPath: \.manualHFTonicMaxISIMs, lower: 0, upper: nil)
                    intParameter(l10n.t("最少棘波数"), keyPath: \.manualHFTonicMinSpikes, range: 0...500)
                }
            }

            HStack(alignment: .top, spacing: 12) {
                parameterCard(title: l10n.t("HFS")) {
                    modeParameter(family: .hfs, selection: $document.manualHFSMode)
                    intParameter(l10n.t("最少棘波数"), keyPath: \.manualHFSMinSpikes, range: 0...2000)
                    doubleParameter(l10n.t("最小时长毫秒"), keyPath: \.manualHFSMinDurationMs, lower: 0, upper: nil)
                }

                parameterCard(title: l10n.t("暂停")) {
                    modeParameter(family: .pause, selection: $document.manualPauseMode)
                    doubleParameter(l10n.t("最小 ISI 毫秒"), keyPath: \.manualPauseMinISIMs, lower: 0, upper: nil)
                }
            }

            learnedThresholdsSection
        }
    }

    // MARK: - Phase 1B: learned-from-annotations preview + explicit Apply (soft anchors only)

    @ViewBuilder
    private var learnedThresholdsSection: some View {
        let proposal = document.learnedThresholdProposal
        Divider().padding(.top, 2)
        VStack(alignment: .leading, spacing: 8) {
            Text(l10n.t("从手动标注学习（预览）"))
                .font(.callout.weight(.bold))
                .foregroundStyle(.secondary)

            if let proposal, !proposal.isAllAutomatic {
                // Req 1: preview does not affect detection until Apply; Apply writes Soft anchors; rerun manually.
                Text(l10n.t("预览仅供查看，不影响检测，直到点击下方“应用学习到的阈值”。应用后请手动重新运行检测。"))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
                // Req 2: Soft-anchor semantics, stated for ALL families (not just Burst).
                Text(l10n.t("学习值以软锚点写入：仅放宽 / 建议，不是上限，也不会排除高于或低于该值的 ISI。要按阈值收窄区间，请改用硬门控。"))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
                VStack(alignment: .leading, spacing: 4) {
                    ForEach(Array(document.learnedThresholdExplanations.enumerated()), id: \.offset) { _, explanation in
                        explanationRow(explanation)
                    }
                }
                if let skips = compactSkipNote(proposal.skipped) {
                    Text(skips)
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }
                HStack(alignment: .top, spacing: 10) {
                    Button(l10n.t("应用学习到的阈值")) { document.applyLearnedThresholds() }
                        .buttonStyle(.borderedProminent)
                    Text(l10n.t("不覆盖你已设为硬门控的家族。"))
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }
                if let result = document.lastLearnedApplyResult {
                    learnedApplyResultNote(result)
                }
            } else {
                Text(l10n.t("暂无可用的学习阈值——请添加更多手动标注（达到各家族的最少样本数）。"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
    }

    private func explanationRow(_ explanation: LearnedThresholdFieldExplanation) -> some View {
        HStack(spacing: 8) {
            Text(learnedFieldTitle(family: explanation.family, field: explanation.field))
                .frame(width: 120, alignment: .leading)
            Text(currentToLearnedText(explanation))
                .foregroundStyle(explanationTint(explanation))
            Spacer(minLength: 8)
            if let contribution = explanation.contribution {
                Text(contribution.evidenceSummary)
                    .foregroundStyle(.secondary)
            }
        }
        .font(.caption)
        .lineLimit(1)
    }

    private func explanationTint(_ explanation: LearnedThresholdFieldExplanation) -> Color {
        switch explanation.change {
        case .skippedHard: return .orange
        case .applied, .changed: return STPDAppTheme.accent
        case .noChange: return .secondary
        }
    }

    /// Compact "current → learned" copy for one proposed field.
    private func currentToLearnedText(_ explanation: LearnedThresholdFieldExplanation) -> String {
        let learned = String(format: "%@ %.0f ms", thresholdModeLabel(explanation.learnedMode ?? .softAnchor), explanation.learnedValueMs ?? 0)
        switch explanation.change {
        case .skippedHard:
            return l10n.t("跳过：当前为硬门控")
        case .noChange:
            return learned + " · " + l10n.t("无变化")
        case .applied:
            return thresholdModeLabel(.automatic) + " → " + learned
        case .changed:
            let current = explanation.currentValueMs
                .map { String(format: "%@ %.0f ms", thresholdModeLabel(explanation.currentMode), $0) }
                ?? thresholdModeLabel(explanation.currentMode)
            return current + " → " + learned
        }
    }

    private func learnedFieldTitle(family: String, field: String) -> String {
        switch (family, field) {
        case ("burst", "seed_upper_sec"): return l10n.t("爆发种子上界")
        case ("burst", "bridge_upper_sec"): return l10n.t("爆发桥接上界")
        case ("tonic", "isi_lower_sec"): return l10n.t("强直下界")
        case ("tonic", "isi_upper_sec"): return l10n.t("强直上界")
        case ("hf_tonic", "isi_floor_sec"): return l10n.t("高频强直下界")
        case ("hf_tonic", "isi_upper_sec"): return l10n.t("高频强直上界")
        case ("pause", "isi_lower_sec"): return l10n.t("暂停下界")
        default: return "\(family).\(field)"
        }
    }

    private func compactSkipNote(_ skips: [LearnedThresholdSkip]) -> String? {
        let insufficient = skips.filter { $0.reason == .notUsableMinCount }.map(\.sourceLabel)
        var parts: [String] = []
        if !insufficient.isEmpty {
            parts.append(l10n.t("样本不足：") + insufficient.joined(separator: ", "))
        }
        if skips.contains(where: { $0.reason == .noMappableField }) {
            parts.append(l10n.t("HFS 暂不学习"))
        }
        return parts.isEmpty ? nil : parts.joined(separator: " · ")
    }

    @ViewBuilder
    private func learnedApplyResultNote(_ result: LearnedThresholdApplyResult) -> some View {
        VStack(alignment: .leading, spacing: 2) {
            if !result.appliedFamilies.isEmpty {
                Text(l10n.t("已应用（软锚点）：") + result.appliedFamilies.joined(separator: ", ") + "。"
                     + l10n.t("重新运行检测以更新自动结果。"))
                    .font(.caption2)
                    .foregroundStyle(STPDAppTheme.accent)
                    .fixedSize(horizontal: false, vertical: true)
            }
            if !result.skippedHardFamilies.isEmpty {
                Text(l10n.t("跳过（已为硬门控）：") + result.skippedHardFamilies.joined(separator: ", "))
                    .font(.caption2)
                    .foregroundStyle(.orange)
                    .fixedSize(horizontal: false, vertical: true)
            }
            if !result.noValueFamilies.isEmpty {
                Text(l10n.t("无可用学习值：") + result.noValueFamilies.joined(separator: ", "))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
            }
            if !result.appliedProvenanceNotes.isEmpty {
                VStack(alignment: .leading, spacing: 1) {
                    ForEach(Array(result.appliedProvenanceNotes.enumerated()), id: \.offset) { _, note in
                        // Display-only label normalization (burst_family → burst); the machine note in
                        // decisionPath / CSV is unchanged.
                        Text(LearnedManualThresholdApplier.displayProvenanceNote(note))
                            .font(.system(.caption2, design: .monospaced))
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                }
            }
        }
    }

    // Task-A wording fix: in Soft mode the burst seed/bridge fields ANCHOR / widen the band — they do not cap it —
    // so they are labelled "anchor" rather than "max". In Hard mode "max" is a real cap, and in Auto the value is
    // inert, so both keep the existing "max" label. No threshold semantics change; this is wording only.
    private var burstSeedFieldLabel: String {
        document.manualBurstMode == .softAnchor ? l10n.t("种子锚点毫秒") : l10n.t("种子最大毫秒")
    }

    private var burstBridgeFieldLabel: String {
        document.manualBurstMode == .softAnchor ? l10n.t("桥接锚点毫秒") : l10n.t("桥接最大毫秒")
    }

    /// Soft-mode help: spells out that the value does not cap selection (only Hard does).
    private var burstSoftAnchorHelpNote: some View {
        Text(l10n.t("软：仅放宽——高于该值的 ISI 仍可能被选为爆发。改用硬模式可对高于该值的 ISI 封顶 / 排除。"))
            .font(.caption2)
            .foregroundStyle(.secondary)
            .fixedSize(horizontal: false, vertical: true)
    }

    private func modeParameter(family: ManualThresholdFamily, selection: Binding<ThresholdMode>) -> some View {
        let effect = manualModeEffectNote(selection.wrappedValue, family: family)
        return VStack(alignment: .leading, spacing: 5) {
            HStack(spacing: 8) {
                Text(l10n.t("模式"))
                    .foregroundStyle(.secondary)
                    .frame(width: 92, alignment: .leading)
                // Auto / Soft / Hard are always visible (segmented) so the active semantics are explicit, with
                // the selected mode in the brand accent.
                GlassSegmentedControl(
                    options: ThresholdMode.allCases.map { ($0, thresholdModeLabel($0)) },
                    selection: selection,
                    minSegmentWidth: 30
                )
                .fixedSize(horizontal: true, vertical: false)
            }
            // Live parameter-effect feedback: inactive (auto) / widening (soft) / narrowing (hard).
            Text(effect.text)
                .font(.caption2)
                .foregroundStyle(effect.tint)
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    /// Mode-derived parameter-effect feedback shown under each family's mode control. Reuses the
    /// `ManualThresholdResolver` / `ThresholdMode` semantics (automatic = adaptive only; soft = widen/anchor,
    /// never narrows; hard = narrow), including the PARAM-2 burst seed/bridge "admission ceiling" exception and
    /// the narrow-only caveat (a hard upper above the adaptive upper cannot expand detection).
    private func manualModeEffectNote(_ mode: ThresholdMode, family: ManualThresholdFamily) -> (text: String, tint: Color) {
        switch mode {
        case .automatic:
            return (l10n.t("自动：仅自适应，不改变检测。"), .secondary)
        case .softAnchor:
            return (l10n.t("软：放宽 / 锚定频带，绝不收窄或强制候选。"), STPDAppTheme.accent)
        case .hardGate:
            if family == .burst {
                return (l10n.t("硬：种子 / 桥接上限作为准入上限——可恢复中等 ISI，设得更低则收窄。"), .orange)
            }
            return (l10n.t("硬：收窄检测。若自适应上限已低于该硬上限，提高它不会扩大检测。"), .orange)
        }
    }

    private func thresholdModeLabel(_ mode: ThresholdMode) -> String {
        switch mode {
        case .automatic:
            return l10n.t("自动")
        case .softAnchor:
            return l10n.t("软")
        case .hardGate:
            return l10n.t("硬")
        }
    }

    private func parameterCard<Content: View>(
        title: String,
        @ViewBuilder content: () -> Content
    ) -> some View {
        VStack(alignment: .leading, spacing: 10) {
            Text(title)
                .font(.headline)
            VStack(alignment: .leading, spacing: 8) {
                content()
            }
        }
        .padding(14)
        .frame(width: 302, alignment: .topLeading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private func doubleParameter(
        _ label: String,
        keyPath: ReferenceWritableKeyPath<RasterDocument, Double>,
        lower: Double,
        upper: Double? = nil
    ) -> some View {
        HStack(spacing: 8) {
            Text(label)
                .foregroundStyle(.secondary)
                .frame(width: 92, alignment: .leading)
            DebouncedDoubleField(
                "0",
                value: boundedDoubleBinding(keyPath, lower: lower, upper: upper),
                width: 76,
                maxFractionDigits: 3
            )
        }
    }

    private func intParameter(
        _ label: String,
        keyPath: ReferenceWritableKeyPath<RasterDocument, Int>,
        range: ClosedRange<Int>
    ) -> some View {
        HStack(spacing: 8) {
            Text(label)
                .foregroundStyle(.secondary)
                .frame(width: 92, alignment: .leading)
            DebouncedIntField(
                "0",
                value: boundedIntBinding(keyPath, range: range),
                range: range,
                width: 76
            )
        }
    }

    private func boundedDoubleBinding(
        _ keyPath: ReferenceWritableKeyPath<RasterDocument, Double>,
        lower: Double,
        upper: Double?
    ) -> Binding<Double> {
        Binding(
            get: { document[keyPath: keyPath] },
            set: { value in
                let finite = value.isFinite ? value : lower
                let lowered = max(finite, lower)
                document[keyPath: keyPath] = upper.map { min(lowered, $0) } ?? lowered
            }
        )
    }

    private func boundedIntBinding(
        _ keyPath: ReferenceWritableKeyPath<RasterDocument, Int>,
        range: ClosedRange<Int>
    ) -> Binding<Int> {
        Binding(
            get: { document[keyPath: keyPath] },
            set: { value in
                document[keyPath: keyPath] = min(max(value, range.lowerBound), range.upperBound)
            }
        )
    }

    private func detectorMetric(_ title: String, _ value: String) -> some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(value)
                .font(.callout.weight(.semibold))
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
        }
    }

    private func summaryStrip(run: ClassicAnchorDetectionRun) -> some View {
        HStack(spacing: 10) {
            DetectorSummaryTile(title: l10n.t("候选项"), value: "\(run.candidateCount)")
            DetectorSummaryTile(title: l10n.t("已锁定"), value: "\(run.lockedClassicCount)", level: .good)
            DetectorSummaryTile(title: l10n.t("强"), value: "\(run.strongCandidateCount)", level: run.strongCandidateCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: l10n.t("爆发"), value: "\(run.burstCount)")
            DetectorSummaryTile(title: l10n.t("高频爆发"), value: "\(run.highFrequencyBurstCount)")
            DetectorSummaryTile(title: l10n.t("长爆发"), value: "\(run.longBurstCount)")
            DetectorSummaryTile(title: l10n.t("复审"), value: "\(run.selectedReviewCount)", level: run.selectedReviewCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: l10n.t("HFS"), value: "\(run.highFrequencySpikingCount)", level: run.highFrequencySpikingCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: l10n.t("高频强直发放"), value: "\(run.highFrequencyTonicCount)")
            DetectorSummaryTile(title: l10n.t("强直发放"), value: "\(run.tonicCount)")
            DetectorSummaryTile(title: l10n.t("暂停"), value: "\(run.pauseCount)")
            DetectorSummaryTile(title: l10n.t("自动"), value: "\(run.selectedAutoCount)", level: .good)
            DetectorSummaryTile(title: l10n.t("事件轨道"), value: "\(run.selectedEventCount)", level: .good)
            DetectorSummaryTile(title: l10n.t("间隙轨道"), value: "\(run.selectedGapCount)", level: run.selectedGapCount > 0 ? .warning : .plain)
            DetectorSummaryTile(title: l10n.t("状态轨道"), value: "\(run.selectedStateCount)", level: run.selectedStateCount > 0 ? .warning : .plain)
        }
    }

    private func adaptiveBandTable(run: ClassicAnchorDetectionRun) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            tableTitle(l10n.t("自适应爆发频带"))

            // Parameter-effect context for the "especially Burst" thresholds: when a manual burst max is active,
            // point the user at the per-train adaptive seed/bridge uppers in this table so they can see whether
            // their limit recovers moderate ISI (above adaptive) or constrains (below).
            if document.manualBurstMode != .automatic,
               document.manualBurstSeedMaxISIMs > 0 || document.manualBurstBridgeMaxISIMs > 0 {
                Text(l10n.t("手动爆发上限已启用：将你的种子 / 桥接上限与下方各序列的自适应种子 / 桥接上界对照——硬上限高于自适应上界可恢复中等 ISI，低于则收窄。"))
                    .font(.caption2)
                    .foregroundStyle(document.manualBurstMode == .hardGate ? .orange : STPDAppTheme.accent)
                    .fixedSize(horizontal: false, vertical: true)
            }

            VStack(alignment: .leading, spacing: 0) {
                HStack(spacing: 12) {
                    tableHeader(l10n.t("序列"), width: 280)
                    tableHeader(l10n.t("有效 ISI"), width: 72)
                    tableHeader(l10n.t("种子下界"), width: 108)
                    tableHeader(l10n.t("种子上界"), width: 108)
                    tableHeader(l10n.t("桥接"), width: 108)
                    tableHeader("S", width: 50)
                    tableHeader(l10n.t("来源"), width: 86)
                    tableHeader(l10n.t("候选项"), width: 82)
                    tableHeader(l10n.t("已锁定"), width: 70)
                }
                .padding(.horizontal, 8)
                .padding(.vertical, 7)

                Divider()

                ForEach(run.resolutions, id: \.trainID) { resolution in
                    let result = run.result(for: resolution.trainID)
                    adaptiveBandRow(resolution: resolution, result: result)
                }
            }
        }
    }

    private func adaptiveBandRow(
        resolution: TrainAdaptiveBandResolution,
        result: ClassicAnchorDetectionResult?
    ) -> some View {
        let burst = resolution.burstBand
        let candidates = result?.candidates ?? []
        let locked = candidates.filter { $0.anchorLockLevel == .lockedClassic }

        return HStack(spacing: 12) {
            Text(resolution.trainName)
                .lineLimit(1)
                .truncationMode(.middle)
                .frame(width: 280, alignment: .leading)
            tableCell("\(resolution.validISICount)", width: 72)
            tableCell(time(burst?.seedLowerSec), width: 108)
            tableCell(time(burst?.seedUpperSec), width: 108)
            tableCell(time(burst?.bridgeUpperSec), width: 108)
            tableCell(number(burst?.contrastS), width: 50)
            tableCell(burst?.primarySource.rawValue ?? "none", width: 86)
            tableCell("\(candidates.count)", width: 82)
            tableCell("\(locked.count)", width: 70)
        }
        .font(.callout)
        .padding(.horizontal, 8)
        .padding(.vertical, 6)
        .background(rowFill(level: locked.isEmpty ? .plain : .good), in: RoundedRectangle(cornerRadius: 7))
    }

    private func candidateAuditTable(run: ClassicAnchorDetectionRun) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            tableTitle(l10n.t("候选项审计"))

            LazyVStack(alignment: .leading, spacing: 0) {
                HStack(spacing: 12) {
                    tableHeader(l10n.t("状态"), width: 96)
                    tableHeader(l10n.t("标签"), width: 104)
                    tableHeader(l10n.t("序列"), width: 250)
                    tableHeader(l10n.t("棘波数"), width: 60)
                    tableHeader(l10n.t("起始 ISI"), width: 72)
                    tableHeader(l10n.t("结束 ISI"), width: 72)
                    tableHeader(l10n.t("时长"), width: 92)
                    tableHeader(l10n.t("频带"), width: 138)
                    tableHeader(l10n.t("边缘对比度"), width: 104)
                    tableHeader(l10n.t("评分"), width: 66)
                    tableHeader(l10n.t("原因"), width: 280)
                }
                .padding(.horizontal, 8)
                .padding(.vertical, 7)

                Divider()

                ForEach(candidateRows(run: run)) { candidate in
                    candidateRow(candidate)
                }
            }
        }
    }

    private func candidateRows(run: ClassicAnchorDetectionRun) -> [ClassicAnchorCandidate] {
        run.candidates.sorted { lhs, rhs in
            if lhs.anchorLockLevel != rhs.anchorLockLevel {
                return lockSortRank(lhs.anchorLockLevel) < lockSortRank(rhs.anchorLockLevel)
            }
            if lhs.priority != rhs.priority {
                return lhs.priority > rhs.priority
            }
            return lhs.score > rhs.score
        }
    }

    private func candidateRow(_ candidate: ClassicAnchorCandidate) -> some View {
        let lockTint = lockColor(candidate.anchorLockLevel)
        return HStack(spacing: 12) {
            Text(lockLabel(candidate.anchorLockLevel))
                .font(.caption.weight(.semibold))
                .foregroundStyle(lockTint)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(
                    lockTint.opacity(candidate.anchorLockLevel == .auditOnly ? 0.08 : 0.12),
                    in: Capsule()
                )
                .frame(width: 96, alignment: .leading)
            tableCell(candidate.finalLabel.rawValue, width: 104)
            Text(candidate.trainName)
                .lineLimit(1)
                .truncationMode(.middle)
                .frame(width: 250, alignment: .leading)
            tableCell("\(candidate.nSpikes)", width: 60)
            tableCell("\(candidate.startISIIndex)", width: 72)
            tableCell("\(candidate.endISIIndex)", width: 72)
            tableCell(time(candidate.durationSec), width: 92)
            tableCell("\(time(candidate.anchorBandLowerSec)) - \(time(candidate.anchorBandUpperSec))", width: 138)
            tableCell(number(candidate.edgeContrastMinQ90), width: 104)
            tableCell(number(candidate.score), width: 66)
            tableCell(candidateShortReason(candidate), width: 280)
        }
        .font(.callout)
        .padding(.horizontal, 8)
        .padding(.vertical, 6)
        .background(rowFill(level: rowLevel(candidate.anchorLockLevel)), in: RoundedRectangle(cornerRadius: 7))
    }

    private func lockSortRank(_ lock: ClassicAnchorLockLevel) -> Int {
        switch lock {
        case .lockedClassic:
            return 0
        case .strongCandidate:
            return 1
        case .auditOnly:
            return 2
        }
    }

    private func lockLabel(_ lock: ClassicAnchorLockLevel) -> String {
        switch lock {
        case .lockedClassic:
            return l10n.t("已锁定")
        case .strongCandidate:
            return l10n.t("强")
        case .auditOnly:
            return l10n.t("审计")
        }
    }

    private func lockColor(_ lock: ClassicAnchorLockLevel) -> Color {
        switch lock {
        case .lockedClassic:
            return .green
        case .strongCandidate:
            return .orange
        case .auditOnly:
            return .secondary
        }
    }

    private func candidateShortReason(_ candidate: ClassicAnchorCandidate) -> String {
        if !candidate.reviewEvidenceSummary.isEmpty {
            return candidate.reviewEvidenceSummary
        }
        if !candidate.possibleBurstStructureSummary.isEmpty {
            return candidate.possibleBurstStructureSummary
        }
        return candidate.auditReasonSummary
    }

    private func rowLevel(_ lock: ClassicAnchorLockLevel) -> RowLevel {
        switch lock {
        case .lockedClassic:
            return .good
        case .strongCandidate:
            return .warning
        case .auditOnly:
            return .plain
        }
    }

    private func tableTitle(_ title: String) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Text(title)
                .font(.headline)
            if let date = document.detectorLastRunDate {
                Text(date.formatted(date: .omitted, time: .standard))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
    }

    private func sectionTitle(_ title: String) -> some View {
        Text(title)
            .font(.headline)
    }

    // P8/P9: the manual hard-threshold SCOPE picker — makes explicit which trains the manual hard thresholds target, so a
    // hard gate is never silently applied to every train. Soft anchors / auto modes apply to all trains regardless. As of
    // P9 the detector enforces this per train (a hard gate applies only to in-scope trains; out-of-scope trains treat it
    // as automatic); changing the scope marks results stale, so re-run detection to apply it.
    private var manualThresholdScopePicker: some View {
        let scope = document.manualThresholdScope
        let total = document.dataset?.trains.count ?? 0
        return VStack(alignment: .leading, spacing: 4) {
            HStack(alignment: .firstTextBaseline, spacing: 8) {
                Text(l10n.t("应用范围"))
                    .foregroundStyle(.secondary)
                    .frame(width: 92, alignment: .leading)
                GlassSegmentedControl(
                    options: ManualThresholdScopeKind.allCases.map { ($0, manualThresholdScopeLabel($0)) },
                    selection: $document.manualThresholdScopeKind,
                    minSegmentWidth: 50
                )
                .fixedSize(horizontal: true, vertical: false)
                Spacer(minLength: 8)
                Text("\(l10n.t("作用序列")) \(scope.resolvedCount(allTrainCount: total))/\(total)")
                    .font(.caption)
                    .foregroundStyle(scope.targetsNoTrain ? .orange : .secondary)
                    .monospacedDigit()
            }
            Text(l10n.t("硬门控仅作用于所选范围；软锚点与自动模式始终对全部序列生效。范围若未匹配任何序列，硬门控当前不影响任何序列。"))
                .font(.caption2)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: false, vertical: true)
            if scope.targetsNoTrain {
                Text(l10n.t("当前范围未匹配任何序列：请聚焦一条序列，或在光栅图中选择可见序列。"))
                    .font(.caption2)
                    .foregroundStyle(.orange)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
    }

    private func manualThresholdScopeLabel(_ kind: ManualThresholdScopeKind) -> String {
        switch kind {
        case .allTrains: return l10n.t("全部序列")
        case .currentTrain: return l10n.t("当前序列")
        case .selectedTrains: return l10n.t("所选序列")
        }
    }

    private func tableHeader(_ title: String, width: CGFloat) -> some View {
        Text(title)
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
            .frame(width: width, alignment: .leading)
    }

    private func tableCell(_ value: String, width: CGFloat) -> some View {
        Text(value)
            .lineLimit(1)
            .truncationMode(.middle)
            .frame(width: width, alignment: .leading)
    }

    private func time(_ value: Double?) -> String {
        guard let value else {
            return "NA"
        }
        return TimeFormatting.seconds(value)
    }

    private func number(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        if abs(value) >= 100 {
            return String(format: "%.1f", value)
        }
        if abs(value) >= 10 {
            return String(format: "%.2f", value)
        }
        return String(format: "%.3f", value)
    }

    private enum RowLevel {
        case plain
        case good
        case warning
    }

    private func rowFill(level: RowLevel) -> Color {
        switch level {
        case .plain:
            return Color.clear
        case .good:
            return Color.green.opacity(0.055)
        case .warning:
            return Color.orange.opacity(0.070)
        }
    }
}

/// Manual-threshold families, used to pick the family-specific parameter-effect microcopy (the burst seed/bridge
/// uppers behave as an admission ceiling, unlike the narrow-only uppers of the other families).
private enum ManualThresholdFamily {
    case burst, tonic, hfTonic, hfs, pause
}

private struct DetectorSummaryTile: View {
    let title: String
    let value: String
    var level: Level = .plain

    enum Level {
        case plain
        case good
        case warning
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(value)
                .font(.title3.weight(.semibold))
        }
        .padding(14)
        .frame(minWidth: 132, maxWidth: .infinity, alignment: .leading)
        .background(fill, in: RoundedRectangle(cornerRadius: 8))
    }

    private var fill: Color {
        switch level {
        case .plain:
            return Color(nsColor: .textBackgroundColor).opacity(0.62)
        case .good:
            return Color.green.opacity(0.10)
        case .warning:
            return Color.orange.opacity(0.12)
        }
    }
}
