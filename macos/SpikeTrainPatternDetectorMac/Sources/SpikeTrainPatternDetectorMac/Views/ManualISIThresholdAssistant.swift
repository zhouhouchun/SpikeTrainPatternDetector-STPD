import STPDCore
import SwiftUI

/// Explicit, detector-independent threshold helper. Editing the fields updates only a preview;
/// the document changes only after the reviewer presses Apply.
struct ManualISIThresholdAssistant: View {
    @Environment(\.l10n) private var l10n

    let rows: [ManualISIGraphicRow]
    let minimumValidISISeconds: Double
    @Binding var selectedRowIDs: Set<String>
    let canUndo: Bool
    let onApply: (ManualAnnotationLabel, Set<Int>) -> Bool
    let onUndo: () -> Void

    @State private var isExpanded = false
    @State private var pattern: ManualISIThresholdPattern = .burst
    @State private var minimumISIMilliseconds = ""
    @State private var maximumISIMilliseconds = ""
    @State private var burstMinimumSpikes = "3"
    @State private var burstMaximumSpikes = "15"
    @State private var tonicMinimumSpikes = "6"
    @State private var tonicMaximumSpikes = ""
    @State private var tonicMetric: ManualISITonicMetric = .cv2
    @State private var tonicMetricMinimum = "0"
    @State private var tonicMetricMaximum = "0.30"
    @State private var applicationFeedback: ApplicationFeedback?

    var body: some View {
        DisclosureGroup(isExpanded: $isExpanded) {
            VStack(alignment: .leading, spacing: 10) {
                ruleFields
                resultStrip
            }
            .padding(.top, 8)
        } label: {
            HStack(spacing: 9) {
                Text(l10n.t("ISI 阈值初标"))
                    .font(.headline)
                Text(l10n.t("人工规则 · 即时预览 · 不运行检测器"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
            }
        }
        .padding(11)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 10))
    }

    private var ruleFields: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 10) {
                Text(l10n.t("模式"))
                    .foregroundStyle(.secondary)
                    .frame(width: 82, alignment: .leading)
                Picker(l10n.t("模式"), selection: $pattern) {
                    Text(l10n.t("Burst")).tag(ManualISIThresholdPattern.burst)
                    Text(l10n.t("Pause")).tag(ManualISIThresholdPattern.pause)
                    Text(l10n.t("Tonic")).tag(ManualISIThresholdPattern.tonic)
                }
                .pickerStyle(.segmented)
                .labelsHidden()
                .frame(width: 240, alignment: .leading)

                rangeFields(
                    title: l10n.t("ISI 闭区间"),
                    lower: $minimumISIMilliseconds,
                    upper: $maximumISIMilliseconds,
                    unit: "ms"
                )

                if pattern == .burst {
                    integerRangeFields(
                        title: l10n.t("Spike 数闭区间"),
                        lower: $burstMinimumSpikes,
                        upper: $burstMaximumSpikes
                    )
                }

                Spacer(minLength: 0)
            }

            if pattern == .burst {
                Text(l10n.t("连续 n 个 ISI 对应 n+1 个 spike；超出 spike 上限的整段会被跳过，不会被切成人工 Burst 小包。"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            if pattern == .tonic {
                HStack(spacing: 10) {
                    Text(l10n.t("指标"))
                        .foregroundStyle(.secondary)
                        .frame(width: 82, alignment: .leading)
                    Picker(l10n.t("指标"), selection: $tonicMetric) {
                        ForEach(ManualISITonicMetric.allCases, id: \.self) {
                            Text($0.displayName).tag($0)
                        }
                    }
                    .labelsHidden()
                    .frame(width: 84, alignment: .leading)
                    rangeFields(
                        title: l10n.t("指标闭区间"),
                        lower: $tonicMetricMinimum,
                        upper: $tonicMetricMaximum,
                        unit: ""
                    )
                    integerRangeFields(
                        title: l10n.t("Spike 数"),
                        lower: $tonicMinimumSpikes,
                        upper: $tonicMaximumSpikes,
                        upperPlaceholder: l10n.t("不限"),
                        upperWidth: 112
                    )
                    Spacer(minLength: 0)
                }

                Text(l10n.t(tonicMetric == .mm
                    ? "MM = 候选段内最大 ISI / 最小 ISI；仅用于 3–5 个 spike 的短 Tonic 初标。"
                    : "CV/CV2/LV 仅用于至少 5 个 ISI（6 个 spike）的 Tonic 初标；3–5 个 spike 请改用 MM。"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
        .onChange(of: tonicMetric) { _, metric in
            switch metric {
            case .mm:
                tonicMinimumSpikes = "3"
                tonicMaximumSpikes = "5"
                tonicMetricMinimum = "1"
                tonicMetricMaximum = ""
            case .cv, .cv2, .lv:
                tonicMinimumSpikes = "6"
                tonicMaximumSpikes = ""
                tonicMetricMinimum = "0"
                tonicMetricMaximum = "0.30"
            }
        }
    }

    @ViewBuilder
    private var resultStrip: some View {
        let result = proposalResult
        VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 10) {
                if let message = result.errorMessage {
                    Text(message)
                        .font(.callout)
                        .foregroundStyle(.orange)
                } else {
                    Text(proposalSummary(result))
                        .font(.callout.monospacedDigit())
                    if result.blockedCandidateCount > 0 {
                        Text(blockedSummary(result.blockedCandidateCount))
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                    if pattern == .tonic,
                       let values = result.metricValueRange {
                        Text("\(tonicMetric.displayName) \(format(values.lowerBound))–\(format(values.upperBound))")
                            .font(.caption.monospacedDigit())
                            .foregroundStyle(.secondary)
                    }
                }
                Spacer(minLength: 0)
            }

            HStack(spacing: 10) {
                Button(l10n.t("应用初标")) {
                    let candidateCount = result.eligibleCandidates.count
                    let isiCount = result.eligibleISIIndices.count
                    // Keep the exact applied support visibly selected in the time plot. This also
                    // gives the reviewer an immediate geometry check after the colored pattern
                    // lane updates from the document's authoritative manual-label source.
                    selectedRowIDs = result.eligibleRowIDs
                    if onApply(pattern.manualLabel, result.eligibleISIIndices) {
                        applicationFeedback = .success(
                            candidateCount: candidateCount,
                            isiCount: isiCount
                        )
                    } else {
                        applicationFeedback = .failure
                    }
                }
                .buttonStyle(.borderedProminent)
                .disabled(result.eligibleISIIndices.isEmpty)

                Button(l10n.t("选中预览")) {
                    applicationFeedback = nil
                    selectedRowIDs = result.eligibleRowIDs
                }
                .disabled(result.eligibleRowIDs.isEmpty)

                Button(l10n.t("撤销上一批"), action: onUndo)
                    .disabled(!canUndo)

                if let applicationFeedback {
                    Label(
                        feedbackText(applicationFeedback),
                        systemImage: applicationFeedback.isSuccess
                            ? "checkmark.circle.fill" : "exclamationmark.triangle.fill"
                    )
                    .font(.caption)
                    .foregroundStyle(applicationFeedback.isSuccess ? Color.green : Color.orange)
                    .lineLimit(2)
                    .fixedSize(horizontal: false, vertical: true)
                }

                Spacer(minLength: 0)
            }
        }
    }

    private func rangeFields(
        title: String,
        lower: Binding<String>,
        upper: Binding<String>,
        unit: String
    ) -> some View {
        HStack(spacing: 5) {
            Text(title)
                .foregroundStyle(.secondary)
                .lineLimit(1)
            compactField(l10n.t("下限"), text: lower)
            Text("…").foregroundStyle(.secondary)
            compactField(l10n.t("上限"), text: upper)
            if !unit.isEmpty { Text(unit).foregroundStyle(.secondary) }
        }
    }

    private func integerRangeFields(
        title: String,
        lower: Binding<String>,
        upper: Binding<String>,
        upperPlaceholder: String = "上限",
        upperWidth: CGFloat = 50
    ) -> some View {
        HStack(spacing: 5) {
            Text(title)
                .foregroundStyle(.secondary)
                .lineLimit(1)
            compactField(l10n.t("下限"), text: lower, width: 50)
            Text("…").foregroundStyle(.secondary)
            compactField(upperPlaceholder, text: upper, width: upperWidth)
        }
    }

    private func compactField(
        _ placeholder: String,
        text: Binding<String>,
        width: CGFloat = 66
    ) -> some View {
        TextField(placeholder, text: text)
            .textFieldStyle(.roundedBorder)
            .multilineTextAlignment(.leading)
            .frame(width: width)
    }

    private var proposalResult: ManualISIThresholdProposalResult {
        guard let lowerMS = nonnegativeDouble(minimumISIMilliseconds),
              let upperMS = nonnegativeDouble(maximumISIMilliseconds),
              lowerMS <= upperMS else {
            return .error(l10n.t("请输入有效的 ISI 闭区间上下限。"))
        }

        let minimumSpikes: Int
        let maximumSpikes: Int?
        switch pattern {
        case .burst:
            guard let minimum = positiveInt(burstMinimumSpikes),
                  let maximum = positiveInt(burstMaximumSpikes),
                  minimum <= maximum else {
                return .error(l10n.t("请输入有效的 Burst spike 数闭区间。"))
            }
            minimumSpikes = minimum
            maximumSpikes = maximum
        case .pause:
            minimumSpikes = 2
            maximumSpikes = 2
        case .tonic:
            guard let minimum = positiveInt(tonicMinimumSpikes) else {
                return .error(l10n.t("请输入有效的 Tonic spike 数和指标闭区间。"))
            }
            let maximum = tonicMaximumSpikes.trimmingCharacters(in: .whitespacesAndNewlines)
                .isEmpty ? nil : positiveInt(tonicMaximumSpikes)
            let validSpikeRange: Bool
            switch tonicMetric {
            case .mm:
                validSpikeRange = minimum >= 3 && maximum.map { $0 >= minimum && $0 <= 5 } == true
            case .cv, .cv2, .lv:
                validSpikeRange = minimum >= 6 && (maximum.map { $0 >= minimum } ?? true)
            }
            guard validSpikeRange,
                  let metricLower = nonnegativeDouble(tonicMetricMinimum),
                  let metricUpper = nonnegativeDouble(tonicMetricMaximum),
                  tonicMetric != .mm || metricLower >= 1,
                  metricLower <= metricUpper else {
                return .error(tonicMetric == .mm
                    ? l10n.t("MM Tonic 初标仅支持 3–5 个 spike，且 MM 闭区间不能小于 1。")
                    : l10n.t("基于 CV/CV2/LV 的 Tonic 初标至少需要 6 个 spike。"))
            }
            minimumSpikes = minimum
            maximumSpikes = maximum
        }

        let metricRange: ManualISIThresholdClosedRange?
        if pattern == .tonic,
           let metricLower = nonnegativeDouble(tonicMetricMinimum),
           let metricUpper = nonnegativeDouble(tonicMetricMaximum) {
            metricRange = ManualISIThresholdClosedRange(
                lowerBound: metricLower,
                upperBound: metricUpper
            )
        } else {
            metricRange = nil
        }

        let rule = ManualISIThresholdRule(
            pattern: pattern,
            isiRangeSeconds: ManualISIThresholdClosedRange(
                lowerBound: lowerMS / 1_000,
                upperBound: upperMS / 1_000
            ),
            minimumSpikeCount: minimumSpikes,
            maximumSpikeCount: maximumSpikes,
            tonicMetric: pattern == .tonic ? tonicMetric : nil,
            tonicMetricRange: metricRange
        )
        let samples = rows.map {
            ManualISIThresholdSample(
                isiIndex: $0.isiIndex,
                isiSeconds: $0.rightSeconds - $0.leftSeconds
            )
        }

        do {
            let candidates = try ManualISIThresholdMarker.propose(
                samples: samples,
                rule: rule,
                minimumValidISISeconds: minimumValidISISeconds
            )
            return ManualISIThresholdProposalResult(
                candidates: candidates,
                rows: rows,
                label: pattern.manualLabel
            )
        } catch {
            return .error(error.localizedDescription)
        }
    }

    private func nonnegativeDouble(_ raw: String) -> Double? {
        let normalized = raw
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .replacingOccurrences(of: ",", with: ".")
        guard let value = Double(normalized), value.isFinite, value >= 0 else { return nil }
        return value
    }

    private func positiveInt(_ raw: String) -> Int? {
        guard let value = Int(raw.trimmingCharacters(in: .whitespacesAndNewlines)),
              value > 0 else { return nil }
        return value
    }

    private func format(_ value: Double) -> String {
        String(format: "%.3f", value)
    }

    private func proposalSummary(_ result: ManualISIThresholdProposalResult) -> String {
        switch l10n.language {
        case .zh:
            "可初标 \(result.eligibleCandidates.count) 段、\(result.eligibleISIIndices.count) 个 ISI"
        case .en:
            "\(result.eligibleCandidates.count) candidate(s), \(result.eligibleISIIndices.count) ISI(s) available for preliminary labeling"
        case .ru:
            "Для предварительной разметки: \(result.eligibleCandidates.count) сегмент(а), \(result.eligibleISIIndices.count) ISI"
        }
    }

    private func blockedSummary(_ count: Int) -> String {
        switch l10n.language {
        case .zh: "另有 \(count) 段因已有同轨标记或 Other 而跳过"
        case .en: "\(count) candidate(s) skipped because the same track or Other is already labeled"
        case .ru: "Пропущено \(count) сегмент(а): уже есть метка на той же дорожке или Other"
        }
    }

    private func feedbackText(_ feedback: ApplicationFeedback) -> String {
        switch feedback {
        case let .success(candidateCount, isiCount):
            return switch l10n.language {
            case .zh: "已写入 \(candidateCount) 段、\(isiCount) 个 ISI；时间图已选中并显示对应模式色带。"
            case .en: "Applied \(candidateCount) segment(s) and \(isiCount) ISI(s); the timeline now selects them and shows the corresponding pattern bands."
            case .ru: "Записано сегментов: \(candidateCount), ISI: \(isiCount); на шкале времени они выбраны и показаны цветными полосами паттерна."
            }
        case .failure:
            return l10n.t("初标未写入；请查看状态信息，确认当前 spike train、最小 spike 数与已有标记。")
        }
    }
}

private enum ApplicationFeedback: Equatable {
    case success(candidateCount: Int, isiCount: Int)
    case failure

    var isSuccess: Bool {
        if case .success = self { return true }
        return false
    }
}

struct ManualISIThresholdProposalResult {
    let eligibleCandidates: [ManualISIThresholdCandidate]
    let blockedCandidateCount: Int
    let eligibleISIIndices: Set<Int>
    let eligibleRowIDs: Set<String>
    let errorMessage: String?

    init(
        candidates: [ManualISIThresholdCandidate],
        rows: [ManualISIGraphicRow],
        label: ManualAnnotationLabel
    ) {
        let rowsByIndex = Dictionary(uniqueKeysWithValues: rows.map { ($0.isiIndex, $0) })
        eligibleCandidates = candidates.filter { candidate in
            candidate.isiIndices.allSatisfy { index in
                guard let row = rowsByIndex[index], row.otherLabel == nil else { return false }
                switch label.semanticTrack {
                case .state: return row.stateLabel == nil
                case .event: return row.eventLabel == nil
                case .other: return row.stateLabel == nil && row.eventLabel == nil && row.otherLabel == nil
                }
            }
        }
        blockedCandidateCount = candidates.count - eligibleCandidates.count
        eligibleISIIndices = Set(eligibleCandidates.flatMap(\.isiIndices))
        eligibleRowIDs = Set(eligibleISIIndices.compactMap { rowsByIndex[$0]?.id })
        errorMessage = nil
    }

    static func error(_ message: String) -> ManualISIThresholdProposalResult {
        ManualISIThresholdProposalResult(
            eligibleCandidates: [],
            blockedCandidateCount: 0,
            eligibleISIIndices: [],
            eligibleRowIDs: [],
            errorMessage: message
        )
    }

    private init(
        eligibleCandidates: [ManualISIThresholdCandidate],
        blockedCandidateCount: Int,
        eligibleISIIndices: Set<Int>,
        eligibleRowIDs: Set<String>,
        errorMessage: String?
    ) {
        self.eligibleCandidates = eligibleCandidates
        self.blockedCandidateCount = blockedCandidateCount
        self.eligibleISIIndices = eligibleISIIndices
        self.eligibleRowIDs = eligibleRowIDs
        self.errorMessage = errorMessage
    }

    var metricValueRange: ClosedRange<Double>? {
        let values = eligibleCandidates.compactMap(\.regularityValue)
        guard let lower = values.min(), let upper = values.max() else { return nil }
        return lower...upper
    }
}

struct ManualISIQCNotice: View {
    @Environment(\.l10n) private var l10n

    let rows: [ManualISIGraphicRow]
    @Binding var thresholdMilliseconds: Double

    private var invalidCount: Int { rows.lazy.filter(\.isInvalidISI).count }

    var body: some View {
        HStack(alignment: .top, spacing: 10) {
            Image(systemName: invalidCount == 0 ? "checkmark.shield" : "exclamationmark.shield.fill")
                .foregroundStyle(invalidCount == 0 ? Color.green : Color.red)
            VStack(alignment: .leading, spacing: 3) {
                Text(l10n.t("手工标记 QC"))
                    .font(.callout.weight(.semibold))
                Text(qcSummary)
                    .font(.callout)
                Text(l10n.t("仅凭时间戳无法判定两端哪一个 spike 错误；红色表示参与可疑间隔，不是单个 spike 的伪迹定论。阈值初标会排除这些 ISI。"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            Spacer()
            Text(l10n.t("绝对无效 ISI 上限"))
                .foregroundStyle(.secondary)
            DebouncedDoubleField(
                "0",
                value: Binding(
                    get: { thresholdMilliseconds },
                    set: { thresholdMilliseconds = max(0, $0) }
                ),
                width: 78
            )
            Text("ms").foregroundStyle(.secondary)
        }
        .padding(10)
        .background(
            (invalidCount == 0 ? Color.green : Color.red).opacity(0.07),
            in: RoundedRectangle(cornerRadius: 10)
        )
        .overlay {
            RoundedRectangle(cornerRadius: 10)
                .stroke((invalidCount == 0 ? Color.green : Color.red).opacity(0.28), lineWidth: 0.8)
        }
    }

    private var qcSummary: String {
        if invalidCount == 0 {
            return l10n.t("当前 spike train 未发现绝对无效 ISI。")
        }
        switch l10n.language {
        case .zh:
            return "发现 \(invalidCount) 个绝对无效 ISI；时间图以红色标出该间隔及其两端相关 spike。"
        case .en:
            return "Found \(invalidCount) absolutely invalid ISI(s); the timeline marks each interval and both involved endpoint spikes in red."
        case .ru:
            return "Обнаружено абсолютно недопустимых ISI: \(invalidCount); на шкале времени интервал и оба его конечных spike отмечены красным."
        }
    }
}

private extension ManualISIThresholdPattern {
    var manualLabel: ManualAnnotationLabel {
        switch self {
        case .burst: .burst
        case .pause: .pause
        case .tonic: .tonic
        }
    }
}

private extension ManualISITonicMetric {
    var displayName: String {
        self == .mm ? "MM (max/min)" : rawValue.uppercased()
    }
}
