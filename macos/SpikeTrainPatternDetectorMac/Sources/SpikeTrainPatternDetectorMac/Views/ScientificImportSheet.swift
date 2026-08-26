import Foundation
import STPDCore
import STPDTabularIO
import SwiftUI

private enum ScientificImportReviewStep: String, CaseIterable, Identifiable {
    case source
    case datasetMeaning
    case columnsAndGroups
    case eventAttributes
    case review

    var id: Self { self }

    var title: String {
        switch self {
        case .source: "数据源"
        case .datasetMeaning: "数据集含义"
        case .columnsAndGroups: "列与分组"
        case .eventAttributes: "事件与属性"
        case .review: "审核"
        }
    }

    var systemImage: String {
        switch self {
        case .source: "doc.badge.gearshape"
        case .datasetMeaning: "waveform.path.ecg"
        case .columnsAndGroups: "rectangle.3.group"
        case .eventAttributes: "tag"
        case .review: "checklist"
        }
    }
}

struct ScientificImportSheet: View {
    @Bindable var coordinator: ScientificImportCoordinator
    let onClose: () -> Void
    let onProceedToManualAnalysis: () -> Void

    @Environment(\.l10n) private var l10n

    @State private var selectedStep: ScientificImportReviewStep = .source
    @State private var selectedAttributeIDs: Set<UUID> = []
    @State private var originCandidatePage = 0
    @State private var isConfirmingSingleSpikeGroupTemplate = false

    var body: some View {
        NavigationSplitView {
            List(ScientificImportReviewStep.allCases, selection: $selectedStep) { step in
                Label(step.title, systemImage: step.systemImage)
                    .tag(step)
                    .foregroundStyle(isStepAvailable(step) ? .primary : .tertiary)
            }
            .navigationSplitViewColumnWidth(min: 190, ideal: 220, max: 260)
        } detail: {
            VStack(spacing: 0) {
                detail
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                Divider()
                footer
            }
        }
        .frame(minWidth: 1_080, idealWidth: 1_240, minHeight: 720, idealHeight: 800)
        .interactiveDismissDisabled(coordinator.isBusy)
        .delayedBackgroundProgress(scientificImportProgressMessage)
        .onChange(of: coordinator.phase) { _, newPhase in
            if newPhase == .reviewingScientificMeaning && selectedStep == .source {
                selectedStep = .datasetMeaning
            }
        }
    }

    private var scientificImportProgressMessage: String? {
        let source: String?
        if coordinator.isPersisting {
            source = "正在保存或恢复并校验科学导入合同…"
        } else {
            switch coordinator.phase {
            case .readingSource:
                source = "正在读取并限定数据源快照…"
            case .stagingSource:
                source = "正在解析表格并构建导入预备数据…"
            case .preflightingSourceFacts:
                source = "正在检查数据源事实与事件属性…"
            case .validatingScientificMeaning:
                source = "正在验证科学含义与规范数据身份…"
            default:
                source = nil
            }
        }
        return source.map(l10n.t)
    }

    @ViewBuilder
    private var detail: some View {
        switch selectedStep {
        case .source:
            sourceStep
        case .datasetMeaning:
            if coordinator.stagedImport != nil {
                datasetMeaningStep
            } else {
                unavailableStageB
            }
        case .columnsAndGroups:
            if coordinator.stagedImport != nil {
                columnsAndGroupsStep
            } else {
                unavailableStageB
            }
        case .eventAttributes:
            if coordinator.stagedImport != nil {
                eventAttributesStep
            } else {
                unavailableStageB
            }
        case .review:
            if coordinator.stagedImport != nil {
                reviewStep
            } else {
                unavailableStageB
            }
        }
    }

    private var sourceStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                stepHeader(
                    "绑定数据源",
                    subtitle: "阶段 A 只记录确切文件、工作表和表头规则，不作任何科学假设。"
                )

                if coordinator.phase == .readingSource {
                    ProgressView("正在读取受限的数据源快照…")
                        .controlSize(.large)
                        .frame(maxWidth: .infinity, minHeight: 260)
                } else if let source = coordinator.source {
                    sourceIdentityCard(source)
                    sourceDecisionCard(source)

                    savedManifestsSection

                    if let error = coordinator.failureMessage {
                        errorBanner(error)
                    }

                    if let staged = coordinator.stagedImport {
                        sourcePreview(staged)
                    }
                } else if let error = coordinator.failureMessage {
                    errorBanner(error)
                    ContentUnavailableView(
                        "数据源尚未绑定",
                        systemImage: "doc.badge.exclamationmark",
                        description: Text("请关闭此审核并选择受支持的 CSV 或 XLSX 文件。")
                    )
                    .frame(maxWidth: .infinity, minHeight: 260)
                } else {
                    ContentUnavailableView(
                        "尚未选择数据源",
                        systemImage: "doc",
                        description: Text("请从工具栏或“文件”菜单选择“导入数据”。")
                    )
                    .frame(maxWidth: .infinity, minHeight: 260)
                }
            }
            .padding(24)
        }
    }

    private var datasetMeaningStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                stepHeader(
                    "确认数据集含义",
                    subtitle: "这些决定适用于整个文件；不会根据表头或时间戳数值大小推断任何含义。"
                )

                stagedDatasetStatus

                GroupBox("时间戳合同") {
                    VStack(alignment: .leading, spacing: 12) {
                        Picker("数据源时间戳单位", selection: sourceTimeUnitBinding) {
                            Text("未确定").tag("")
                            Text("秒（s）").tag(SpikeTimeUnit.seconds.rawValue)
                            Text("毫秒（ms）").tag(SpikeTimeUnit.milliseconds.rawValue)
                        }
                        .frame(maxWidth: 360, alignment: .leading)

                        Text("接受的时间戳必须能精确映射为有符号整数微秒。以秒显示时保留 6 位小数，以毫秒显示时保留 3 位；导入器绝不会静默舍入。记录经过时间为负值时会阻止导入；只有明确确认事件原点后，有符号的事件相对时间戳才可能有效。")
                            .font(.callout)
                            .foregroundStyle(.secondary)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.vertical, 6)
                }

                GroupBox("此文件所表示的活动类型") {
                    VStack(alignment: .leading, spacing: 12) {
                        Picker("活动模式", selection: activityModeBinding) {
                            Text("未确定").tag(nil as ScientificDatasetActivityMode?)
                            Text(l10n(ScientificDatasetActivityMode.putativeSingleUnit.activityModePickerDisplaySource)).tag(
                                ScientificDatasetActivityMode.putativeSingleUnit as ScientificDatasetActivityMode?
                            )
                            Text(l10n(ScientificDatasetActivityMode.intentionalMultiUnit.activityModePickerDisplaySource)).tag(
                                ScientificDatasetActivityMode.intentionalMultiUnit as ScientificDatasetActivityMode?
                            )
                            Text(l10n(ScientificDatasetActivityMode.unknownOrUncertain.activityModePickerDisplaySource)).tag(
                                ScientificDatasetActivityMode.unknownOrUncertain as ScientificDatasetActivityMode?
                            )
                        }
                        .frame(maxWidth: 420, alignment: .leading)

                        Text(activityModeExplanation)
                            .font(.callout)
                            .foregroundStyle(.secondary)

                        Text("更改活动模式会清除每个 spike 列的重复时间戳决定，因为相同时间戳在不同模式下具有不同科学含义。请重新逐项明确确认。")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.vertical, 6)
                }

                recordingSegmentGroupBox

                datasetMeaningCompletionStatus

                authorityNotice
            }
            .padding(24)
        }
    }

    private var recordingSegmentGroupBox: some View {
        GroupBox("此文件的记录片段") {
            VStack(alignment: .leading, spacing: 12) {
                Text("本次导入整体视为一个记录片段，所有 spike train 和事件组均属于该片段。共享同一片段并不意味着时钟或原点不同的分组可以直接比较。")
                    .font(.callout)
                    .foregroundStyle(.secondary)

                TextField("记录片段 ID", text: recordingSegmentIDBinding, prompt: Text("输入并确认稳定的片段标识符"))
                    .frame(maxWidth: 360, alignment: .leading)
                Text("这是承载科学身份的稳定记录片段标识符，会进入数据集的科学身份。系统不会静默设置默认值、删改或生成；非空但无效的标识符会被拒绝，而不会被改写。")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Text("例如可输入 Grechishnikova_STN_2017。此处不会根据文件名自动填写，必须由您明确确认。")
                    .font(.caption)
                    .foregroundStyle(.secondary)

                Picker("记录组织方式", selection: recordingRegimeBinding) {
                    Text("未确定").tag(nil as ScientificRecordingRegime?)
                    Text("连续记录，无试次").tag(ScientificRecordingRegime.continuousUntrialed as ScientificRecordingRegime?)
                    Text("按试次组织").tag(ScientificRecordingRegime.trialized as ScientificRecordingRegime?)
                    Text("未知或不确定").tag(ScientificRecordingRegime.unknownOrUncertain as ScientificRecordingRegime?)
                }
                .frame(maxWidth: 420, alignment: .leading)

                Picker("导入片段内的 spike-train 覆盖情况", selection: importedExcerptCoverageBinding) {
                    Text("未确定").tag(nil as ImportedExcerptCoverage?)
                    Text("所有纳入的 spike-train 数据流在整个导入片段内均持续可观察且有效").tag(ImportedExcerptCoverage.allSpikeTrainsFullImportedExcerpt as ImportedExcerptCoverage?)
                    Text("至少一条纳入的 spike-train 数据流在导入片段内并非全程可观察或有效").tag(ImportedExcerptCoverage.notAllSpikeTrainsFullImportedExcerpt as ImportedExcerptCoverage?)
                    Text("未知或不确定").tag(ImportedExcerptCoverage.unknownOrUncertain as ImportedExcerptCoverage?)
                }
                .frame(maxWidth: 480, alignment: .leading)
                Text("完整覆盖表示每条纳入的 spike-train 数据流在整个片段内持续可观察且有效；并不要求片段起止边缘必须出现 spike、数据流必须非空，也不构成单单位分离质量的证明。")
                    .font(.caption)
                    .foregroundStyle(.secondary)

                Toggle("我确认确切的记录起止时间未知或无法获得", isOn: observationBoundsConfirmedBinding)
                    .frame(maxWidth: 480, alignment: .leading)

                Text("重复的刺激或奖赏事件不会自动建立试次。连续数据可能只是较长记录中的截取片段。由于确切起止时间未知，导入器不能给出权威的全记录发放率、状态占用率、起止边缘 pause，或断言某状态延伸到采集边界。")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.vertical, 6)
        }
    }

    private var stagedDatasetStatus: some View {
        GroupBox {
            HStack(alignment: .top, spacing: 12) {
                Image(systemName: "checkmark.circle.fill")
                    .font(.title2)
                    .foregroundStyle(.green)
                VStack(alignment: .leading, spacing: 5) {
                    Text("数据文件已成功读取，可以继续配置")
                        .font(.headline)
                    if let staged = coordinator.stagedImport {
                        Text("已暂存全部 \(staged.columns.count) 列和 \(staged.dataRowCount) 个数据行；数据没有丢失，也不是不可用状态。")
                    }
                    Text("当前规范导入不会直接替换主光栅图。完成确认并保存后，可在“手工 ISI 标记与审核”中载入这份数据；主界面继续显示原数据并不表示 CSV 读取失败。")
                        .foregroundStyle(.secondary)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.vertical, 4)
        }
    }

    @ViewBuilder
    private var datasetMeaningCompletionStatus: some View {
        let missingItems = datasetMeaningMissingItems
        if missingItems.isEmpty {
            Label("本页所需信息已全部确认，可以继续设置“列与分组”。", systemImage: "checkmark.circle.fill")
                .foregroundStyle(.green)
                .padding(12)
                .frame(maxWidth: .infinity, alignment: .leading)
                .background(.green.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
        } else {
            VStack(alignment: .leading, spacing: 7) {
                Label("本页仍有 \(missingItems.count) 项需要确认", systemImage: "info.circle.fill")
                    .font(.headline)
                ForEach(missingItems, id: \.self) { item in
                    Text("• \(item)")
                }
                Text("这些是数据含义尚未确认，不表示文件读取失败。您也可以先进入后续页面查看已读取的列。")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .padding(12)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.blue.opacity(0.07), in: RoundedRectangle(cornerRadius: 8))
        }
    }

    private var datasetMeaningMissingItems: [String] {
        guard let form = coordinator.manifestForm else { return ["等待数据源绑定完成"] }
        var items: [String] = []
        if form.sourceTimeUnit == nil { items.append("选择数据源时间戳单位") }
        if form.activityMode == nil { items.append("选择活动模式") }
        if form.recordingSegmentIDText.isEmpty { items.append("输入稳定的记录片段 ID") }
        if form.recordingRegime == nil { items.append("选择记录组织方式") }
        if form.importedExcerptCoverage == nil { items.append("确认导入片段内的数据流覆盖情况") }
        if !form.observationBoundsConfirmedUnavailable { items.append("确认确切的记录起止时间未知或无法获得") }
        return items
    }

    private var columnsAndGroupsStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                stepHeader(
                    "定义数据列和事件组",
                    subtitle: "每个源列必须且只能分配一次。每个连续分组必须遵循 S+ E*：一个或多个 spike train，之后可接零个或多个事件列。"
                )

                if let form = coordinator.manifestForm {
                    spikeTrainBatchSetup

                    LazyVStack(alignment: .leading, spacing: 16) {
                        ForEach(form.columns) { column in
                            columnEditor(id: column.id)
                        }
                    }
                }
            }
            .padding(24)
        }
    }

    private var spikeTrainBatchSetup: some View {
        GroupBox("常见纯 Spike-train 表格的批量设置") {
            VStack(alignment: .leading, spacing: 12) {
                Text("如果本文件的每一列都是同一次记录中的 spike train，且没有事件列，可用下面的显式操作减少逐列重复设置。系统不会仅凭数值或表头自动套用模板。")
                    .font(.callout)
                    .foregroundStyle(.secondary)

                HStack(spacing: 10) {
                    Button("套用：全部列属于同一记录经过时间分组") {
                        isConfirmingSingleSpikeGroupTemplate = true
                    }

                    Button("用表头填充尚为空白的列 ID") {
                        mutateForm { $0.fillEmptyColumnSemanticIDsFromHeaders() }
                    }

                    Menu("批量设置时间顺序") {
                        Button("全部保留数据源顺序") {
                            mutateForm {
                                $0.applyOrderDecisionToAllSpikeColumns(.preserveSourceOrder)
                            }
                        }
                        Button("全部采用稳定升序排序") {
                            mutateForm {
                                $0.applyOrderDecisionToAllSpikeColumns(.stableAscendingSort)
                            }
                        }
                    }

                    Menu("批量设置重复时间戳") {
                        Button("全部保留事件重数") {
                            mutateForm {
                                $0.applyDuplicateDecisionToAllSpikeColumns(.preserveMultiplicity)
                            }
                        }
                        if coordinator.manifestForm?.activityMode == .putativeSingleUnit {
                            Button("全部在单神经元分析视图中虚拟折叠") {
                                mutateForm {
                                    $0.applyDuplicateDecisionToAllSpikeColumns(.collapseExact)
                                }
                            }
                        }
                    }
                    .disabled(coordinator.manifestForm?.activityMode == nil)
                }

                if let firstColumnID = coordinator.manifestForm?.columns.first?.id {
                    HStack(spacing: 10) {
                        Text("单一分组语义 ID")
                            .fontWeight(.semibold)
                        TextField(
                            "例如 STN_recording_group",
                            text: columnTextBinding(firstColumnID, \.groupSemanticIDText)
                        )
                        .frame(maxWidth: 360)
                    }
                }

                Text("套用模板后，仍需确认分组 ID、列 ID、时间顺序和重复时间戳策略。")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.vertical, 5)
        }
        .confirmationDialog(
            "确认所有列都是同一记录经过时间分组中的 Spike train？",
            isPresented: $isConfirmingSingleSpikeGroupTemplate,
            titleVisibility: .visible
        ) {
            Button("确认并套用模板", role: .destructive) {
                mutateForm { $0.configureAllColumnsAsSingleRecordingElapsedSpikeGroup() }
            }
            Button("取消", role: .cancel) {}
        } message: {
            Text("该操作会替换当前列角色和分组边界，但不会自动生成科学 ID，也不会替您决定时间顺序或重复时间戳策略。")
        }
    }

    private var eventAttributesStep: some View {
        ScrollView([.horizontal, .vertical]) {
            VStack(alignment: .leading, spacing: 16) {
                stepHeader(
                    "确认事件属性",
                    subtitle: "每个键都必须明确指定类型、科学或展示角色、单位决定及空字符串策略。批量分配角色仍会为每个键分别记录决定。"
                )

                HStack(spacing: 10) {
                    Button {
                        originCandidatePage = 0
                        Task { await coordinator.refreshPreflight() }
                    } label: {
                        if coordinator.phase == .preflightingSourceFacts {
                            ProgressView()
                                .controlSize(.small)
                        } else {
                            Label("发现数据源事实", systemImage: "magnifyingglass")
                        }
                    }
                    .buttonStyle(.borderedProminent)
                    .disabled(coordinator.isBusy)

                    Button {
                        mutateForm { $0.addAttribute() }
                    } label: {
                        Label("添加属性", systemImage: "plus")
                    }

                    Button("将所选设为：科学属性") {
                        mutateForm { $0.applyAttributeRole(.scientific, to: selectedAttributeIDs) }
                    }
                    .disabled(selectedAttributeIDs.isEmpty)

                    Button("将所选设为：展示属性") {
                        mutateForm { $0.applyAttributeRole(.presentation, to: selectedAttributeIDs) }
                    }
                    .disabled(selectedAttributeIDs.isEmpty)

                    Button(role: .destructive) {
                        mutateForm { $0.removeAttributes(ids: selectedAttributeIDs) }
                        selectedAttributeIDs.removeAll()
                    } label: {
                        Label("移除所选", systemImage: "trash")
                    }
                    .disabled(selectedAttributeIDs.isEmpty)
                }

                attributeHeader
                if let form = coordinator.manifestForm, form.attributes.isEmpty {
                    Text("尚无属性定义。可手动添加键，或运行“发现数据源事实”，将事件时间戳旁发现的未确定键加入列表。")
                        .foregroundStyle(.secondary)
                        .padding(.vertical, 18)
                } else if let form = coordinator.manifestForm {
                    LazyVStack(alignment: .leading, spacing: 8) {
                        ForEach(form.attributes) { attribute in
                            attributeEditor(id: attribute.id)
                            Divider()
                        }
                    }
                }

                Text("内联的 @x.unit=… 仅是导入建议。上方经确认的 manifest 单位是唯一权威；发生冲突时将阻止验证。")
                    .font(.callout)
                    .foregroundStyle(.secondary)

                GroupBox("事件列元数据语法") {
                    HStack(alignment: .top, spacing: 20) {
                        Text(
                            """
                            event_stimulus
                            1.250000
                            @intensity=2.5
                            @intensity.unit=mW
                            @note=
                            2.000000
                            @condition=reward
                            """
                        )
                        .font(.system(.caption, design: .monospaced))
                        .textSelection(.enabled)

                        Text("每个时间戳开始一次事件 occurrence。同一事件列中连续的 @key=value 行都属于该 occurrence，直到下一个时间戳或空白行为止。仅当 note 被确认为 String 且允许空字符串时，@note= 才表示明确存在的空字符串；省略 @note 行表示该键缺失。.unit 行仅是建议，不能覆盖已确认的 manifest。")
                            .font(.callout)
                            .foregroundStyle(.secondary)
                            .frame(maxWidth: 620, alignment: .leading)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.vertical, 6)
                }

                preflightFacts
            }
            .padding(24)
            .frame(minWidth: 1_050, alignment: .topLeading)
        }
    }

    private var reviewStep: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                stepHeader(
                    "验证前审核",
                    subtitle: "下一步将执行解析 → 精确规范化 → 独立验证，并显示可定位到数据源的阻断项。"
                )

                if let staged = coordinator.stagedImport {
                    sourcePreview(staged)
                }

                reviewOutcome

                authorityNotice

                persistenceStatusView

                if coordinator.persistedConfirmation == nil {
                    Label(
                        "当前内容只是待验证的导入草稿，尚未成为已确认的数据集，也不授予分析权限；它不会替换当前活动数据。",
                        systemImage: "lock.shield"
                    )
                    .foregroundStyle(.orange)
                    .padding(14)
                    .background(.orange.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
                }
            }
            .padding(24)
        }
    }

    private var unavailableStageB: some View {
        ContentUnavailableView(
            "请先绑定数据源事实",
            systemImage: "lock",
            description: Text("请在“数据源”中选择工作表和表头规则，然后绑定确切的表格快照。")
        )
    }

    private var footer: some View {
        HStack(spacing: 12) {
            Text(phaseDescription)
                .font(.callout)
                .foregroundStyle(.secondary)
            Spacer()

            if coordinator.persistedConfirmation != nil {
                Button {
                    onProceedToManualAnalysis()
                } label: {
                    Label("进入人工 ISI 标记与分析", systemImage: "person.crop.circle.badge.checkmark")
                }
                .buttonStyle(.borderedProminent)
                .disabled(!canProceedToCanonicalManualAnalysis)

                Button("关闭") {
                    onClose()
                }
            } else {
                if coordinator.stagedImport != nil, let previousStep {
                    Button("上一步：\(previousStep.title)") {
                        selectedStep = previousStep
                    }
                    .disabled(coordinator.isBusy)
                }

                if coordinator.stagedImport != nil, let nextStep {
                    Button("下一步：\(nextStep.title)") {
                        selectedStep = nextStep
                    }
                    .buttonStyle(.borderedProminent)
                    .disabled(coordinator.isBusy)
                }

                if selectedStep == .source,
                   coordinator.source != nil,
                   coordinator.phase != .readingSource {
                    Button {
                        Task { await coordinator.bindSourceFacts() }
                    } label: {
                        if coordinator.phase == .stagingSource {
                            ProgressView()
                                .controlSize(.small)
                        } else {
                            Text(coordinator.stagedImport == nil ? "绑定数据源事实" : "重新绑定数据源事实")
                        }
                    }
                    .buttonStyle(.borderedProminent)
                    .disabled(!coordinator.canBindSourceFacts || coordinator.isBusy)
                }

                if selectedStep == .review, coordinator.stagedImport != nil {
                    Button {
                        Task { await coordinator.validateScientificReview() }
                    } label: {
                        if coordinator.phase == .validatingScientificMeaning {
                            ProgressView()
                                .controlSize(.small)
                        } else {
                            Text("运行精确验证")
                        }
                    }
                    .buttonStyle(.bordered)
                    .disabled(coordinator.isBusy)
                }

                if selectedStep == .review, coordinator.canConfirmAndSave {
                    Button {
                        Task { await coordinator.confirmAndSaveScientificImport() }
                    } label: {
                        if coordinator.isPersisting {
                            ProgressView()
                                .controlSize(.small)
                        } else {
                            Text("确认并保存")
                        }
                    }
                    .buttonStyle(.borderedProminent)
                    .disabled(coordinator.isBusy)
                }

                Button("取消审核", role: .cancel) {
                    coordinator.cancel()
                    onClose()
                }
            }
        }
        .padding(.horizontal, 18)
        .padding(.vertical, 12)
    }

    /// A saved-and-verified confirmation notice (or the current persistence message). A compatible
    /// single-unit import may proceed to detector-independent manual ISI analysis. Automatic detector
    /// authority remains locked until its separate run/consumer contract is complete.
    @ViewBuilder
    private var persistenceStatusView: some View {
        if let persisted = coordinator.persistedConfirmation {
            Label {
                VStack(alignment: .leading, spacing: 4) {
                    Text("已保存并验证确认清单。")
                        .fontWeight(.semibold)
                    Text(canProceedToCanonicalManualAnalysis
                        ? "记录 \(persisted.confirmationRecordDigest.prefix(12))… 已在内存中激活。现在可以进入人工 ISI 标记与描述性分析；自动检测器和自动算法结果仍保持锁定。"
                        : "记录 \(persisted.confirmationRecordDigest.prefix(12))… 已在内存中激活。该数据的活动模式或时间组织不满足单神经元人工模式分析条件，仍可保存和恢复，但不会运行单神经元模式分析。")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                }
            } icon: {
                Image(systemName: "checkmark.seal.fill").foregroundStyle(.green)
            }
            .padding(14)
            .background(.green.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
        } else if let message = coordinator.persistenceMessage {
            Label(message, systemImage: "info.circle")
                .padding(14)
                .background(.blue.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
        }
    }

    /// Discovered saved manifests for the current source. Discovery only surfaces candidates; each
    /// requires explicit selection and an explicit Restore & Verify, which replays the full chain. A
    /// discovery/store error remains visible even when zero valid records are found.
    @ViewBuilder
    private var savedManifestsSection: some View {
        if !coordinator.savedManifestSummaries.isEmpty {
            GroupBox("此数据源已保存的清单") {
                VStack(alignment: .leading, spacing: 10) {
                    Text("此确切数据源有 \(coordinator.savedManifestSummaries.count) 条已保存的确认记录。恢复时会重新读取数据源、重放完整导入链并进行精确比较；仅解码文件绝不会自动恢复确认。请明确选择一条记录。")
                        .font(.callout)
                        .foregroundStyle(.secondary)

                    ForEach(coordinator.savedManifestSummaries, id: \.recordDigest) { summary in
                        savedManifestRow(summary)
                    }

                    // Once a wrapper is installed this session, Restore is REPLACED by the verified
                    // standing — never offered again (re-restoring is neither permitted nor shown).
                    if coordinator.persistedConfirmation == nil {
                        Button {
                            Task { await coordinator.restoreAndVerify() }
                        } label: {
                            if coordinator.isPersisting {
                                ProgressView().controlSize(.small)
                            } else {
                                Text("恢复并验证")
                            }
                        }
                        .buttonStyle(.borderedProminent)
                        .disabled(!coordinator.canRestoreSelectedManifest)

                        if coordinator.phase != .awaitingSourceDecisions {
                            Text("只能在绑定数据源事实之前执行恢复。如需恢复已保存清单，请重新绑定或取消当前操作。")
                                .font(.caption)
                                .foregroundStyle(.secondary)
                        }

                        if let message = coordinator.persistenceMessage {
                            Text(message).font(.callout).foregroundStyle(.secondary)
                        }
                    } else {
                        persistenceStatusView
                    }
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(.vertical, 6)
            }
        } else if coordinator.persistedConfirmation == nil, let message = coordinator.persistenceMessage {
            // Zero valid saved records, but a discovery/store issue must remain visible.
            Label(message, systemImage: "exclamationmark.triangle")
                .font(.callout)
                .foregroundStyle(.secondary)
                .padding(12)
                .frame(maxWidth: .infinity, alignment: .leading)
                .background(.yellow.opacity(0.10), in: RoundedRectangle(cornerRadius: 8))
        }
    }

    private func savedManifestRow(_ summary: StoredManifestSummary) -> some View {
        let isSelected = coordinator.selectedSavedRecordDigest == summary.recordDigest
        return Button {
            coordinator.selectSavedManifest(recordDigest: isSelected ? nil : summary.recordDigest)
        } label: {
            HStack(spacing: 10) {
                Image(systemName: isSelected ? "largecircle.fill.circle" : "circle")
                    .foregroundStyle(isSelected ? Color.accentColor : Color.secondary)
                VStack(alignment: .leading, spacing: 2) {
                    Text("片段 \(summary.recordingSegmentID) · \(activityModeLabel(summary.activityMode))")
                    Text("\(selectionLabel(summary.selection)) · \(headerRuleLabel(summary.headerRule)) · \(sourceUnitLabel(summary.sourceTimeUnit))")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    Text("\(regimeLabel(summary.recordingRegime)) · \(coverageLabel(summary.importedExcerptCoverage)) · \(summary.groupCount) 个分组，\(summary.attributeCount) 个属性 · \(summary.recordDigestPrefix)…")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                Spacer()
            }
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .disabled(coordinator.isPersisting)
    }

    private func activityModeLabel(_ mode: ScientificDatasetActivityMode) -> String {
        return l10n(mode.activityModeDisplaySource)
    }

    private func headerRuleLabel(_ rule: PersistedTabularHeaderRule) -> String {
        switch rule {
        case .firstRecordIsHeader: return "首行为表头"
        case .headerless: return "无表头"
        }
    }

    private func regimeLabel(_ regime: ScientificRecordingRegime) -> String {
        switch regime {
        case .continuousUntrialed: return "连续记录"
        case .trialized: return "按试次组织"
        case .unknownOrUncertain: return "组织方式未知"
        }
    }

    private func coverageLabel(_ coverage: ImportedExcerptCoverage) -> String {
        switch coverage {
        case .allSpikeTrainsFullImportedExcerpt: return "完整覆盖"
        case .notAllSpikeTrainsFullImportedExcerpt: return "部分覆盖"
        case .unknownOrUncertain: return "覆盖情况未知"
        }
    }

    private func selectionLabel(_ selection: StagedSourceTransactionSelection) -> String {
        switch selection {
        case .commaSeparatedValues: return "CSV"
        case .excelWorksheet(let name, _, _, _): return "XLSX · \(name)"
        }
    }

    private func sourceUnitLabel(_ unit: SpikeTimeUnit) -> String {
        switch unit {
        case .seconds: return "秒"
        case .milliseconds: return "毫秒"
        }
    }

    private func sourceIdentityCard(_ source: BoundedScientificSource) -> some View {
        GroupBox("确切数据源快照") {
            Grid(alignment: .leading, horizontalSpacing: 18, verticalSpacing: 8) {
                GridRow {
                    Text("文件").foregroundStyle(.secondary)
                    Text(source.displayName)
                }
                GridRow {
                    Text("格式").foregroundStyle(.secondary)
                    Text(source.format.rawValue.uppercased())
                }
                GridRow {
                    Text("大小").foregroundStyle(.secondary)
                    Text(ByteCountFormatter.string(fromByteCount: Int64(source.byteCount), countStyle: .file))
                }
                GridRow {
                    Text("SHA-256").foregroundStyle(.secondary)
                    Text(source.sourceSHA256)
                        .font(.system(.caption, design: .monospaced))
                        .textSelection(.enabled)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.vertical, 6)
        }
    }

    private func sourceDecisionCard(_ source: BoundedScientificSource) -> some View {
        GroupBox("明确的数据源决定") {
            VStack(alignment: .leading, spacing: 14) {
                if source.format == .xlsx {
                    Picker("工作表", selection: worksheetBinding) {
                        Text("未确定").tag(nil as UInt32?)
                        ForEach(coordinator.worksheets, id: \.sheetID) { worksheet in
                            Text("\(worksheet.name) — \(visibilityLabel(worksheet.visibility))")
                                .tag(worksheet.sheetID as UInt32?)
                        }
                    }
                    .frame(maxWidth: 520, alignment: .leading)
                    Text("隐藏工作表仍会显示在此列表中，并且必须由用户明确选择。")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }

                Picker("首个逻辑行", selection: headerDecisionBinding) {
                    Text("未确定").tag(nil as CanonicalTabularHeaderDecision?)
                    Text("用作列标题").tag(
                        CanonicalTabularHeaderDecision.firstRecordIsHeader as CanonicalTabularHeaderDecision?
                    )
                    Text("作为数据处理（无表头）").tag(
                        CanonicalTabularHeaderDecision.headerless as CanonicalTabularHeaderDecision?
                    )
                }
                .frame(maxWidth: 520, alignment: .leading)

                if coordinator.headerDecision == .headerless {
                    Label(
                        "无表头输入仅限一个记录经过时间分组，且只能包含 spike 列；不允许事件列。",
                        systemImage: "exclamationmark.triangle"
                    )
                    .font(.callout)
                    .foregroundStyle(.orange)
                }

                Text("更改任一决定都会丢弃阶段 B 的全部选择。不同数据源绑定之间不会静默迁移任何内容。")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.vertical, 6)
        }
    }

    private func sourcePreview(_ staged: StagedScientificImport) -> some View {
        let visibleColumns = Array(staged.columns.prefix(12))
        let visibleRowCount = min(staged.dataRowCount, 20)
        return GroupBox("只读表格预览") {
            VStack(alignment: .leading, spacing: 8) {
                ScrollView([.horizontal, .vertical]) {
                    VStack(alignment: .leading, spacing: 0) {
                        HStack(spacing: 0) {
                            previewCell("行", isHeader: true, width: 52)
                            ForEach(visibleColumns, id: \.sourceColumn) { column in
                                previewCell(
                                    column.header ?? "列 \(column.sourceColumn.oneBasedIndex)",
                                    isHeader: true
                                )
                            }
                        }
                        ForEach(0..<visibleRowCount, id: \.self) { row in
                            HStack(spacing: 0) {
                                previewCell("\(row + 1)", isHeader: true, width: 52)
                                ForEach(visibleColumns, id: \.sourceColumn) { column in
                                    previewCell(stagedCellText(column.cells[row]), isHeader: false)
                                }
                            }
                        }
                    }
                }
                .frame(maxHeight: 380)

                if staged.columns.count > visibleColumns.count || staged.dataRowCount > visibleRowCount {
                    Text("预览显示前 \(visibleColumns.count)/\(staged.columns.count) 列及前 \(visibleRowCount)/\(staged.dataRowCount) 个数据行。暂存中仍绑定全部行和列。")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            .padding(.vertical, 6)
        }
    }

    private func columnEditor(id: UUID) -> some View {
        let form = columnForm(id: id)
        let index = coordinator.manifestForm?.columns.firstIndex(where: { $0.id == id }) ?? 0
        let sourceIndex = form?.sourceColumn.oneBasedIndex ?? index + 1
        let isGroupStart = index == 0 || form?.startsNewGroup == true
        return GroupBox {
            VStack(alignment: .leading, spacing: 12) {
                HStack(alignment: .firstTextBaseline) {
                    Text("列 \(sourceIndex)")
                        .font(.headline)
                    Text(form?.header.map { "表头：\($0)" } ?? "无表头")
                        .foregroundStyle(.secondary)
                    Spacer()

                    if index == 0 {
                        Text("开始分组 1")
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                    } else {
                        Picker("分组边界", selection: columnBoundaryBinding(id)) {
                            Text("未确定").tag(nil as Bool?)
                            Text("延续当前分组").tag(false as Bool?)
                            Text("开始新分组").tag(true as Bool?)
                        }
                        .labelsHidden()
                        .frame(width: 170)
                    }
                }

                if isGroupStart {
                    HStack(spacing: 12) {
                        TextField("分组语义 ID", text: columnTextBinding(id, \.groupSemanticIDText))
                            .frame(width: 220)
                        Picker("时间基准", selection: columnOptionalBinding(id, \.groupTimeBasis)) {
                            Text("未确定").tag(nil as ScientificImportTimeBasisChoice?)
                            Text("记录经过时间").tag(
                                ScientificImportTimeBasisChoice.recordingElapsed as ScientificImportTimeBasisChoice?
                            )
                            Text("事件相对时间").tag(
                                ScientificImportTimeBasisChoice.eventRelative as ScientificImportTimeBasisChoice?
                            )
                        }
                        .frame(width: 250)

                        if form?.groupTimeBasis == .eventRelative {
                            TextField(
                                "原点事件列",
                                text: columnTextBinding(id, \.eventOriginColumnText)
                            )
                            .frame(width: 150)
                            TextField(
                                "原点数据行",
                                text: columnTextBinding(id, \.eventOriginDataRowText)
                            )
                            .frame(width: 140)
                        }
                    }
                }

                HStack(spacing: 12) {
                    Picker("角色", selection: columnOptionalBinding(id, \.role)) {
                        Text("未确定").tag(nil as ScientificImportColumnRole?)
                        Text("Spike train").tag(
                            ScientificImportColumnRole.spikeTrain as ScientificImportColumnRole?
                        )
                        Text("事件定义").tag(
                            ScientificImportColumnRole.eventDefinition as ScientificImportColumnRole?
                        )
                    }
                    .frame(width: 190)

                    TextField("列语义 ID", text: columnTextBinding(id, \.semanticIDText))
                        .frame(width: 210)

                    if form?.role == .eventDefinition {
                        TextField("事件类型 ID", text: columnTextBinding(id, \.eventTypeIDText))
                            .frame(width: 180)
                    }

                    Picker("顺序", selection: columnOptionalBinding(id, \.orderDecision)) {
                        Text("顺序未确定").tag(nil as TimestampOrderDecision?)
                        Text("保留数据源顺序").tag(
                            TimestampOrderDecision.preserveSourceOrder as TimestampOrderDecision?
                        )
                        Text("稳定升序排序").tag(
                            TimestampOrderDecision.stableAscendingSort as TimestampOrderDecision?
                        )
                    }
                    .frame(width: 220)

                    if form?.role == .spikeTrain {
                        Picker("完全重复时间戳", selection: columnOptionalBinding(id, \.duplicateDecision)) {
                            Text("重复策略未确定").tag(nil as ExactDuplicateDecision?)
                            Text("保留事件重数").tag(
                                ExactDuplicateDecision.preserveMultiplicity as ExactDuplicateDecision?
                            )
                            if coordinator.manifestForm?.activityMode == .putativeSingleUnit {
                                Text("在单单位分析视图中虚拟折叠").tag(
                                    ExactDuplicateDecision.collapseExact as ExactDuplicateDecision?
                                )
                            }
                        }
                        .frame(width: 210)
                        .disabled(coordinator.manifestForm?.activityMode == nil)
                        .help(
                            duplicateDecisionHelp
                        )
                    }
                }
            }
            .padding(.vertical, 5)
        }
    }

    private var attributeHeader: some View {
        HStack(spacing: 8) {
            Text("").frame(width: 24)
            Text("键").frame(width: 170, alignment: .leading)
            Text("类型").frame(width: 150, alignment: .leading)
            Text("角色").frame(width: 150, alignment: .leading)
            Text("单位").frame(width: 160, alignment: .leading)
            Text("指定符号").frame(width: 150, alignment: .leading)
            Text("空字符串").frame(width: 180, alignment: .leading)
        }
        .font(.caption.weight(.semibold))
        .foregroundStyle(.secondary)
    }

    private func attributeEditor(id: UUID) -> some View {
        let form = attributeForm(id: id)
        return HStack(spacing: 8) {
            Toggle(
                "选择属性",
                isOn: Binding(
                    get: { selectedAttributeIDs.contains(id) },
                    set: { selected in
                        if selected { selectedAttributeIDs.insert(id) }
                        else { selectedAttributeIDs.remove(id) }
                    }
                )
            )
            .labelsHidden()
            .frame(width: 24)

            TextField("key", text: attributeTextBinding(id, \.keyText))
                .frame(width: 170)
            Picker("类型", selection: attributeOptionalBinding(id, \.scalarType)) {
                Text("未确定").tag(nil as EventAttributeScalarType?)
                ForEach(EventAttributeScalarType.allCases, id: \.self) { type in
                    Text(attributeTypeLabel(type)).tag(type as EventAttributeScalarType?)
                }
            }
            .labelsHidden()
            .frame(width: 150)
            Picker("角色", selection: attributeOptionalBinding(id, \.role)) {
                Text("未确定").tag(nil as EventAttributeRole?)
                Text("科学属性").tag(EventAttributeRole.scientific as EventAttributeRole?)
                Text("展示属性").tag(EventAttributeRole.presentation as EventAttributeRole?)
            }
            .labelsHidden()
            .frame(width: 150)
            Picker("单位", selection: attributeOptionalBinding(id, \.unitChoice)) {
                Text("未确定").tag(nil as ScientificImportAttributeUnitChoice?)
                Text("不适用").tag(
                    ScientificImportAttributeUnitChoice.notApplicable as ScientificImportAttributeUnitChoice?
                )
                Text("无量纲").tag(
                    ScientificImportAttributeUnitChoice.dimensionless as ScientificImportAttributeUnitChoice?
                )
                Text("指定单位").tag(
                    ScientificImportAttributeUnitChoice.specified as ScientificImportAttributeUnitChoice?
                )
            }
            .labelsHidden()
            .frame(width: 160)
            TextField("e.g. mW", text: attributeTextBinding(id, \.specifiedUnitText))
                .frame(width: 150)
                .disabled(form?.unitChoice != .specified)
            Picker("空字符串", selection: attributeOptionalBinding(id, \.emptyStringPolicy)) {
                Text("未确定").tag(nil as EventEmptyStringPolicy?)
                Text("禁止").tag(EventEmptyStringPolicy.forbid as EventEmptyStringPolicy?)
                Text("允许明确空值").tag(
                    EventEmptyStringPolicy.allowExplicitEmptyString as EventEmptyStringPolicy?
                )
            }
            .labelsHidden()
            .frame(width: 180)
        }
    }

    private var authorityNotice: some View {
        VStack(alignment: .leading, spacing: 6) {
            Label(
                canProceedToCanonicalManualAnalysis
                    ? "人工 ISI 标记与描述性分析可用"
                    : "自动分析权限仍处于锁定状态",
                systemImage: canProceedToCanonicalManualAnalysis
                    ? "person.crop.circle.badge.checkmark"
                    : "lock.shield"
            )
                .font(.headline)
            Text(canProceedToCanonicalManualAnalysis
                ? "该确认数据可以直接进入逐 ISI 人工标记、检查和导出流程。此入口不运行自动检测器，也不会把当前尚未完善的自动算法结果声明为权威结果。"
                : "只有在所有决定均完成、精确规范化和独立验证均无阻断项，并由用户明确确认后，\(l10n(ScientificDatasetActivityMode.putativeSingleUnit.activityModeDisplaySource))数据才可能具备后续资格。多神经元及未知/不确定数据仍只能浏览，不能运行单神经元检测器。")
                .font(.callout)
                .foregroundStyle(.secondary)
        }
        .padding(14)
        .background(.blue.opacity(0.07), in: RoundedRectangle(cornerRadius: 8))
    }

    private var canProceedToCanonicalManualAnalysis: Bool {
        guard let base = coordinator.persistedConfirmation?.baseConfirmation else {
            return false
        }
        return base.activityMode == .putativeSingleUnit
            && base.recordingSegment.regime == .continuousUntrialed
            && base.recordingSegment.importedExcerptCoverage
                == .allSpikeTrainsFullImportedExcerpt
    }

    @ViewBuilder
    private var preflightFacts: some View {
        if !coordinator.preflightFormIssues.isEmpty {
            issueList(
                title: "预检表单阻断项",
                messages: coordinator.preflightFormIssues.map(formIssueMessage),
                additionalCount: 0
            )
        } else if let report = coordinator.preflightReport {
            VStack(alignment: .leading, spacing: 12) {
                Text("可定位到数据源的预检")
                    .font(.headline)

                if report.hasIssues {
                    issueList(
                        title: "预检发现",
                        messages: report.issues.map { String(describing: $0) },
                        additionalCount: report.additionalIssueCount
                    )
                }

                if !report.columnFacts.isEmpty {
                    Text("时间戳事实")
                        .font(.subheadline.weight(.semibold))
                    Grid(alignment: .leading, horizontalSpacing: 18, verticalSpacing: 6) {
                        GridRow {
                            Text("分组 / 列")
                            Text("角色")
                            Text("有效数")
                            Text("源负值")
                            Text("逆序数")
                            Text("完全重复数")
                        }
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)
                        ForEach(Array(report.columnFacts.prefix(100).enumerated()), id: \.offset) { _, fact in
                            GridRow {
                                Text("G\(fact.groupIndex) / C\(fact.sourceColumn.oneBasedIndex)")
                                Text(fact.role == .spikeTrain ? "Spike" : "事件")
                                Text("\(fact.validTimestampCount)")
                                Text("\(fact.negativeTimestampCount)")
                                Text("\(fact.sourceOrderDescentCount)")
                                Text("\(fact.exactDuplicateTimestampCount)")
                            }
                            .font(.caption.monospacedDigit())
                        }
                    }
                    if report.columnFacts.count > 100 {
                        Text("当前显示已分类列中的前 100/\(report.columnFacts.count) 列。")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                }

                if !report.discoveredEventAttributes.isEmpty {
                    Text("发现的事件属性")
                        .font(.subheadline.weight(.semibold))
                    ForEach(
                        Array(report.discoveredEventAttributes.prefix(100).enumerated()),
                        id: \.offset
                    ) { _, attribute in
                        let shownUnitSuggestions = attribute.inlineUnitSuggestions
                            .prefix(10)
                            .map(\.rawUnit)
                        HStack(alignment: .firstTextBaseline, spacing: 12) {
                            Text(attribute.key.canonicalText)
                                .font(.system(.callout, design: .monospaced))
                                .frame(width: 220, alignment: .leading)
                            Text("发现 \(attribute.valueSightings.count) 个取值")
                            if !shownUnitSuggestions.isEmpty {
                                Text(
                                    "内联单位建议：\(shownUnitSuggestions.joined(separator: ", "))"
                                        + (attribute.inlineUnitSuggestions.count > shownUnitSuggestions.count
                                            ? " … (+\(attribute.inlineUnitSuggestions.count - shownUnitSuggestions.count))"
                                            : "")
                                )
                                    .foregroundStyle(.secondary)
                            }
                        }
                    }
                }

                if !report.eventOriginCandidates.isEmpty {
                    Text("事件相对时间原点候选")
                        .font(.subheadline.weight(.semibold))
                    let pageSize = 50
                    let pageCount = max(
                        1,
                        (report.eventOriginCandidates.count + pageSize - 1) / pageSize
                    )
                    let shownPage = min(originCandidatePage, pageCount - 1)
                    let start = shownPage * pageSize
                    let end = min(start + pageSize, report.eventOriginCandidates.count)

                    HStack(spacing: 10) {
                        Button("上一页") {
                            originCandidatePage = max(0, shownPage - 1)
                        }
                        .disabled(shownPage == 0)
                        Button("下一页") {
                            originCandidatePage = min(pageCount - 1, shownPage + 1)
                        }
                        .disabled(shownPage + 1 >= pageCount)
                        Text("第 \(shownPage + 1)/\(pageCount) 页 · 候选 \(start + 1)–\(end)/\(report.eventOriginCandidates.count)")
                            .font(.caption.monospacedDigit())
                            .foregroundStyle(.secondary)
                    }

                    ForEach(Array(report.eventOriginCandidates[start..<end]), id: \.self) { candidate in
                        VStack(alignment: .leading, spacing: 4) {
                            HStack(spacing: 10) {
                                Text(originCandidateLocationText(candidate))
                                    .font(.system(.caption, design: .monospaced))
                                Button("用作原点") {
                                    applyOriginCandidate(candidate)
                                }
                                .controlSize(.small)
                            }
                            if !candidate.scientificAttributes.isEmpty {
                                Text("科学身份字段：\(originCandidateAttributesText(candidate))")
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                                    .textSelection(.enabled)
                            }
                        }
                    }
                }
            }
            .padding(14)
            .background(.secondary.opacity(0.06), in: RoundedRectangle(cornerRadius: 8))
        } else {
            Text("指定时间戳单位、分组边界和列角色后，请运行“发现数据源事实”。发现的键会被加入列表，但其类型、角色、单位和空值策略仍保持未确定。")
                .font(.callout)
                .foregroundStyle(.secondary)
        }
    }

    @ViewBuilder
    private var reviewOutcome: some View {
        switch coordinator.reviewOutcome {
        case .none:
            Label(
                "尚未对当前草稿运行验证。",
                systemImage: "circle.dotted"
            )
            .foregroundStyle(.secondary)
        case .formIssues(let issues):
            VStack(alignment: .leading, spacing: 10) {
                issueList(
                    title: "表单阻断项",
                    messages: issues.map(formIssueMessage),
                    additionalCount: 0
                )
                Button("返回“列与分组”完成设置") {
                    selectedStep = .columnsAndGroups
                }
            }
        case .planIssues(let issues):
            VStack(alignment: .leading, spacing: 10) {
                issueList(
                    title: "清单阻断项",
                    messages: planIssueMessages(issues),
                    additionalCount: 0
                )
                HStack(spacing: 10) {
                    if issues.contains(where: isDatasetMeaningPlanIssue) {
                        Button("返回“数据集含义”完成设置") {
                            selectedStep = .datasetMeaning
                        }
                    }
                    if issues.contains(where: isColumnOrGroupPlanIssue) {
                        Button("返回“列与分组”完成设置") {
                            selectedStep = .columnsAndGroups
                        }
                    }
                    if issues.contains(where: isMissingDuplicateDecision) {
                        Button("全部保留重复时间戳") {
                            mutateForm {
                                $0.applyDuplicateDecisionToAllSpikeColumns(.preserveMultiplicity)
                            }
                        }
                        if coordinator.manifestForm?.activityMode == .putativeSingleUnit {
                            Button("全部采用虚拟折叠") {
                                mutateForm {
                                    $0.applyDuplicateDecisionToAllSpikeColumns(.collapseExact)
                                }
                            }
                        }
                    }
                }
            }
        case .normalizationIssues(let issues, let additionalCount):
            issueList(
                title: "精确数据阻断项",
                messages: issues.map { String(describing: $0) },
                additionalCount: additionalCount
            )
        case .validation(let report):
            if report.hasBlockingIssues {
                issueList(
                    title: "独立验证阻断项",
                    messages: report.blockingIssues.map { String(describing: $0) },
                    additionalCount: report.additionalBlockingIssueCount
                )
            } else {
                VStack(alignment: .leading, spacing: 8) {
                    Label(
                        "精确规范化和独立验证已通过。",
                        systemImage: "checkmark.seal.fill"
                    )
                    .font(.headline)
                    .foregroundStyle(.green)
                    if !report.warnings.isEmpty || report.additionalWarningCount > 0 {
                        Text("警告：\(report.warnings.count + report.additionalWarningCount)")
                            .foregroundStyle(.orange)
                    }
                    Text("这仅证明准备值可以复现；不会授予科学权限，也不会更改当前活动数据集。")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                }
                .padding(14)
                .frame(maxWidth: .infinity, alignment: .leading)
                .background(.green.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
            }
        }
    }

    private func issueList(
        title: String,
        messages: [String],
        additionalCount: Int
    ) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            Label(title, systemImage: "xmark.octagon.fill")
                .font(.headline)
                .foregroundStyle(.red)
            ForEach(Array(messages.enumerated()), id: \.offset) { _, message in
                Text("• \(message)")
                    .font(.system(.caption, design: .monospaced))
                    .textSelection(.enabled)
            }
            if additionalCount > 0 {
                Text("…另有 \(additionalCount) 个阻断项。")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
        .padding(14)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.red.opacity(0.07), in: RoundedRectangle(cornerRadius: 8))
    }

    private func formIssueMessage(_ issue: ScientificImportManifestFormIssue) -> String {
        switch issue {
        case .sourceBindingChanged:
            return "数据源绑定已经改变，请重新绑定文件。"
        case .sourceColumnsChanged:
            return "数据源列结构已经改变，请重新绑定文件。"
        case .invalidRecordingSegmentID(let error):
            return "记录片段 ID 无效：\(semanticIDErrorMessage(error))"
        case .missingGroupBoundary(let column):
            return "第 \(column) 列尚未确认是延续当前分组还是开始新分组。"
        case .missingColumnRole(let column):
            return "第 \(column) 列尚未选择列角色（Spike train 或事件定义）。"
        case .groupStartsWithEvent(let column):
            return "第 \(column) 列把一个分组从事件列开始；每个分组必须先包含至少一个 Spike train。"
        case .spikeAfterEventWithinGroup(let column):
            return "第 \(column) 列在同一分组的事件列之后又被设为 Spike train；请开始新分组或调整列角色。"
        case .invalidGroupSemanticID(let column, let error):
            return "第 \(column) 列开始的分组 ID 无效：\(semanticIDErrorMessage(error))"
        case .invalidColumnSemanticID(let column, let error):
            return "第 \(column) 列的语义 ID 无效：\(semanticIDErrorMessage(error))"
        case .invalidEventTypeID(let column, let error):
            return "第 \(column) 列的事件类型 ID 无效：\(semanticIDErrorMessage(error))"
        case .invalidEventOriginColumn(let column):
            return "第 \(column) 列填写的原点事件列必须是大于 0 的整数。"
        case .invalidEventOriginDataRow(let column):
            return "第 \(column) 列填写的原点数据行必须是大于 0 的整数。"
        case .missingAttributeKey(let definition):
            return "第 \(definition) 个事件属性尚未填写键名。"
        case .invalidAttributeKey(let definition, let error):
            return "第 \(definition) 个事件属性键无效：\(attributeKeyErrorMessage(error))"
        case .missingSpecifiedUnit(let definition):
            return "第 \(definition) 个事件属性选择了指定单位，但尚未填写单位符号。"
        case .invalidSpecifiedUnit(let definition, let error):
            return "第 \(definition) 个事件属性的单位符号无效：\(unitSymbolErrorMessage(error))"
        }
    }

    private func semanticIDErrorMessage(_ error: ScientificSemanticIDError) -> String {
        switch error {
        case .sourceTooLong(let maximum), .canonicalValueTooLong(let maximum):
            return "长度超过 \(maximum) 个 UTF-8 字节"
        case .empty: return "不能为空"
        case .blank: return "不能只包含空白字符"
        case .containsControlCharacter: return "不能包含控制字符"
        }
    }

    private func planIssueMessage(_ issue: ScientificImportPlanIssue) -> String {
        switch issue {
        case .sourceBindingMismatch:
            return "清单不再属于当前数据源，请重新绑定文件。"
        case .missingSourceTimeUnit:
            return "尚未选择数据源时间戳单位。"
        case .missingActivityMode:
            return "尚未选择活动模式。"
        case .missingRecordingSegmentID:
            return "尚未填写记录片段 ID。"
        case .missingRecordingRegime:
            return "尚未选择记录组织方式。"
        case .missingImportedExcerptCoverage:
            return "尚未确认导入片段内的数据流覆盖情况。"
        case .missingObservationBoundsAvailability:
            return "尚未确认记录起止时间是否未知或无法获得。"
        case .missingEventScopeGroups, .emptyEventScopeGroups:
            return "尚未定义任何有效的数据列分组。"
        case .missingGroupSemanticID(let group):
            return "第 \(group) 个分组尚未填写分组语义 ID。"
        case .groupHasNoSpikeTrains(let group):
            return "第 \(group) 个分组没有 Spike train 列。"
        case .missingGroupTimeBasis(let group):
            return "第 \(group) 个分组尚未选择时间基准。"
        case .missingEventRelativeOrigin(let group):
            return "第 \(group) 个事件相对时间分组尚未指定事件原点。"
        case .missingSpikeTrainSemanticID(let group, let column):
            return "第 \(group) 个分组的第 \(column) 列尚未填写 Spike-train 语义 ID。"
        case .missingSpikeTrainOrderDecision(let group, let column):
            return "第 \(group) 个分组的第 \(column) 列尚未确认时间戳顺序策略。"
        case .missingSpikeTrainDuplicateDecision(let group, let column):
            return "第 \(group) 个分组的第 \(column) 列尚未确认重复时间戳策略。"
        case .missingEventDefinitionSemanticID(let group, let column):
            return "第 \(group) 个分组的第 \(column) 列尚未填写事件定义 ID。"
        case .missingEventTypeID(let group, let column):
            return "第 \(group) 个分组的第 \(column) 列尚未填写事件类型 ID。"
        case .missingEventOrderDecision(let group, let column):
            return "第 \(group) 个分组的第 \(column) 列尚未确认事件时间戳顺序策略。"
        case .sourceColumnNotAssigned(let column):
            return "第 \(column) 个数据源列尚未分配到分组。"
        case .sourceColumnAssignedMultipleTimes(let column, let count):
            return "第 \(column) 个数据源列被重复分配了 \(count) 次。"
        case .missingEventAttributeScalarType(let definition, let key):
            return "第 \(definition) 个事件属性“\(key.canonicalText)”尚未选择数据类型。"
        case .missingEventAttributeRole(let definition, let key):
            return "第 \(definition) 个事件属性“\(key.canonicalText)”尚未选择科学或展示角色。"
        case .missingEventAttributeUnit(let definition, let key):
            return "第 \(definition) 个事件属性“\(key.canonicalText)”尚未确认单位。"
        case .missingEventAttributeEmptyStringPolicy(let definition, let key):
            return "第 \(definition) 个事件属性“\(key.canonicalText)”尚未确认空字符串策略。"
        case .internalResolutionInvariant:
            return "导入器内部一致性检查失败；活动数据没有改变。"
        default:
            return "当前列、分组或属性决定存在冲突，请返回相应页面检查：\(String(describing: issue))"
        }
    }

    private func planIssueMessages(_ issues: [ScientificImportPlanIssue]) -> [String] {
        var messages: [String] = []
        var missingIDsByGroup: [Int: [Int]] = [:]
        var missingOrderByGroup: [Int: [Int]] = [:]
        var missingDuplicatesByGroup: [Int: [Int]] = [:]

        for issue in issues {
            switch issue {
            case .missingSpikeTrainSemanticID(let group, let column):
                missingIDsByGroup[group, default: []].append(column)
            case .missingSpikeTrainOrderDecision(let group, let column):
                missingOrderByGroup[group, default: []].append(column)
            case .missingSpikeTrainDuplicateDecision(let group, let column):
                missingDuplicatesByGroup[group, default: []].append(column)
            default:
                messages.append(planIssueMessage(issue))
            }
        }

        for group in missingIDsByGroup.keys.sorted() {
            let columns = missingIDsByGroup[group, default: []].sorted()
            messages.append(
                "第 \(group) 个分组有 \(columns.count) 个 Spike-train 列尚未填写语义 ID（列 \(columnList(columns))）。"
            )
        }
        for group in missingOrderByGroup.keys.sorted() {
            let columns = missingOrderByGroup[group, default: []].sorted()
            messages.append(
                "第 \(group) 个分组有 \(columns.count) 个 Spike-train 列尚未确认时间戳顺序策略（列 \(columnList(columns))）。"
            )
        }
        for group in missingDuplicatesByGroup.keys.sorted() {
            let columns = missingDuplicatesByGroup[group, default: []].sorted()
            messages.append(
                "第 \(group) 个分组有 \(columns.count) 个 Spike-train 列尚未确认重复时间戳策略（列 \(columnList(columns))）。"
            )
        }
        return messages
    }

    private func columnList(_ columns: [Int]) -> String {
        if columns.count <= 12 {
            return columns.map(String.init).joined(separator: "、")
        }
        return columns.prefix(12).map(String.init).joined(separator: "、")
            + "等共 \(columns.count) 列"
    }

    private func isDatasetMeaningPlanIssue(_ issue: ScientificImportPlanIssue) -> Bool {
        switch issue {
        case .missingSourceTimeUnit,
             .missingActivityMode,
             .missingRecordingSegmentID,
             .missingRecordingRegime,
             .missingImportedExcerptCoverage,
             .missingObservationBoundsAvailability:
            return true
        default:
            return false
        }
    }

    private func isColumnOrGroupPlanIssue(_ issue: ScientificImportPlanIssue) -> Bool {
        switch issue {
        case .missingGroupSemanticID,
             .groupHasNoSpikeTrains,
             .missingGroupTimeBasis,
             .missingSpikeTrainSemanticID,
             .missingSpikeTrainOrderDecision,
             .missingSpikeTrainDuplicateDecision,
             .missingEventDefinitionSemanticID,
             .missingEventTypeID,
             .missingEventOrderDecision,
             .sourceColumnNotAssigned,
             .sourceColumnAssignedMultipleTimes:
            return true
        default:
            return false
        }
    }

    private func isMissingDuplicateDecision(_ issue: ScientificImportPlanIssue) -> Bool {
        if case .missingSpikeTrainDuplicateDecision = issue { return true }
        return false
    }

    private func attributeKeyErrorMessage(_ error: EventAttributeKeyError) -> String {
        switch error {
        case .sourceTooLong(let maximum), .canonicalValueTooLong(let maximum):
            return "长度超过 \(maximum) 个 UTF-8 字节"
        case .empty: return "不能为空"
        case .blank: return "不能只包含空白字符"
        case .containsControlCharacter: return "不能包含控制字符"
        case .containsMetadataSeparator: return "不能包含保留的等号（=）"
        case .reservedUnitSuffix: return "不能使用保留的 .unit 后缀"
        }
    }

    private func unitSymbolErrorMessage(_ error: OpaqueUnitSymbolError) -> String {
        switch error {
        case .sourceTooLong(let maximum), .canonicalValueTooLong(let maximum):
            return "长度超过 \(maximum) 个 UTF-8 字节"
        case .empty: return "不能为空"
        case .blank: return "不能只包含空白字符"
        case .containsControlCharacter: return "不能包含控制字符"
        }
    }

    private func stepHeader(_ title: String, subtitle: String) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.title2.weight(.semibold))
            Text(subtitle)
                .foregroundStyle(.secondary)
        }
    }

    private func errorBanner(_ message: String) -> some View {
        Label(message, systemImage: "xmark.octagon.fill")
            .foregroundStyle(.red)
            .padding(12)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.red.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
    }

    private func previewCell(_ text: String, isHeader: Bool, width: CGFloat = 160) -> some View {
        Text(text)
            .font(isHeader ? .caption.weight(.semibold) : .caption.monospaced())
            .lineLimit(1)
            .truncationMode(.middle)
            .frame(width: width, height: 28, alignment: .leading)
            .padding(.horizontal, 6)
            .background(isHeader ? Color.secondary.opacity(0.10) : Color.clear)
            .overlay(Rectangle().stroke(Color.secondary.opacity(0.16), lineWidth: 0.5))
    }

    private func stagedCellText(_ value: StagedCellValue) -> String {
        switch value {
        case .blank:
            return ""
        case .text(let rawText):
            return rawText
        case .spreadsheetNumber(let rawLexeme):
            return rawLexeme
        }
    }

    private func visibilityLabel(_ visibility: CanonicalXLSXWorksheetVisibility) -> String {
        switch visibility {
        case .visible: "可见"
        case .hidden: "隐藏"
        case .veryHidden: "深度隐藏"
        }
    }

    private func attributeTypeLabel(_ type: EventAttributeScalarType) -> String {
        switch type {
        case .string: "字符串"
        case .integer: "整数"
        case .exactDecimal: "精确小数"
        case .boolean: "布尔值"
        }
    }

    private var activityModeExplanation: String {
        switch coordinator.manifestForm?.activityMode {
        case .putativeSingleUnit:
            "您已明确确认本文件代表单神经元活动；这项选择决定后续分析方式，但本身不等同于软件已经证明 spike sorting 或单神经元分离质量。"
        case .intentionalMultiUnit:
            "仅作为实验性占位；不会复用单神经元活动的阈值和检测器。"
        case .unknownOrUncertain:
            "可以审核该表格，但不得产生确定性的生物学解释或权威检测器输出。"
        case .none:
            "请明确选择采集含义；同一个文件不能按列混用不同活动模式。"
        }
    }

    private var duplicateDecisionHelp: String {
        switch coordinator.manifestForm?.activityMode {
        case .putativeSingleUnit:
            "规范原始数据始终保留每个重复时间戳。请选择后续单神经元活动分析视图是否可以虚拟折叠完全重复值；数据源和规范数据中的事件重数绝不会被删除。"
        case .intentionalMultiUnit, .unknownOrUncertain:
            "规范原始数据会保留事件重数。多神经元活动和不确定数据不能借用单神经元活动的虚拟折叠策略。"
        case .none:
            "请先选择活动模式，再确认完全重复时间戳的表示方式。"
        }
    }

    private var phaseDescription: String {
        switch coordinator.phase {
        case .idle: "当前没有导入审核。"
        case .readingSource: "正在读取确切且受限的数据源快照…"
        case .awaitingSourceDecisions: "正在等待明确的阶段 A 决定。"
        case .stagingSource: "正在绑定所选表格…"
        case .reviewingScientificMeaning: "数据源事实已绑定；科学选择仍为草稿。"
        case .preflightingSourceFacts: "正在使用 Core 语法扫描精确时间戳和事件元数据…"
        case .validatingScientificMeaning: "正在执行精确规范化和独立验证…"
        case .validatedPreparation:
            canProceedToCanonicalManualAnalysis
                ? "确认清单已保存；可以进入人工 ISI 标记与分析。"
                : "准备值已验证；自动检测权限仍处于锁定状态。"
        case .failed: "导入未能完成；活动数据集没有改变。"
        }
    }

    private var previousStep: ScientificImportReviewStep? {
        guard let index = ScientificImportReviewStep.allCases.firstIndex(of: selectedStep), index > 0 else {
            return nil
        }
        return ScientificImportReviewStep.allCases[index - 1]
    }

    private var nextStep: ScientificImportReviewStep? {
        guard let index = ScientificImportReviewStep.allCases.firstIndex(of: selectedStep),
              index + 1 < ScientificImportReviewStep.allCases.count else {
            return nil
        }
        return ScientificImportReviewStep.allCases[index + 1]
    }

    private func isStepAvailable(_ step: ScientificImportReviewStep) -> Bool {
        step == .source || coordinator.stagedImport != nil
    }

    private var headerDecisionBinding: Binding<CanonicalTabularHeaderDecision?> {
        Binding(
            get: { coordinator.headerDecision },
            set: { decision in
                if let decision { coordinator.selectHeaderDecision(decision) }
            }
        )
    }

    private var worksheetBinding: Binding<UInt32?> {
        Binding(
            get: { coordinator.selectedWorksheetSheetID },
            set: { sheetID in
                if let sheetID { coordinator.selectWorksheet(sheetID: sheetID) }
            }
        )
    }

    private var sourceTimeUnitBinding: Binding<String> {
        Binding(
            get: { coordinator.manifestForm?.sourceTimeUnit?.rawValue ?? "" },
            set: { rawValue in
                mutateForm { form in
                    form.sourceTimeUnit = SpikeTimeUnit(rawValue: rawValue)
                }
            }
        )
    }

    private var activityModeBinding: Binding<ScientificDatasetActivityMode?> {
        Binding(
            get: { coordinator.manifestForm?.activityMode },
            set: { value in mutateForm { $0.selectActivityMode(value) } }
        )
    }

    private var recordingSegmentIDBinding: Binding<String> {
        Binding(
            get: { coordinator.manifestForm?.recordingSegmentIDText ?? "" },
            set: { value in mutateForm { $0.recordingSegmentIDText = value } }
        )
    }

    private var recordingRegimeBinding: Binding<ScientificRecordingRegime?> {
        Binding(
            get: { coordinator.manifestForm?.recordingRegime },
            set: { value in mutateForm { $0.recordingRegime = value } }
        )
    }

    private var importedExcerptCoverageBinding: Binding<ImportedExcerptCoverage?> {
        Binding(
            get: { coordinator.manifestForm?.importedExcerptCoverage },
            set: { value in mutateForm { $0.importedExcerptCoverage = value } }
        )
    }

    private var observationBoundsConfirmedBinding: Binding<Bool> {
        Binding(
            get: { coordinator.manifestForm?.observationBoundsConfirmedUnavailable ?? false },
            set: { value in mutateForm { $0.observationBoundsConfirmedUnavailable = value } }
        )
    }

    private func columnBoundaryBinding(_ id: UUID) -> Binding<Bool?> {
        columnOptionalBinding(id, \.startsNewGroup)
    }

    private func columnTextBinding(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportColumnDecisionForm, String>
    ) -> Binding<String> {
        Binding(
            get: { columnForm(id: id)?[keyPath: keyPath] ?? "" },
            set: { value in
                mutateForm { form in
                    form.updateColumn(id: id) { column in
                        column[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func columnOptionalBinding<Value: Hashable>(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportColumnDecisionForm, Value?>
    ) -> Binding<Value?> {
        Binding(
            get: { columnForm(id: id)?[keyPath: keyPath] },
            set: { value in
                mutateForm { form in
                    form.updateColumn(id: id) { column in
                        column[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func attributeTextBinding(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportAttributeDecisionForm, String>
    ) -> Binding<String> {
        Binding(
            get: { attributeForm(id: id)?[keyPath: keyPath] ?? "" },
            set: { value in
                mutateForm { form in
                    form.updateAttribute(id: id) { attribute in
                        attribute[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func attributeOptionalBinding<Value: Hashable>(
        _ id: UUID,
        _ keyPath: WritableKeyPath<ScientificImportAttributeDecisionForm, Value?>
    ) -> Binding<Value?> {
        Binding(
            get: { attributeForm(id: id)?[keyPath: keyPath] },
            set: { value in
                mutateForm { form in
                    form.updateAttribute(id: id) { attribute in
                        attribute[keyPath: keyPath] = value
                    }
                }
            }
        )
    }

    private func columnForm(id: UUID) -> ScientificImportColumnDecisionForm? {
        coordinator.manifestForm?.columns.first(where: { $0.id == id })
    }

    private func attributeForm(id: UUID) -> ScientificImportAttributeDecisionForm? {
        coordinator.manifestForm?.attributes.first(where: { $0.id == id })
    }

    private func originCandidateLocationText(
        _ candidate: ScientificImportEventOriginCandidate
    ) -> String {
        let eventID = candidate.eventDefinitionID?.semanticID.canonicalText
            ?? "未解析事件 ID"
        return "分组 \(candidate.groupIndex) · \(eventID) · 列 \(candidate.sourceColumn.oneBasedIndex) · 数据行 \(candidate.timestampCell.oneBasedDataRowIndex) · \(candidate.sourceTick.microseconds) µs"
    }

    private func originCandidateAttributesText(
        _ candidate: ScientificImportEventOriginCandidate
    ) -> String {
        candidate.scientificAttributes.map { attribute in
            let prefix = String(attribute.rawValue.prefix(80))
            let suffix = attribute.rawValue.count > prefix.count ? "…" : ""
            return "\(attribute.key.canonicalText)=\(prefix)\(suffix)"
        }
        .joined(separator: ", ")
    }

    private func applyOriginCandidate(_ candidate: ScientificImportEventOriginCandidate) {
        guard coordinator.preflightReport?.eventOriginCandidates.contains(candidate) == true else {
            return
        }
        mutateForm { form in
            let groupStartIndices = form.columns.indices.filter { index in
                index == 0 || form.columns[index].startsNewGroup == true
            }
            let groupOffset = candidate.groupIndex - 1
            guard groupStartIndices.indices.contains(groupOffset) else { return }
            let startIndex = groupStartIndices[groupOffset]
            let groupStartID = form.columns[startIndex].id
            form.updateColumn(id: groupStartID) { column in
                column.groupTimeBasis = .eventRelative
                column.eventOriginColumnText = String(candidate.sourceColumn.oneBasedIndex)
                column.eventOriginDataRowText =
                    String(candidate.timestampCell.oneBasedDataRowIndex)
            }
        }
    }

    private func mutateForm(_ mutation: (inout ScientificImportManifestForm) -> Void) {
        guard var form = coordinator.manifestForm else { return }
        mutation(&form)
        coordinator.manifestForm = form
    }
}
