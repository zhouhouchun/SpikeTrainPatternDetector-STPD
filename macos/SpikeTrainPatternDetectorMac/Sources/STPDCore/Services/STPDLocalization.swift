import Foundation

/// UI language for the app. Default is Chinese (`zh`), mirroring the R/Shiny design where the source
/// strings are Chinese and English is a switchable translation. This is a DISPLAY concern only — it never
/// changes internal machine values (enum raw values, candidate/train IDs, CSV columns, finalLabel strings,
/// decision paths, export schemas).
public enum STPDLanguage: String, Sendable, CaseIterable, Hashable, Identifiable {
    case zh
    case en
    case ru

    public var id: String { rawValue }

    /// The label shown for this language in the language switch (always its own endonym, not translated).
    public var nativeLabel: String {
        switch self {
        case .zh: return "中文"
        case .en: return "English"
        case .ru: return "Русский"
        }
    }
}

/// Pure, centralized localization helper mirroring the R/Shiny client-side translator
/// (`R/56_i18n.R`: `stpd_i18n_exact_dictionary` + `stpd_i18n_phrase_dictionary`).
///
/// Convention: call sites pass the **Chinese source** string. In `zh` mode the source is returned verbatim;
/// in non-Chinese modes the source is translated by an exact full-string lookup, then (if no exact hit) by
/// ordered phrase substitution. Russian falls back to the existing English translation for an uncovered key,
/// rather than leaving a mixed Chinese interface.
///
/// Strict non-goal: never route machine strings through here — only user-visible display text.
public enum STPDLocalization {
    /// Translate a Chinese source string for the given language. `zh` returns the source unchanged.
    public static func text(_ source: String, language: STPDLanguage) -> String {
        switch language {
        case .zh:
            return source
        case .en:
            return translated(source, exact: exactDictionary, phrases: phraseEntries) ?? source
        case .ru:
            return translated(source, exact: russianExactDictionary, phrases: russianPhraseEntries)
                ?? translated(source, exact: exactDictionary, phrases: phraseEntries)
                ?? source
        }
    }

    private static func translated(
        _ source: String,
        exact: [String: String],
        phrases: [(zh: String, en: String)]
    ) -> String? {
        if let exact = exact[source] { return exact }
        var out = source
        var replaced = false
        for entry in phrases where out.contains(entry.zh) {
            out = out.replacingOccurrences(of: entry.zh, with: entry.en)
            replaced = true
        }
        return replaced ? out : nil
    }

    // MARK: - Exact dictionary (full-string Chinese source -> English). R `stpd_i18n_exact_dictionary`.

    public static let exactDictionary: [String: String] = [
        // Language switch + top-level workbench groups/sections (R-parity where present in R/56_i18n.R).
        "语言 / Language": "Language",
        "中文": "Chinese",
        "确定的单神经元电活动": "Confirmed single-unit activity",
        "确定的单神经元电活动（Single-unit）": "Confirmed single-unit activity",
        "多神经元电活动": "Multi-unit activity",
        "多神经元电活动（Multi-unit，实验性）": "Multi-unit activity (experimental)",
        "未知或不确定": "Unknown or uncertain",
        "主图": "Primary views",
        "状态与流形": "State & manifold",
        "诊断": "Diagnostics",
        "验证": "Validation",
        "参数": "Parameters",
        "输出": "Outputs",
        "更多": "More",
        "功能": "Features",
        "左侧栏": "Left sidebar",
        "右侧栏": "Right sidebar",
        "打开结果包": "Open result package",
        "打开人工审核结果包": "Open manual-review result package",
        "导出事件 CSV": "Export events CSV",
        "导出逐 ISI 审核草稿": "Export per-ISI review draft",
        "导出 HFS-Burst 审计 CSV": "Export HFS–Burst audit CSV",
        "导入人工标注": "Import manual annotations",
        "导入审核结果": "Import review results",
        // Phase 1 IA: live-module sidebar groups
        "数据": "Data",
        "棘波图": "Raster",
        "模式检测": "Pattern detection",
        "导入数据": "Import data",
        "数据导入 / QC": "Data import / QC",
        "数据导入与 QC": "Data import & QC",
        "结构检测": "Structure detection",
        "spike train 光栅图": "Spike-train raster",
        "状态与流形（后续分析）": "State & manifold (downstream analysis)",
        "时间戳图": "Timestamp plot",
        "Spike train ISI 热力图": "Spike-train ISI heatmap",
        "Spike train 模式热力图": "Spike-train pattern heatmap",
        "ISI 热力图": "ISI heatmap",
        "模式热力图": "Pattern heatmap",
        "合并": "Combined",
        "手工": "Manual",
        "状态 · 事件 · 其它": "State · event · other",
        "状态、事件与其它标记分轨显示；不进行颜色插值。":
            "States, events, and other marks are rendered on separate tracks; colors are not interpolated.",
        "模式图例": "Mode legend",
        "手工标记在同一轨道内覆盖自动显示，但不会删除自动检测记录。":
            "Manual marks override automatic display within the same track without deleting detector evidence.",
        "对齐时间戳图": "Aligned timestamp plot",
        "原始时间戳图": "Raw timestamp plot",
        "目标核团深度图": "Target-nucleus depth plot",
        "ISI 时间剖面": "ISI time profile",
        "ISI 状态空间": "ISI state space",
        "ISI 状态空间分析": "ISI state-space analysis",
        "事件对齐活动": "Event-Aligned Activity",
        "神经流形": "Neural Manifold",
        "区间直方图": "Interval histogram",
        "数据集 ISI 直方图": "Dataset ISI histogram",
        "结构候选": "Structure candidates",
        "阈值预览": "Threshold preview",
        "支持方法": "Support methods",
        "手动标记 vs 检测器报告": "Manual labels vs detector report",
        "手工 ISI 标记与审核": "Manual ISI labeling & review",
        "科学验证": "Scientific validation",
        "批处理 / API": "Batch / API",
        "方法 / 审计说明": "Methods / audit notes",
        "检测器 / 参数": "Detector / parameters",
        "模拟 / 预览": "Simulator / Preview",
        "自适应 train 调参": "Adaptive train tuning",
        "神经网络模型": "Neural-network model",
        "数据 QC": "Data QC",
        "检测结果科学审核": "Scientific review of detection results",
        "结果包回读": "Result-package readback",
        "事件 / 输出": "Events / output",
        "Seed / Bridge 诊断": "Seed / Bridge diagnostics",

        // Pattern legend names (capitalized for UI legends; R uses lowercase inline forms in the phrase dict).
        "爆发": "Burst",
        "暂停": "Pause",
        "强直发放": "Tonic",
        "高频强直发放": "HF tonic",
        "高频连续发放": "HF spiking",
        "其他": "Others",
        "未标注": "Unlabeled",
        "非爆发 / 强负例": "Not burst (veto)",

        // Common section sub-headers / controls used by the localized first-pass surfaces.
        "群体轨迹": "Population trajectory",
        "时间范围": "time range",
        "全时长": "full duration",
        "颜色": "Color",
        "显示": "Display",
        "坐标轴": "Axes",
        "时间": "Time",
        "对齐": "Aligned",
        "对齐时间": "Aligned time",
        "原始时间戳": "Raw timestamp",
        "自动": "Auto",
        "最终": "Final",
        "训练选择": "Train selection",
        "全选": "Select all",
        "清空": "Clear",
        "反选": "Invert",
        "数据集": "Dataset",
        "重置": "Reset",
        "图例": "Legend",
        "最大": "Max.",
        "数据源": "Source",
        "spike 光栅图": "spike raster",
        "对齐 spike 光栅图": "Aligned spike raster",
        "原始 spike 光栅图": "Raw spike raster",
        "Spike 高度": "Spike height",
        "ISI 分辨率约为": "Estimated ISI resolution",
        "细节层级上限": "detail-level limit",
        "重置缩放": "Reset zoom",
        "热力图清晰度": "Heatmap resolution",
        "预设": "Preset",
        "探索": "Explore",
        "发表": "Publication",
        "探索：Jet 色图；当前视图稳健 5–95% 范围。":
            "Explore: Jet palette; robust 5th–95th percentile range for the current view.",
        "发表：Cividis 色图；全数据集固定数值范围。":
            "Publication: Cividis palette; fixed numeric range for the full dataset.",
        "颜色范围": "Colour scale",
        "当前视图 5–95% 稳健范围": "Current-view robust 5th–95th percentile range",
        "全数据集固定范围": "Full-dataset fixed range",
        "裁剪": "Clipped",
        "个时间格": "time cells",
        "颜色由局部 ISI 强度决定：短 ISI 为暖色，长 ISI 为冷色。":
            "Color encodes local ISI intensity: short ISIs are warm; long ISIs are cool.",
        "长 ISI / 低局部频率": "Long ISI / low local frequency",
        "短 ISI / 高局部频率": "Short ISI / high local frequency",
        "请先在时间戳图中选择至少一条 spike train。":
            "Select at least one spike train in the timestamp plot first.",
        "设置光栅视口中可见的时间跨度；旁边显示估算的 ISI 分辨率。":
            "Set the visible time span in the raster viewport; the estimated ISI resolution appears alongside it.",
        "以像素控制每个 spike 标记的垂直高度；最大值随当前轨道高度变化。":
            "Control the vertical height of each spike marker in pixels; the maximum follows the current track height.",
        "尚未加载光栅图": "Raster not loaded",
        "目前没有可用的 spike train。": "No spike trains are available.",
        "在时间戳图上手工标记": "Annotate on raster",
        "开启后，在同一条 spike train 的时间戳行内拖动即可标记连续 ISI。":
            "When enabled, drag within one spike-train row to label a contiguous ISI range.",
        "显示 ISI 信息栏": "Show ISI information panel",
        "关闭后不再绘制鼠标悬停的 ISI 信息栏，可提升手工标记时的流畅度。":
            "When off, the hover ISI information panel is not drawn, improving manual-labeling responsiveness.",
        "操作": "Action",
        "标记": "Label",
        "擦除": "Erase",
        "清除轨道": "Clear track",
        "撤销上一步": "Undo last step",
        "在同一行拖动，仅清除所选轨道；共存轨道会保留。":
            "Drag within one row to clear only the selected track; coexisting tracks are retained.",
        "拖动范围必须保持在同一行；黑色 spike 始终保留可见。":
            "Keep a drag range within one row; black spike marks always remain visible.",
        "手工标记": "Manual labels",
        "纯手工 ISI 标记（无需模式检测）": "Manual ISI labeling (no pattern detection)",
        "本地草稿 · 未封存": "Local draft · unsealed",
        "身份绑定草稿": "Identity-bound draft",
        "审核已确认": "Review confirmed",
        "导出已标注 ISI": "Export labeled ISIs",
        "导入手工 ISI 草稿": "Import manual ISI draft",
        "导出 CSV / XLSX / NEX": "Export CSV / XLSX / NEX",
        "确认完整审核": "Confirm complete review",
        "导出 .stpdresult": "Export .stpdresult",
        "尚未加载 spike train": "No spike train loaded",
        "请先导入或打开数据集，再进行 ISI 人工标注。":
            "Import or open a dataset before manually labeling ISIs.",
        "规范人工审核不可用": "Canonical manual review unavailable",
        "已持久化的导入尚不具备规范人工模式审核资格。":
            "The persisted import is not yet eligible for canonical manual pattern review.",
        "时间图和表格都只使用原始 spike 时间戳，不运行模式检测。可在时间图拖动选择连续 ISI，也可在表格精确多选。状态与事件相互独立，因此 HFS 可与其内嵌的 HFB 共存；可选择导出 CSV、XLSX 或 NeuroExplorer NEX，文件会明确标记为本地人工草稿。":
            "The timeline and table use only the original spike timestamps; pattern detection is not run. Drag on the timeline to select contiguous ISIs or select exact rows in the table. States and events remain independent, so HFS may coexist with an embedded HFB. Export is available as CSV, XLSX, or NeuroExplorer NEX, and the file is explicitly identified as a local manual draft.",
        "时间图和表格都直接来自已持久化的规范整数微秒数据集，不读取任何自动检测候选。可在时间图拖动选择连续 ISI，也可在表格精确多选；任何阶段均可选择导出 CSV、XLSX 或 NeuroExplorer NEX，未标记项会保留为空。只有确认完整审核并导出 .stpdresult 时，才要求每个 ISI 都归入状态、事件或其他。状态与事件保持独立，因此 HFS 可与内嵌 HFB 共存。":
            "The timeline and table come directly from the persisted canonical integer-microsecond dataset and do not read automatic detector candidates. Drag on the timeline to select contiguous ISIs or select exact rows in the table. CSV, XLSX, or NeuroExplorer NEX may be exported at any stage, with unlabeled entries left blank. Every ISI must be assigned to State, Event, or Other only when confirming the complete review and exporting .stpdresult. States and events remain independent, so HFS may coexist with an embedded HFB.",
        "轨道": "Track",
        "清除选择": "Clear selection",
        "使用 Shift-单击或 Command-单击进行批量选择。":
            "Use Shift-click or Command-click for multiple selection.",
        "左时间戳（s）": "Left timestamp (s)",
        "左 MM": "Left MM",
        "左 MM = 左邻 ISI ÷ 当前 ISI": "Left MM = preceding ISI ÷ current ISI",
        "当前 ISI（ms）": "Current ISI (ms)",
        "右 MM": "Right MM",
        "右 MM = 右邻 ISI ÷ 当前 ISI": "Right MM = following ISI ÷ current ISI",
        "右时间戳（s）": "Right timestamp (s)",
        "备注": "Note",
        "审核 / ISI #": "Review / ISI #",
        "已审核": "Reviewed",
        "未审核": "Unreviewed",
        "时间图手工选择": "Manual selection on timeline",
        "拖动选择连续 ISI；点击已标记色块后按 Delete 可清除该区块。":
            "Drag to select contiguous ISIs; click a labeled block and press Delete to clear it.",
        "鼠标悬停在相邻 spike 之间时显示该 ISI 的时间戳、间隔与模式信息":
            "Hover between adjacent spikes to show timestamps, interval, and pattern information for that ISI",
        "适合窗口": "Fit to window",
        "Spike": "Spikes",
        "手工 ISI 时间图": "Manual ISI timeline",
        "拖动以选择连续 ISI；点击已标记色块后按 Delete 清除该区块":
            "Drag to select contiguous ISIs; click a labeled block and press Delete to clear it",
        "未标记": "Unlabeled",
        "ISI 阈值初标": "ISI threshold-assisted preliminary labeling",
        "人工规则 · 即时预览 · 不运行检测器": "Manual rule · live preview · detector not run",
        "ISI 闭区间": "Closed ISI range",
        "Spike 数闭区间": "Closed spike-count range",
        "连续 n 个 ISI 对应 n+1 个 spike；超出 spike 上限的整段会被跳过，不会被切成人工 Burst 小包。":
            "n consecutive ISIs represent n+1 spikes; an entire run above the spike limit is skipped rather than chopped into artificial Burst packets.",
        "指标": "Metric",
        "指标闭区间": "Closed metric range",
        "不限": "No limit",
        "基于 CV/CV2/LV 的 Tonic 初标至少需要 5 个 ISI（6 个 spike）；更短片段仍由用户按结构证据审核。":
            "CV/CV2/LV-based preliminary Tonic labeling requires at least 5 ISIs (6 spikes); shorter spans remain available for explicit structural review.",
        "MM = 候选段内最大 ISI / 最小 ISI；仅用于 3–5 个 spike 的短 Tonic 初标。":
            "MM = maximum ISI / minimum ISI within the candidate; it is used only for preliminary short-Tonic labeling with 3–5 spikes.",
        "CV/CV2/LV 仅用于至少 5 个 ISI（6 个 spike）的 Tonic 初标；3–5 个 spike 请改用 MM。":
            "CV/CV2/LV are used only for preliminary Tonic labeling with at least 5 ISIs (6 spikes); use MM for 3–5 spikes.",
        "选中预览": "Select preview",
        "应用初标": "Apply preliminary labels",
        "撤销上一批": "Undo last batch",
        "初标未写入；请查看状态信息，确认当前 spike train、最小 spike 数与已有标记。":
            "Preliminary labels were not written. Check the status message and verify the current spike train, minimum spike count, and existing labels.",
        "正在运行模式检测并构建审计结果…": "Running pattern detection and building audit results…",
        "正在导入、校验并绑定手工标记…": "Importing, validating, and binding manual annotations…",
        "正在读取并验证规范手工 ISI 草稿…": "Reading and validating the canonical manual ISI draft…",
        "正在构建、校验并导出结果包…": "Building, validating, and exporting the result package…",
        "正在读取并完整校验结果包…": "Reading and fully validating the result package…",
        "正在保存或恢复并校验科学导入合同…": "Saving or restoring and validating the scientific-import contract…",
        "正在计算 Isomap 神经流形…": "Computing the Isomap neural manifold…",
        "正在计算 PHATE 神经流形…": "Computing the PHATE neural manifold…",
        "正在读取并限定数据源快照…": "Reading and bounding the data-source snapshot…",
        "正在解析表格并构建导入预备数据…": "Parsing the table and building staged import data…",
        "正在检查数据源事实与事件属性…": "Checking source facts and event attributes…",
        "正在验证科学含义与规范数据身份…": "Validating scientific meaning and canonical dataset identity…",
        "正在生成 ISI 热力图…": "Generating the ISI heatmap…",
        "正在生成模式热力图…": "Generating the pattern heatmap…",
        "正在生成模拟数据与预览结果…": "Generating simulated data and preview results…",
        "正在生成当前检测的权威结果表…": "Building authoritative tables for the current detection run…",
        "正在构建结果表的语义化审阅视图…": "Building the semantic review view for the result tables…",
        "请输入有效的 ISI 闭区间上下限。": "Enter valid bounds for the closed ISI range.",
        "请输入有效的 Burst spike 数闭区间。": "Enter a valid closed Burst spike-count range.",
        "基于 CV/CV2/LV 的 Tonic 初标至少需要 6 个 spike。":
            "CV/CV2/LV-based preliminary Tonic labeling requires at least 6 spikes.",
        "MM Tonic 初标仅支持 3–5 个 spike，且 MM 闭区间不能小于 1。":
            "MM-based preliminary Tonic labeling supports only 3–5 spikes, and the closed MM range cannot be below 1.",
        "请输入有效的 Tonic spike 数和指标闭区间。": "Enter a valid Tonic spike count and closed metric range.",
        "手工标记 QC": "Manual-labeling QC",
        "当前 spike train 未发现绝对无效 ISI。": "No absolutely invalid ISI was found in the current spike train.",
        "仅凭时间戳无法判定两端哪一个 spike 错误；红色表示参与可疑间隔，不是单个 spike 的伪迹定论。阈值初标会排除这些 ISI。":
            "Timestamps alone cannot identify which endpoint spike is erroneous. Red indicates involvement in a suspect interval, not a definitive artifact verdict for one spike. Threshold-assisted labeling excludes these ISIs.",
        "绝对无效 ISI 上限": "Absolutely invalid ISI upper limit",
        "高频 Burst（HFB）": "High-frequency Burst (HFB)",
        "长 Burst": "Long Burst",
        "高频 Tonic": "High-frequency Tonic",
        "高频持续发放（HFS）": "High-frequency spiking (HFS)",
        "非 Burst（否决）": "Not Burst (veto)",
        "悬停约2秒或单击切换；也可按住当前玻璃块拖到目标功能后松开。":
            "Hover for about 2 seconds or click to switch; you can also drag the current glass control to a destination and release.",

        // Spike-train selector popover.
        "已选择": "Selected",
        "搜索 spike train": "Search spike trains",
        "没有匹配的 spike train": "No matching spike train",
        "请换一个关键词，或清空搜索内容。": "Try a different keyword, or clear the search.",
        "Spike 数": "Spikes",
        "时长": "Duration",
        "起点": "Start",
        "终点": "End",
        "发放率": "Firing rate",
        "Spike Train 选择器": "Spike-train selector",
        "ISI Spike Train 选择器": "ISI spike-train selector",
        "ISI 状态空间 Spike Train 选择器": "ISI state-space spike-train selector",
        "神经流形 Spike Train 选择器": "Neural Manifold spike-train selector",

        // Empty-state / status messages.
        "神经流形未加载": "Neural Manifold not loaded",
        "先加载 spike train 数据集。": "Load a spike-train dataset first.",
        "请至少上传一个数据集。": "Please upload at least one dataset.",
        "请先加载 spike train 数据。": "Please load spike-train data first.",
        "选择不足": "Insufficient selection",
        "请选择至少两条 spike train 才能构建群体流形。":
            "Select at least two spike trains to build the population manifold.",
        "暂无可预览的 spike train。": "No spike trains to preview.",
        "无状态空间点": "No state-space points",

        // I18N-2B: spike-train selection-scope short titles (display-only labels).
        "Spike 序列": "Spike trains",
        "ISI 序列": "ISI trains",
        "状态空间序列": "State-space trains",
        "流形序列": "Manifold trains",

        // I18N-2A: spike-train count controls.
        "棘波序列": "Spike train",
        "条": "trains",
        "全部": "All",
        "选择": "Select",
        "打开详细 spike train 选择器。": "Open the detailed spike-train selector.",
        "选择前 N 条 spike train 显示；右侧 raster 会显示当前可见 train 名称。":
            "Show the first N spike trains; the raster on the right lists the currently visible train names.",
        "选择 ISI 时间剖面中显示的 spike train；该选择与时间戳图互不影响。":
            "Choose the spike trains shown in the ISI time profile; this selection is independent of the timestamp plots.",
        "选择 ISI 状态空间中显示的 spike train；该选择与 raster 和 ISI 时间剖面互不影响。":
            "Choose the spike trains shown in the ISI state space; this selection is independent of the raster and the ISI time profile.",
        "选择参与群体神经流形（PCA）的 spike train；该选择与 raster、ISI 时间剖面和 ISI 状态空间互不影响。":
            "Choose the spike trains that feed the population Neural Manifold (PCA); this selection is independent of the raster, ISI time profile, and ISI state space.",

        // I18N-2A: Neural Manifold control labels, toggles, legend, summary panels, empty states, help.
        "实时": "Live",
        "视图 / 坐标轴": "View / axes",
        "显示与样式": "Display + style",
        "时间 / 分箱": "Time / binning",
        "分箱与原点": "Bin + origin",
        "分箱": "Bin",
        "分箱数": "Bins",
        "分箱大小": "Bin size",
        "群体分箱": "Population bin",
        "原始区间": "Raw range",
        "对齐区间": "Aligned range",
        "平滑与连线范围": "Smoothing + line range",
        "信号 / PCA 输入": "Signal / PCA input",
        "变换": "Transform",
        "缩放": "Scaling",
        "缩放与重置": "Scaling + reset",
        "连线": "Line",
        "时间着色": "Time color",
        "原点": "Origin",
        "事件": "Events",
        "关闭": "off",
        "自动模式": "Auto pattern",
        "最终模式": "Final pattern",
        "早": "early",
        "晚": "late",
        "统一": "uniform",
        "摘要": "Summary",
        "矩阵 (bins × trains)": "Matrix (bins × trains)",
        "事件状态占比 (最终)": "Event-state occupancy (Final)",
        "尚无最终标注。": "No final labels yet.",
        "方法 / 嵌入": "Method / embedding",
        "嵌入方法": "Embedding method",
        "方法": "Method",
        "Isomap 参数": "Isomap settings",
        "仅 Isomap 适用。": "Isomap only.",
        "断开处理": "Disconnected",
        "邻居数": "Neighbors",
        "最大嵌入分箱": "Max embedded bins",
        "选择降维方法：PCA（线性基线）或 Isomap（测地流形）。坐标始终来自群体活动矩阵，从不使用检测标签。":
            "Choose the dimensionality method: PCA (linear baseline) or Isomap (geodesic manifold). Coordinates always come from the population activity matrix, never detector labels.",
        "Isomap 的 kNN 邻居数 k；以及 kNN 图断开时的处理：保留最大连通分量，或报错。":
            "Isomap kNN neighbor count k; and how a disconnected kNN graph is handled: keep the largest component, or error.",
        "Isomap 的 kNN 邻居数 k；最大嵌入分箱对应 R max_points，超过时按时间均匀抽样；断开处理可保留最大连通分量，或报错。":
            "Isomap kNN neighbor count k; Max embedded bins mirrors R max_points and evenly samples over time when exceeded; disconnected graphs can keep the largest component or error.",
        "Isomap 诊断": "Isomap diagnostics",
        "嵌入分箱": "Embedded bins",
        "连通分量": "Components",
        "神经元特征": "Neuron features",
        "残差方差": "Residual variance",
        "采样分箱": "Sampled bins",
        "暂无法计算嵌入": "Cannot compute the embedding yet",
        "正在计算 Isomap…": "Computing Isomap…",
        "无法计算 Isomap 嵌入。": "Could not compute the Isomap embedding.",
        "Isomap 至少需要五个群体时间分箱。": "Isomap needs at least five population time bins.",
        "Isomap 至少需要两个神经元特征。": "Isomap needs at least two neuron features.",
        "Isomap kNN 图不连通。请增大邻居数，或将“断开处理”设为“最大连通分量”。":
            "The Isomap kNN graph is disconnected. Increase Neighbors, or set Disconnected to Largest.",
        "最大 Isomap 连通分量的分箱过少。请增大邻居数。":
            "The largest Isomap component has too few bins. Increase Neighbors.",
        "正在计算 PHATE…": "Computing PHATE…",
        "无法计算 PHATE 嵌入。": "Could not compute the PHATE embedding.",
        "PHATE 至少需要五个群体时间分箱。": "PHATE needs at least five population time bins.",
        "PHATE 至少需要两个神经元特征。": "PHATE needs at least two neuron features.",
        "PHATE 扩散势退化，无法计算嵌入。":
            "The PHATE diffusion potential is degenerate; no embedding can be computed.",
        "扩散时间": "Diffusion time",
        "PCA 参数": "PCA parameters",
        "PHATE 参数": "PHATE parameters",
        "PCA 无需额外参数。": "PCA needs no extra parameters.",
        "PHATE 诊断": "PHATE diagnostics",
        "核带宽 ε": "Kernel bandwidth ε",
        "后端": "Backend",
        "扩散势 + 经典 MDS": "Diffusion potential + classical MDS",
        "选择降维方法：PCA（线性基线）、Isomap（测地流形）、PHATE（扩散势）、FA（因子分析）或 GPFA（平滑因子轨迹）。坐标始终来自群体活动矩阵，从不使用检测标签。":
            "Choose the dimensionality-reduction method: PCA (linear baseline), Isomap (geodesic manifold), PHATE (diffusion potential), FA (factor analysis), or GPFA (smoothed factor trajectory). Coordinates always come from the population activity matrix, never from detection labels.",
        "Isomap：kNN 邻居数 k 与断开处理；PHATE：扩散邻居数与扩散时间 t；FA / GPFA：无额外参数（GPFA 复用平滑 σ）。最大嵌入分箱仅用于 Isomap/PHATE，FA/GPFA 嵌入全部分箱。":
            "Isomap: kNN neighbor count k and disconnected handling; PHATE: diffusion neighbor count and diffusion time t; FA / GPFA: no extra parameters (GPFA reuses the smoothing σ). Max embedded bins applies only to Isomap/PHATE — FA/GPFA embed every bin.",
        // NM-3D: FA / GPFA-style
        "FA 参数": "FA parameters",
        "GPFA 参数": "GPFA parameters",
        "FA 无需额外参数。": "FA needs no extra parameters.",
        "GPFA 使用上方的平滑 σ 进行额外的 FA 预平滑。":
            "GPFA-style uses the smoothing σ above for the extra FA pre-smoothing.",
        "因子分析诊断": "Factor analysis diagnostics",
        "输入特征": "Input features",
        "保留特征": "Kept features",
        "丢弃特征": "Dropped features",
        "因子数": "Factors",
        "平均唯一度": "Mean uniqueness",
        "唯一度": "Uniqueness",
        "Top 载荷 (|F1|+|F2|)": "Top loadings (|F1|+|F2|)",
        "FA 至少需要三个群体时间分箱。": "FA needs at least three population time bins.",
        "FA 至少需要三个有效神经元特征。": "FA needs at least three usable neuron features.",
        "分箱过少，无法识别该因子数的 FA 模型。": "Too few bins to identify the FA model for this factor count.",
        "无法拟合因子分析模型。": "Could not fit the factor-analysis model.",
        "训练数": "Trains",
        "平滑 σ": "Smoothing σ",
        "时间原点": "Time origin",
        "平面": "Plane",
        "方差解释 (PC1–PC3)": "Variance explained (PC1–PC3)",
        "无方差信息。": "No variance information.",
        "无方差信息": "No variance information",
        "占比": "explained",
        "累计": "cumulative",
        "Top 载荷 (|PC1|+|PC2|)": "Top loadings (|PC1|+|PC2|)",
        "全时段": "full duration",
        "暂无法构建矩阵": "Cannot build matrix yet",
        "暂无法运行 PCA": "Cannot run PCA yet",
        "调整训练选择或参数后，这里会显示矩阵、方差和载荷摘要。":
            "Adjust the train selection or parameters and the matrix, variance, and loadings summary appears here.",
        "请选择至少一条 spike train。": "Select at least one spike train.",
        "所选 spike train 都没有足够（≥2）的 spike，无法分箱。":
            "None of the selected spike trains have enough (≥2) spikes to bin.",
        "有效时间 bin 太少（需要 ≥3 个）。请增大时间范围或减小 bin 宽度。":
            "Too few valid time bins (need ≥3). Increase the time range or reduce the bin width.",
        "需要至少两条 spike train 才能构建群体流形。":
            "At least two spike trains are needed to build the population manifold.",
        "至少需要两条有变化的 spike train（去掉常量 / 空 train）。":
            "At least two varying spike trains are needed (drop constant / empty trains).",
        "切换 2D / 3D 显示，开关轨迹连线 / 时间渐变，并选择着色方式：时间、自动模式或最终复核模式（bin 按主导检测状态着色）。PCA 不变，仅绘制方式改变。":
            "Switch 2-D / 3-D display, toggle the trajectory line / time-gradient, and choose Color by: Time, Auto pattern, or Final reviewed pattern (bins colored by dominant detector state). PCA is unchanged; only drawing changes.",
        "坐标轴可使用 PC1/PC2/PC3 或时间（群体 bin 中点）。2D 使用 X/Y；3D 使用 X/Y/Z。":
            "Axes can use PC1/PC2/PC3 or Time (population-bin midpoint). 2-D uses X/Y; 3-D uses X/Y/Z.",
        "群体 bin 宽度（R bin_sec，默认 50 ms / 0.05 s）。bin 越小时间分辨率越高，但每个 bin 的发放率噪声更大、bin 数更多。":
            "Population bin width (R bin_sec, default 50 ms / 0.05 s). Smaller bins give finer temporal resolution but noisier per-bin rates and more bins.",
        "高斯平滑 sigma（单位 bin，R smoothing_sigma_bins）；0 表示禁用。连线范围控制用于连接轨迹点的时间区间。0→0 表示连接整段记录。":
            "Gaussian smoothing sigma in bins (R smoothing_sigma_bins); 0 disables. Line range controls the time interval used for connecting trajectory points. 0→0 means connect the full recording.",
        "每个 bin 的信号变换（R transform）。sqrt(count + 3/8) 稳定计数方差；log1p(rate) 压缩高发放率；rate 为 Hz；count 为原始计数。":
            "Per-bin signal transform (R transform). sqrt(count + 3/8) stabilizes count variance; log1p(rate) compresses high rates; rate is Hz; count is raw.",
        "PCA 前的每神经元列缩放（R scaling）。z-score 和 robust 使各神经元可比；none 保留原始变换单位。PCA 在缩放后的矩阵上运行。":
            "Per-neuron column scaling before PCA (R scaling). z-score and robust make neurons comparable; none keeps the raw transform units. PCA runs on the scaled matrix.",
        "将流形参数重置为 R 默认值（50 ms，raw，√count，z-score，σ = 1）。不会改变训练选择。":
            "Reset the manifold parameters to the R defaults (50 ms, raw, √count, z-score, σ = 1). Does not change the train selection.",
        "任务 / 刺激事件的时间上下文（原始时间）。事件仅为标注层——它们绝不进入群体 bin 或 PCA。":
            "Task / stimulus event timing context (raw time). Events are an annotation layer only — they never enter the population bins or PCA.",

        // I18N-2A: Structure candidates header, buttons, empty states, summary tiles, sections, controls.
        "导入审核": "Import review",
        "导出 CSV": "Export CSV",
        "检测中": "Detecting",
        "重新检测": "Re-run detection",
        "运行检测": "Run detection",
        "参数已更改 · 请重新检测": "Parameters changed · re-run detection",
        "Detector/Parameters 的更改尚未应用到当前栅格显示，请重新运行检测以生效。":
            "Detector/Parameters edits aren't applied to the displayed raster yet — re-run detection.",
        "无数据集": "No Dataset",
        "无候选审核": "No Candidate Audit",
        "运行自适应经典锚点检测以生成结构候选。":
            "Run adaptive classic-anchor detection to generate structural candidates.",
        "运行时间": "Runtime",
        "最慢序列": "Slowest train",
        "候选": "Candidates",
        "锁定经典": "Locked classic",
        "强候选": "Strong",
        "高频爆发": "HF burst",
        "长爆发": "Long burst",
        "审查": "Review",
        "事件轨道": "Event track",
        "间隔轨道": "Gap track",
        "状态轨道": "State track",
        "流水线阶段": "Pipeline stages",
        "最慢的序列": "Slowest trains",
        "性能": "Performance",
        "未记录耗时行。": "No timing rows recorded.",
        "通道": "Channel",
        "聚焦下一个": "Focus next",
        "候选审核": "Candidate audit",
        "标签": "Label",
        "仅显示已选": "Selected only",
        "没有候选符合当前筛选条件。": "No candidates match the current filters.",
        "共享审查通道。筛选此列表并驱动检查器的上一个/下一个 + 批量接受。":
            "Shared review channel. Filters this list and drives Inspector previous/next + batch Accept.",
        "聚焦此通道中的第一个待处理结构，或前进到下一个。不改变审查状态。":
            "Focus the first open structure in this channel, or advance to the next one. Does not change review status.",
        "在活动通道内的可选精细标签筛选。通道改变时重置为全部模式。":
            "Optional fine label filter within the active channel. Resets to All patterns when the channel changes.",
        "默认：仅显示已选中的自动候选。关闭后可检视被拒绝、诊断和冲突候选。":
            "Default: show selected auto candidates only. Turn off to inspect rejected, diagnostic, and conflict candidates.",
        "清除": "Clear",
        "序列名包含": "Train name contains",
        "起始": "Start",
        "结束": "End",
        "片段审查器": "Fragment reviewer",
        "无活动时间窗": "no active window",
        "候选与冲突": "Candidates and conflicts",
        "左": "Left",
        "右": "Right",
        "左侧时间戳": "Left timestamp",
        "右侧时间戳": "Right timestamp",
        "ISI 间隔": "ISI interval",
        "绝对无效 ISI": "Absolutely invalid ISI",
        "通过": "Pass",
        "STPD 检测": "STPD detection",
        "尚无匹配结果": "No matching result",
        "可能 Burst": "Possible Burst",
        "配置": "Profile",
        "ISI 详情": "ISI details",
        "自动检测": "Automatic detection",
        "自动审核": "Automatic review",
        "未检测到模式": "No detected pattern",
        "无": "None",
        "是": "Yes",
        "已选": "Selected",
        "审核状态": "Audit status",
        "仅显示前 18 条局部 ISI；缩小时间窗以查看更多细节。":
            "Showing first 18 local ISIs; narrow the window for more detail.",
        "状态": "Status",
        "序列": "Train",
        "ISI 跨度": "ISI span",
        "Spike 跨度": "Spike span",
        "频带": "Band",
        "对比度": "Contrast",
        "审核": "Audit",
        "原因": "Why",
        "阶段": "Stage",
        "决策": "Decision",
        "接受": "Accept",
        "需要审查": "Needs review",
        "拒绝": "Reject",
        "清除审查": "Clear review",
        "已锁定": "Locked",
        "强": "Strong",
        "否": "No",
        "片段核对": "Fragment audit",
        "带入当前候选": "Use current candidate",
        "在 raster 查看": "View in raster",
        "在 ISI 时间剖面查看": "View in ISI time profile",
        "显示与指定时间窗重叠的全部候选，包括未选中、拒绝和被压制候选。":
            "Show all candidates overlapping the given time window, including unselected, rejected, and suppressed candidates.",
        "输入有效的 Start / End，或先点击候选表中的一行再点“带入当前候选”。":
            "Enter a valid Start / End, or click a row in the candidate table and then use \"Use current candidate\".",
        "当前片段内没有找到可核对的 ISI 或候选。可以扩大时间窗，或输入更精确的 train 名称。":
            "No reviewable ISIs or candidates were found in this fragment. Widen the time window, or enter a more precise train name.",

        // I18N-2A: Neural Manifold header summary (fixed-case branches).
        "选择至少两条 spike train 以构建群体流形。":
            "Select at least two spike trains to build the population manifold.",

        // I18N-2B: DetectorParametersView / ISITimelineView / DatasetISIHistogramView / DataQCView.
        "单位": "Unit",
        "尚未运行检测": "No Detection Run",
        "打开原始棘波时间戳 CSV，或加载内置示例。": "Open a raw spike timestamp CSV or load the bundled sample.",
        "棘波数": "Spikes",
        "最小有效 ISI": "Minimum valid ISI",
        "直方图分箱": "Histogram bin",
        "仅控制 ISI 分布汇总与自适应诊断。爆发种子频带由结构性 Burst I 锚点导出，而非来自默认直方图区间。": "Only controls ISI distribution summaries and adaptive diagnostics. Burst seed bands are derived from structural Burst I anchors, not from a default histogram interval.",
        "诊断直方图分箱宽度（毫秒）。": "Diagnostic histogram bin width in milliseconds.",
        "不应期": "Refractory",
        "事件模式参数": "Event pattern parameters",
        "经典爆发": "Classic burst",
        "对比度 I": "Contrast I",
        "暂停侧翼": "Pause flank",
        "最少棘波数": "Min spikes",
        "最多棘波数": "Max spikes",
        "对比度 I 接受 Burst I。仅当边界 ISI 达到爆发内 q90 的相应倍数时，暂停侧翼才播种暂停。": "Contrast I accepts Burst I. Pause flank only seeds pause when the boundary ISI is this many times intra-burst q90.",
        "最小 ISI 毫秒": "Min ISI ms",
        "0 = 自动。在结构性爆发侧翼与强直发放守卫计算完成后，手动值将覆盖按序列自适应的暂停下界。": "0 = auto. Manual values override the train-adaptive pause lower bound after structural burst flanks and tonic guards are computed.",
        "状态模式参数": "State pattern parameters",
        "爆发占比": "Burst frac",
        "低尾占比": "Low tail",
        "HFS": "HFS",
        "短间隔占比": "Short frac",
        "大间隔占比": "Large frac",
        "最大连续大间隔": "Max large",
        "手动阈值": "Manual thresholds",
        "可选的按家族 ISI / 棘波计数限制，针对每条序列依据自适应频带解析。自动 = 保持不变。硬门控仅收窄（爆发硬门控同时驱动 ISI 剖面爆发路径）；软锚点仅放宽，绝不强制候选项。数值单位为毫秒；每个已应用阈值都会记录在候选决策路径与 resolved_thresholds CSV 列中。": "Optional per-family ISI / spike-count limits resolved per train against the adaptive bands. Auto = unchanged. Hard gates narrow only (a burst hard gate also drives the ISI-profile burst route); soft anchors only widen and never force a candidate. Values are milliseconds; every applied threshold is recorded in the candidate decision path and the resolved_thresholds CSV column.",
        "模式": "Mode",
        "种子最大毫秒": "Seed max ms",
        "桥接最大毫秒": "Bridge max ms",
        // Task A: Soft-mode burst field labels (anchor, not a cap) + soft-mode help copy.
        "种子锚点毫秒": "Seed anchor ms",
        "桥接锚点毫秒": "Bridge anchor ms",
        "软：仅放宽——高于该值的 ISI 仍可能被选为爆发。改用硬模式可对高于该值的 ISI 封顶 / 排除。":
            "Soft widens only — ISIs above this value may still be selected as burst. Use Hard to cap / exclude ISIs above this value.",
        // Phase 1B: learned-from-annotations preview + Apply.
        "从手动标注学习（预览）": "Learned from manual annotations (preview)",
        "应用学习到的阈值": "Apply learned thresholds",
        "作为软锚点写入；不覆盖硬门控家族；之后需手动重新运行检测。":
            "Written as soft anchors; hard-gated families are not overwritten; re-run detection manually afterwards.",
        "暂无可用的学习阈值——请添加更多手动标注（达到各家族的最少样本数）。":
            "No usable learned thresholds yet — add more manual annotations (reaching each family's minimum sample count).",
        "爆发种子上界": "Burst seed upper",
        "爆发桥接上界": "Burst bridge upper",
        "强直下界": "Tonic lower",
        "强直上界": "Tonic upper",
        "高频强直下界": "HF-tonic lower",
        "高频强直上界": "HF-tonic upper",
        "暂停下界": "Pause lower",
        "样本不足：": "Insufficient samples: ",
        "HFS 暂不学习": "HFS not learned yet",
        "已应用（软锚点）：": "Applied (soft anchor): ",
        "重新运行检测以更新自动结果。": "Re-run detection to update auto results.",
        "跳过（已为硬门控）：": "Skipped (already hard-gated): ",
        // Phase 1C: current → learned explainability.
        "跳过：当前为硬门控": "Skipped: current family is Hard",
        "无变化": "No change",
        "无可用学习值：": "No usable learned value: ",
        // Phase 1F: clearer learned-threshold wording.
        "预览仅供查看，不影响检测，直到点击下方“应用学习到的阈值”。应用后请手动重新运行检测。":
            "Preview only — it does not affect detection until you click Apply learned thresholds below. After applying, re-run detection manually.",
        "学习值以软锚点写入：仅放宽 / 建议，不是上限，也不会排除高于或低于该值的 ISI。要按阈值收窄区间，请改用硬门控。":
            "Learned values are written as Soft anchors: they widen / suggest only — not a cap, and they do not exclude ISIs above or below. Use a Hard gate to constrain the band by the threshold.",
        "不覆盖你已设为硬门控的家族。": "Does not overwrite families you set to Hard.",
        // SEG-PREVIEW-1: boundary sensitivity preview (read-only diagnostic).
        "边界敏感性预览": "Boundary sensitivity preview",
        "仅为诊断预览：不修改阈值、检测结果或标签。展开 = 纳入相邻 ISI 后是否仍通过强直门限；收缩 = 去掉边缘 ISI 后核心是否仍为强直。":
            "Diagnostic preview only — it does not modify thresholds, detection results, or labels. Expansion = would including a neighboring ISI still pass the tonic gates; contraction = does the core stay tonic without the edge ISIs.",
        "强直门限": "Tonic gates",
        "强直": "Tonic",
        "展开": "Expansion",
        "核心稳定性": "Core stability",
        "仅预览，未应用。": "Preview only — not applied.",
        "当前": "Current",
        "左 +1": "Left +1",
        "右 +1": "Right +1",
        "两侧 +1": "Both +1",
        "左 −1": "Left −1",
        "右 −1": "Right −1",
        "两侧 −1": "Both −1",
        "过短": "Too short",
        "边界": "Boundary",
        "稳定": "Stable",
        "改变": "Changed",
        // Adaptive-v2 burst canonicalization control.
        "推荐功能": "Recommended",
        "Adaptive v2 爆发规范化（默认开启）": "Adaptive v2 burst canonicalization (default on)",
        "将结构上像爆发、但与该序列的爆发画像不兼容的候选降级为「可能爆发 / 复核」。不会删除候选，也不会覆盖手动语义标签。默认开启；如需与旧版逻辑比较，可暂时关闭。":
            "Demotes structurally burst-like candidates that are incompatible with this train's burst profile to possible burst / review. It does not delete candidates and does not override manual semantic labels. Default on; turn it off temporarily to compare with the legacy logic.",
        "更改后不会自动重新检测；如已有结果，请重新运行检测以查看效果。":
            "Changing this does not auto-run detection; if results already exist, re-run detection to see the effect.",
        // P8: manual hard-threshold scope picker (Detector / Parameters · Manual thresholds).
        "应用范围": "Threshold scope",
        "全部序列": "All trains",
        "当前序列": "Current train",
        "所选序列": "Selected trains",
        "作用序列": "Applies to",
        // P10: clearer scope copy (3 explicit points) + the candidate-inspector provenance label.
        "硬门控仅作用于所选范围；软锚点与自动模式始终对全部序列生效。范围若未匹配任何序列，硬门控当前不影响任何序列。":
            "Hard gates apply only to the selected scope; soft anchors and automatic modes always apply to all trains. If the scope targets no train, hard gates currently affect no train.",
        "按范围应用硬门控": "Hard gate applied by scope",
        "当前范围未匹配任何序列：请聚焦一条序列，或在光栅图中选择可见序列。":
            "The current scope matches no train: focus a train, or select visible trains in the raster.",
        // SIM-1D: Simulator / Preview page — tonic regularity ladder (preview-only; reads default detector settings).
        "标准强直": "Clean tonic",
        "棘波过少": "Too few spikes",
        "CV 过高": "CV too high",
        "CV2 过高": "CV2 too high",
        "LV 过高": "LV too high",
        "非纯强直": "Not clean tonic",
        "种子": "Seed",
        "平均 ISI": "Mean ISI",
        "抖动阶梯": "Jitter ladder",
        "抖动 SD": "Jitter SD",
        "棘波 / ISI": "Spikes / ISI",
        "检测标签": "Detector labels",
        "判定": "Verdict",
        "生成的强直序列（按抖动从小到大）": "Generated tonic trains (low → high jitter)",
        "选中行的光栅": "Selected row raster",
        "仅预览：不修改当前数据集、检测运行或检测器参数。CV / CV2 / LV 是评估指标，不是直接的生成旋钮——抖动 SD 才是。":
            "Preview only: does not modify the current dataset, detection run, or detector parameters. CV / CV2 / LV are evaluation metrics, not direct generative knobs — the jitter SD is.",
        "按当前检测器门限评估": "Evaluated against the current detector gates",
        "默认": "Default",
        "变化": "Changed",
        // SIM-2D: Simulator / Preview — Burst response mode (preview-only; reads the current detector parameters).
        "爆发响应": "Burst response",
        "爆发响应预览：在每个刺激时刻放置一个爆发包，按检测器的事件覆盖逐时刻判定。":
            "Burst-response preview: a burst packet is placed at each stimulus onset; each onset is judged by the detector's event coverage.",
        "按当前检测器参数评估": "Evaluated against the current detector parameters",
        "爆发门限": "Burst gates",
        "生成的爆发响应序列（按包棘波数从小到大）": "Generated burst-response trains (by packet spike count, ascending)",
        "包棘波": "Packet spikes",
        "命中 / 可能 / 拆分 / 漏检": "Hit / Poss / Split / Miss",   // SIM-2G: compact dense-table column label
        "每个预期爆发时刻的计数。": "Counts are per intended burst onset.",   // SIM-2G: per-onset count caption
        "包大小阶梯": "Packet ladder",
        "常规": "Regular",
        "密集": "Dense",
        "稀疏": "Sparse",
        // SIM-2D: burst verdict display labels (the model keeps machine tokens raw; only these display strings localize).
        "识别为爆发": "Detected as burst",
        "识别为可能爆发": "Detected as possible burst",
        "拆分或部分爆发包": "Split / partial packet",
        "漏检爆发包": "Missed packet",
        // SIM-2E polish: short verdict-chip labels for the dense burst table (the full labels stay on the selected-row
        // card; "爆发" → "Burst" already exists above), plus the previously-missing tonic jitter-preset names.
        "可能": "Possible",
        "拆分": "Split",
        "漏检": "Missed",
        "标准": "Standard",
        "精细": "Fine",
        "宽": "Wide",
        // P7B: Adaptive-v2 canonicalization explanation copy (Inspector / pinned ISI / hover card). Display only —
        // the raw decision-path tokens are never localized. Short titles:
        "保留为爆发（已修整边界）": "Kept as burst (boundary trimmed)",
        "保留为爆发（强核心）": "Kept as burst (strong core)",   // P11A
        "降级为可能爆发": "Downgraded to Possible burst",
        "保留复核（暂停侧翼）": "Kept for review (pause-flanked)",
        "更像强直而非爆发": "More tonic-like than burst",
        "桥接 / 尾部无核心": "Bridge/tail without core",
        "证据不足": "Insufficient evidence",
        // Summaries:
        "Adaptive v2 保留该爆发：已去除一个较慢的边界 ISI，紧密核心仍符合该序列的爆发画像。":
            "Adaptive v2 kept this as a burst after trimming a slow boundary ISI. The tight core remains compatible with this train's burst profile.",
        "去除左边界。": "Trimmed left edge.",
        "去除右边界。": "Trimmed right edge.",
        "去除两侧边界。": "Trimmed both edges.",
        "Adaptive v2 将该爆发样候选降级为可能爆发 / 复核，因为它与该序列的爆发画像不匹配。":
            "Adaptive v2 downgraded this burst-like candidate to Possible burst / review because it does not match this train's burst profile.",
        "Adaptive v2 将该爆发样候选降级为可能爆发 / 复核。":
            "Adaptive v2 downgraded this burst-like candidate to Possible burst / review.",
        "Adaptive v2 保留该爆发：其紧密核心足够强，即使尾部 ISI 略高于爆发上限，整体仍符合标准爆发。":
            "Adaptive v2 kept this as a burst: its tight core is strong enough that, even though the tail ISIs run slightly above the burst ceiling, it still reads as a canonical burst.",
        "该密集片段位于明显暂停样间隔之间，因此保留为复核，而不是直接接受为标准爆发。":
            "The packet sits between strong pause-like gaps, so it is kept for review rather than accepted as a canonical burst.",
        "该区间更像规则强直放电，而不是标准爆发。":
            "This interval is more compatible with tonic-like regular firing than with a canonical burst.",
        "该区间只符合桥接 / 尾部范围，缺少足够的爆发核心。":
            "The interval only fits the bridge/tail band and lacks a sufficient burst core.",
        "Adaptive-v2 证据不足；未做出强标准爆发判定。":
            "Adaptive-v2 evidence was insufficient; no strong canonical decision was made.",
        "爆发准入上限": "Burst eligibility ceiling",
        // SEG-PREVIEW-2: surface the preview from a pinned ISI.
        "评估覆盖该 ISI 的强直候选": "Evaluating the tonic candidate covering this ISI",
        "聚焦候选": "Focus candidate",
        "敏感性预览仅适用于强直候选。": "Sensitivity preview is available for tonic candidates.",
        // SEG-PREVIEW-4: one-line summary verdict.
        "稳定强直核心": "Stable tonic core",
        "边界敏感": "Boundary-sensitive",
        "依赖边缘 ISI": "Edge-dependent",
        "核心可通过": "Core passes",
        "核心不足": "Insufficient core",
        "不符合强直": "Not tonic-compatible",
        "当前通过；扩展仍通过，收缩通过或过短——核心稳健。":
            "Current passes; expansions still pass and contractions pass or are too short — a robust core.",
        "当前通过，但纳入相邻 ISI 会改变判定——边界处敏感。":
            "Current passes, but including a neighboring ISI changes the verdict — sensitive at the boundary.",
        "当前通过，但去掉边缘 ISI 会改变判定——判定依赖边缘 ISI。":
            "Current passes, but dropping an edge ISI changes the verdict — the label depends on edge ISIs.",
        "当前不通过，但收缩到核心可满足强直门限。":
            "Current fails, but a contracted core meets the tonic gates.",
        "收缩多为过短，无法评估核心稳定性。":
            "Contractions are mostly too short, so core stability can't be assessed.",
        "当前不通过，且无收缩能满足门限。":
            "Current fails and no contraction meets the gates.",
        // SEG-PREVIEW-5: row-level explanation captions.
        "当前检测到的候选边界。": "Current detected candidate boundary.",
        "核心过短，无法单独评估强直规则性。": "Core is too short to evaluate tonic regularity on its own.",
        "失败原因：%@。": "Failure reason: %@.",
        "纳入或移除该边缘后仍通过强直门限。": "Still passes tonic gates after including or removing this edge.",
        "该边界变化会改变强直兼容性。": "This boundary change alters tonic compatibility.",
        "仅用于解释边界敏感性。": "Shown only to explain boundary sensitivity.",
        "最大 ISI 毫秒": "Max ISI ms",
        "最小时长毫秒": "Min dur ms",
        "软": "Soft",
        "硬": "Hard",
        // Phase 5: manual-threshold parameter-effect feedback
        "自动：仅自适应，不改变检测。": "Auto: adaptive only — detection unchanged.",
        "软：放宽 / 锚定频带，绝不收窄或强制候选。":
            "Soft: widen / anchor the band; never narrows or forces candidates.",
        "硬：种子 / 桥接上限作为准入上限——可恢复中等 ISI，设得更低则收窄。":
            "Hard: seed / bridge max act as an admission ceiling — can recover moderate ISI; set lower to constrain.",
        "硬：收窄检测。若自适应上限已低于该硬上限，提高它不会扩大检测。":
            "Hard: constrains detection. If the adaptive upper is already below this hard max, raising it will not expand detection.",
        "手动爆发上限已启用：将你的种子 / 桥接上限与下方各序列的自适应种子 / 桥接上界对照——硬上限高于自适应上界可恢复中等 ISI，低于则收窄。":
            "A manual burst max is active: compare your seed / bridge limit with the per-train adaptive seed / bridge uppers below — a hard max above the adaptive upper can recover moderate ISI, below it constrains.",
        "候选项": "Candidates",
        "复审": "Review",
        "间隙轨道": "Gap track",
        "自适应爆发频带": "Adaptive burst bands",
        "有效 ISI": "Valid ISI",
        "种子下界": "Seed lower",
        "种子上界": "Seed upper",
        "桥接": "Bridge",
        "来源": "Source",
        "候选项审计": "Candidate audit",
        "起始 ISI": "Start ISI",
        "结束 ISI": "End ISI",
        "边缘对比度": "Edge contrast",
        "评分": "Score",
        "审计": "Audit",
        "未加载 ISI 序列": "ISI Trace Not Loaded",
        "请先加载脉冲序列数据集。": "Load a spike train dataset first.",
        "未选择 ISI 序列": "No ISI trains selected",
        "使用顶部的 ISI 序列选择器选择一个或多个脉冲序列。": "Use the ISI train selector in the header to choose one or more spike trains.",
        "布局": "Layout",
        "Y 轴刻度": "Y scale",
        "锁定 Y 轴": "Lock Y",
        "全部可见的四个 ISI 窗格使用同一共享 Y 轴范围。": "Use one shared Y range across the four visible ISI panes.",
        "重置 ISI 窗口": "Reset ISI window",
        "清除已锁定的 ISI 参考。": "Clear the locked ISI reference.",
        "复制到手动阈值": "Copy to manual threshold",
        "将参考 ISI 复制到某个手动阈值字段。若该族为自动，则会成为软锚点，在下次检测器运行时生效。": "Copy the reference ISI into one of the manual threshold fields. If that family is automatic it becomes a soft anchor so it applies on the next detector run.",
        "相似 ±": "Similar ±",
        "高亮显示数值在参考值此比例范围内的 ISI。": "Highlight ISIs whose value is within this fraction of the reference.",
        "点击 ISI 点或线段将其锁定为参考。": "Click an ISI dot or segment to lock it as a reference.",
        "阈值线": "Threshold lines",
        "叠加显示活动的手动阈值线（实线 = 硬门限，虚线 = 软锚点）。": "Overlay active manual threshold lines (solid = hard gate, dashed = soft anchor).",
        "可见窗口": "Visible window",
        "控制可见 ISI 时间视口的宽度。横向滚动以浏览时间轴。": "Controls the width of the visible ISI time viewport. Scroll horizontally to move through time.",
        "显示下一组（最多四个）ISI 窗格。": "Show the next group of up to four ISI panes.",
        "点击聚焦此序列。按住 Command 点击可聚焦多个序列。": "Click to focus this train. Command-click to focus multiple trains.",
        "无可见 ISI 序列": "No visible ISI trace",
        "状态层": "States",
        "低于最小有效 ISI": "Below minimum ISI",
        "绝对不应期": "Absolute refractory",
        "疑似不应期": "Refractory suspect",
        "正常": "OK",
        "原始": "Raw",
        "均衡": "Balanced",
        "叠加": "Overlay",
        "计数": "Count",
        "比例": "Fraction",
        "仅供诊断 · 结构优先检测": "Diagnostic only · structure-first detection",
        "X 上限": "X max",
        "X 窗口": "X window",
        "全量 X": "Full X",
        "0 = 自动": "0 = auto",
        "0 = 全部": "0 = all",
        "输入窗口宽度后，可在图中左右拖动 X 轴。": "Enter a window width, then drag in the chart to pan the X axis.",
        "输入窗口宽度后，可在图中拖动或用触控板左右滑动 X 轴。": "Enter a window width, then drag or swipe horizontally in the chart to pan the X axis.",
        "对数 Y": "log Y",
        "显示最小有效 ISI 与疑似不应期阈值。": "Show minimum-valid-ISI and refractory-suspect thresholds.",
        "显示最小有效 ISI 与绝对不应期阈值。": "Show minimum-valid-ISI and absolute-refractory thresholds.",
        "结构频带": "Structure bands",
        "显示由结构检测器推断的区间。这些频带不会改变直方图计数。": "Show intervals inferred from the structural detector. These bands do not change histogram counts.",
        "打开原始尖峰时间戳 CSV，或加载内置示例。": "Open a raw spike timestamp CSV or load the bundled sample.",
        "可见": "Visible",
        "低于最小有效 ISI 而排除": "Excluded below minimum ISI",
        "结构衍生 ISI 频带": "Structure-derived ISI bands",
        "运行结构检测以显示结构衍生 ISI 频带。检测前仍可查看直方图计数。": "Run structural detection to show structure-derived ISI bands. Histogram counts remain available before detection.",
        "当前检测运行没有可用的结构衍生数据集频带。": "No structure-derived dataset bands are available for the current detection run.",
        "模式分类": "Pattern",
        "下限": "Lower",
        "上限": "Upper",
        "锚点": "Anchors",
        "逐序列 ISI 审计": "Per-train ISI audit",
        "有效": "Valid",
        "最小值": "Min",
        "中位数": "Median",
        "最大值": "Max",
        "数据集 ISI 分布": "Dataset ISI distribution",
        "原始汇集计数": "Raw pooled count",
        "序列均衡占比": "Train-balanced fraction",
        "原始汇集占比": "Raw pooled fraction",
        "ISI 分箱": "ISI bin",
        "范围": "Range",
        "原始计数": "Raw count",
        "原始占比": "Raw fraction",
        "序列均衡": "Train-balanced",
        "表头行": "Header row",
        "导入 CSV 中的原始时间戳单位。": "Raw timestamp unit in the imported CSV.",
        "打开 CSV": "Open CSV",
        "加载示例": "Load Sample",
        "最小有效 ISI 单位": "Minimum valid ISI unit",
        "疑似不应期阈值单位": "Refractory suspect threshold unit",
        "显示单位": "Display unit",
        "控制 QC 结果和消息的显示单位。它不会改变原始导入的时间戳。": "Controls the display unit for QC results and messages. It does not change the raw imported timestamps.",
        "打开原始尖峰时间戳 CSV 或加载捆绑的示例。": "Open a raw spike timestamp CSV or load the bundled sample.",
        "序列数": "Trains",
        "尖峰": "Spikes",
        "错误": "Errors",
        "警告": "Warnings",
        "低于最小有效 ISI 的区间": "Below-minimum ISI",
        "已合并重复项": "Duplicates merged",
        "重复项": "Duplicates",
        "质量表": "Quality table",
        "尖峰数": "Spikes",
        "最小 ISI": "Min ISI",
        "中位 ISI": "Median ISI",
        "消息": "Message",
        "低于最小有效 ISI 的区间详情": "Below-minimum ISI details",
        "当前没有低于最小有效阈值的 ISI。": "No ISI falls below the current minimum-valid threshold.",
        "ISI 索引": "ISI index",
        "阈值": "Threshold",
        "重复时间戳详情": "Duplicate timestamp details",
        "保留的序列中无重复时间戳。": "No duplicate timestamps in the retained trains.",
        "时间戳": "Timestamp",
        "排序行": "Sorted rows",
        "输入行": "Input rows",
        "策略": "Policy",
        "重复时间戳": "Duplicate timestamps",
        "应用": "Apply",
        "将所选的重复时间戳策略应用于当前数据集。": "Apply the selected duplicate timestamp policy to the current dataset.",
        "参考": "Reference",
        "正在生成学习预览…": "Generating learning preview…",
        "生成学习预览": "Generate learning preview",
        "重新生成预览": "Regenerate preview",
        "应用所选学习阈值": "Apply selected learned thresholds",
        "仍然应用所选家族": "Apply selected families anyway",
        "所选家族存在科学或跨序列一致性警告":
            "Selected families have scientific or cross-train consistency warnings",
        "这些警告不会删除证据，但表示建议可能不稳定或与预期模式顺序不一致。请确认后再应用。":
            "These warnings do not remove evidence, but the suggestions may be unstable or inconsistent with the expected pattern order. Confirm before applying.",
        "勾选本次要应用的模式家族；未勾选家族保持当前参数和溯源不变。":
            "Select the pattern families to apply now; unselected families keep their current parameters and provenance.",
        "选择是否将该家族的学习值应用为软锚点。":
            "Choose whether to apply this family's learned values as soft anchors.",
        "选择是否应用 HFS 的保守最低支持门槛；应用前需要确认。":
            "Choose whether to apply the conservative HFS minimum-support gates; confirmation is required.",
        "每个标记片段先投一票，再在每条序列内取中位数，最后让各序列等权参与；绝不把所有 ISI 混池。":
            "Each labeled segment votes once, then medians are taken within each train and trains are weighted equally; raw ISIs are never pooled.",
        "序列覆盖只表示当前数据集内多条 spike train 的一致性；不等于独立神经元、记录会话或动物重复，也不能直接证明外部泛化。":
            "Train coverage describes consistency across spike trains in this dataset only; it is not independent neuron, session, or animal replication and does not establish external generalization.",
        "HFS 的 ISI 分布仍仅作描述；只有证据充分时才建议最少 spike 数和最短持续时间，并必须由用户确认。":
            "The HFS ISI distribution remains descriptive; minimum spike count and duration are suggested only with sufficient evidence and require user confirmation.",
        "Burst、Tonic、HF tonic 与 Pause 以软锚点写入。HFS 的最少 spike 数和最短持续时间是保守下限，会排除支持不足的候选，因此应用前必须确认。":
            "Burst, Tonic, HF-tonic, and Pause are applied as soft anchors. HFS minimum spike count and duration are conservative lower gates that reject candidates with insufficient support, so confirmation is required.",
        "请先生成身份绑定的学习预览；生成本身不会改变检测器。":
            "Generate an identity-bound learning preview first; generation does not change the detector.",
        "当前标记可供审计，但尚不足以生成兼容的学习阈值。":
            "The current labels remain auditable but are insufficient for compatible learned thresholds.",
        "撤销上一次学习阈值应用": "Undo last learned-threshold application",
        "恢复应用前的阈值；不会自动运行检测器。":
            "Restore the thresholds from before application; the detector will not run automatically.",
        "参数已在应用后改变，为避免覆盖修改，回滚已锁定。":
            "Parameters changed after application; rollback is locked to protect those edits.",
        "标记 %d / 可用 %d 段 · 序列 %d / %d":
            "Labeled %d / usable %d segments · trains %d / %d",
        "中心 %.3f ms": "Center %.3f ms",
        "序列中位数范围 %.3f–%.3f ms": "Train-median range %.3f–%.3f ms",
        "留一序列命中 %d/%d": "Leave-one-train-out hits %d/%d",
        "爆发家族": "Burst family",
        "单序列探索": "Single-train exploratory",
        "双序列暂定": "Two-train provisional",
        "多序列支持": "Multi-train supported",
        "多序列覆盖": "Multi-train coverage",
        "可用片段不足，未生成该家族阈值。": "Insufficient usable segments; no threshold was generated for this family.",
        "仅覆盖一条序列，结果属于探索性建议。": "Only one train is covered; this is an exploratory suggestion.",
        "留一序列检查只有部分一致，请结合预览复核。": "Leave-one-train-out checks are only partly consistent; review the preview.",
        "留一序列检查不一致；建议补充标记或检查异质性。": "Leave-one-train-out checks are inconsistent; add labels or inspect heterogeneity.",
        "未观察到预期的 Burst–Tonic 稳健中心顺序；证据仍被保留。": "The expected Burst–Tonic robust-center order was not observed; evidence is retained.",
        "未观察到预期的 Tonic–Pause 稳健中心顺序；证据仍被保留。": "The expected Tonic–Pause robust-center order was not observed; evidence is retained.",
        "HFS 稳健中心未位于 Tonic 的较小 ISI 一侧。": "The HFS robust center is not on the smaller-ISI side of Tonic.",
        "HF tonic 稳健中心未与 Burst 的较小 ISI 区间分离。": "The HF-tonic robust center is not separated above the smaller Burst-ISI region.",
        "HFS 建议值是候选筛选下限，而非普通软锚点；仅在确认标记片段可代表 HFS 状态后应用。":
            "HFS suggestions are candidate-screening lower gates, not ordinary soft anchors; apply only after confirming that the labeled segments represent HFS states.",
        "HFS 最少 spike 数": "HFS minimum spike count",
        "HFS 最短持续时间": "HFS minimum duration",
        "%d 个 spike": "%d spikes",
        "已应用 HFS 保守最低支持门槛；重新运行检测以更新自动结果。":
            "Applied the conservative HFS minimum-support gates; re-run detection to update automatic results.",
        "已写入所选人工学习阈值；请重新运行检测。":
            "Applied the selected manually learned thresholds; re-run detection.",
        "没有可应用的人工学习阈值。":
            "No manually learned thresholds are available to apply.",
        "请先确认科学导入并进入规范人工 ISI 工作区。": "Confirm the scientific import and enter the canonical manual-ISI workspace first.",
        "当前人工标记草稿与规范数据集身份不一致。": "The manual-label draft does not match the canonical dataset identity.",
        "绝对无效 ISI 上界必须是有限的非负数。": "The absolutely-invalid ISI boundary must be finite and nonnegative.",
        "绝对无效 ISI 上界不能精确表示为整数微秒；请调整输入精度。": "The absolutely-invalid ISI boundary is not exactly representable in integer microseconds; adjust its precision.",
        "按 train 留出验证": "Train-held-out validation",
        "基线检测 vs 仅由校准 train 学到的阈值": "Baseline detection vs thresholds learned only from calibration trains",
        "正在运行两次留出检测…": "Running two held-out detections…",
        "运行留出验证": "Run held-out validation",
        "重新运行留出验证": "Re-run held-out validation",
        "明确勾选留出 train；其余 train 用于一次性校准。留出标签只用于评价，不进入学习。": "Explicitly select held-out trains; all remaining trains are used for one-time calibration. Held-out labels are used only for evaluation and never enter learning.",
        "留出：": "Held out:",
        "校准 %d 条 · 留出 %d 条": "Calibration %d · held out %d",
        "至少需要两条规范 spike train 才能进行 train 级留出验证。": "At least two canonical spike trains are required for train-level held-out validation.",
        "正在校准 train 上学习，并在留出 train 上分别运行未学习基线和学习后检测；人工留出标签不会反馈到检测器。": "Learning on calibration trains and running both the unlearned baseline and learned detector on held-out trains; held-out manual labels are never fed back into the detector.",
        "检测参数已改变，当前留出报告已过期；请重新运行。": "Detector parameters changed; the current held-out report is stale. Re-run it.",
        "报告：校准 %d 条 · 留出 %d 条 · 已评价家族 %d 个": "Report: calibration %d · held out %d · assessed families %d",
        "留出 train 没有可评价的已审核模式标签；空白行没有被当作阴性。": "The held-out trains have no reviewed pattern labels that can be assessed; blank rows were not treated as negatives.",
        "不自动给出单一通过结论：F1、分段 IoU、边界误差和错误拆分/合并需要共同审阅。": "No single automatic pass verdict is issued: review F1, segment IoU, boundary error, and false splits/merges together.",
        "已审核 ISI %d": "Reviewed ISIs %d",
        "拆分 Δ %+d · 合并 Δ %+d": "splits Δ %+d · merges Δ %+d",
        "请至少明确留出一条 spike train，并保留至少一条校准 train。": "Explicitly hold out at least one spike train and retain at least one calibration train.",
        "验证期间数据、人工标记、train 分工或检测参数已改变；请重新运行。": "The data, manual labels, train roles, or detector parameters changed during validation; re-run it.",
        "正在校准 train 上学习，并在留出 train 上运行基线与学习后检测…": "Learning on calibration trains and running baseline and learned detection on held-out trains…",
        "train 留出验证已取消。": "Train-held-out validation was cancelled.",
        "train 留出验证完成；结果仅供比较，未改变检测器。": "Train-held-out validation completed; the comparison did not change the detector.",
        "train 留出验证失败。": "Train-held-out validation failed.",
    ]

    // MARK: - Russian dictionary
    // Russian follows the same Chinese-source contract as the English table. The owner-specified
    // core terminology is kept verbatim: Burst = «пачек», Pause = «пауза», Tonic = «тоник».
    public static let russianExactDictionary: [String: String] = [
        "语言 / Language": "Язык",
        "中文": "Китайский",
        "确定的单神经元电活动": "Подтверждённая активность одиночного нейрона",
        "确定的单神经元电活动（Single-unit）": "Подтверждённая активность одиночного нейрона",
        "多神经元电活动": "Мультиюнитная активность",
        "多神经元电活动（Multi-unit，实验性）": "Мультиюнитная активность (экспериментальная)",
        "未知或不确定": "Неизвестно или не определено",
        "主图": "Основные виды",
        "状态与流形": "Состояния и многообразие",
        "诊断": "Диагностика",
        "验证": "Проверка",
        "参数": "Параметры",
        "输出": "Вывод",
        "更多": "Ещё",
        "功能": "Функции",
        "左侧栏": "Левая панель",
        "右侧栏": "Правая панель",
        "左侧时间戳": "Левая метка времени",
        "右侧时间戳": "Правая метка времени",
        "ISI 间隔": "Интервал ISI",
        "绝对无效 ISI": "Абсолютно недопустимый ISI",
        "通过": "Пройдено",
        "STPD 检测": "Детекция STPD",
        "尚无匹配结果": "Совпадающих результатов нет",
        "状态": "Состояние",
        "事件": "Событие",
        "可能 Burst": "Возможная пачка",
        "拒绝": "Отклонено",
        "配置": "Профиль",
        "ISI 详情": "Сведения об ISI",
        "自动检测": "Автоматическая детекция",
        "自动审核": "Автоматическая проверка",
        "未检测到模式": "Паттерн не обнаружен",
        "无": "Нет",
        "是": "Да",
        "导入数据": "Импорт данных",
        "打开结果包": "Открыть пакет результатов",
        "打开人工审核结果包": "Открыть пакет ручной проверки",
        "导出事件 CSV": "Экспорт событий CSV",
        "导出逐 ISI 审核草稿": "Экспорт черновика проверки ISI",
        "导入人工标注": "Импорт ручных меток",
        "导入审核结果": "Импорт результатов проверки",
        "数据": "Данные",
        "棘波图": "Растр",
        "模式检测": "Детекция паттернов",
        "数据导入 / QC": "Импорт данных / QC",
        "数据导入与 QC": "Импорт данных и QC",
        "结构检测": "Структурная детекция",
        "spike train 光栅图": "Растр spike train",
        "状态与流形（后续分析）": "Состояния и многообразие (последующий анализ)",
        "时间戳图": "График временных меток",
        "对齐时间戳图": "Выровненный график временных меток",
        "原始时间戳图": "Исходный график временных меток",
        "目标核团深度图": "Глубина целевого ядра",
        "ISI 时间剖面": "Временной профиль ISI",
        "ISI 状态空间": "Пространство состояний ISI",
        "神经流形": "Нейронное многообразие",
        "区间直方图": "Гистограмма интервалов",
        "数据集 ISI 直方图": "Гистограмма ISI набора данных",
        "Spike train ISI 热力图": "Тепловая карта ISI spike train",
        "Spike train 模式热力图": "Тепловая карта паттернов spike train",
        "ISI 热力图": "Тепловая карта ISI",
        "模式热力图": "Тепловая карта паттернов",
        "合并": "Совместно",
        "自动": "Авто",
        "手工": "Вручную",
        "时间": "Время",
        "对齐时间": "Выровненное время",
        "原始时间戳": "Исходное время",
        "数据集": "Набор данных",
        "数据源": "Источник данных",
        "状态 · 事件 · 其它": "Состояние · событие · другое",
        "模式图例": "Легенда паттернов",
        "图例": "Легенда",
        "棘波序列": "Спайковая последовательность",
        "显示": "Показ",
        "来源": "Источник",
        "热力图清晰度": "Разрешение тепловой карты",
        "对齐": "Выровнено",
        "原始": "Исходные",
        "全选": "Выбрать всё",
        "条": "шт.",
        "全部": "Все",
        "选择": "Выбрать",
        "清空": "Очистить",
        "重置": "Сбросить",
        "最大": "Макс.",
        "可见窗口": "Видимый интервал",
        "可见": "Показано",
        "序列": "Последовательности",
        "spike 光栅图": "растр спайков",
        "对齐 spike 光栅图": "Выровненный растр спайков",
        "原始 spike 光栅图": "Растр исходных спайков",
        "Spike 高度": "Высота спайка",
        "ISI 分辨率约为": "Оценочное разрешение ISI",
        "尚未加载光栅图": "Растр не загружен",
        "目前没有可用的 spike train。": "Нет доступных spike train.",
        "没有可见的 spike train": "Нет видимых spike train",
        "请先在时间戳图中选择至少一条 spike train。": "Сначала выберите хотя бы один spike train на графике временных меток.",
        "在时间戳图上手工标记": "Ручная разметка на растре",
        "显示 ISI 信息栏": "Показывать панель сведений об ISI",
        "操作": "Действие",
        "标记": "Разметить",
        "擦除": "Стереть",
        "清除轨道": "Очистить дорожку",
        "撤销上一步": "Отменить последнее действие",
        "手工标记": "Ручные метки",
        "纯手工 ISI 标记（无需模式检测）": "Ручная разметка ISI (без детекции паттернов)",
        "本地草稿 · 未封存": "Локальный черновик · не запечатан",
        "身份绑定草稿": "Черновик, привязанный к идентичности данных",
        "审核已确认": "Проверка подтверждена",
        "导出已标注 ISI": "Экспорт размеченных ISI",
        "导入手工 ISI 草稿": "Импорт черновика ручной разметки ISI",
        "导出 CSV / XLSX / NEX": "Экспорт CSV / XLSX / NEX",
        "确认完整审核": "Подтвердить полную проверку",
        "导出 .stpdresult": "Экспорт .stpdresult",
        "尚未加载 spike train": "Спайковая последовательность не загружена",
        "请先导入或打开数据集，再进行 ISI 人工标注。":
            "Перед ручной разметкой ISI импортируйте или откройте набор данных.",
        "规范人工审核不可用": "Каноническая ручная проверка недоступна",
        "已持久化的导入尚不具备规范人工模式审核资格。":
            "Сохранённый импорт пока не соответствует условиям канонической ручной проверки паттернов.",
        "时间图和表格都只使用原始 spike 时间戳，不运行模式检测。可在时间图拖动选择连续 ISI，也可在表格精确多选。状态与事件相互独立，因此 HFS 可与其内嵌的 HFB 共存；可选择导出 CSV、XLSX 或 NeuroExplorer NEX，文件会明确标记为本地人工草稿。":
            "Шкала времени и таблица используют только исходные временные метки спайков; детекция паттернов не запускается. Непрерывные ISI можно выбрать перетаскиванием на шкале времени, а точные строки — в таблице. Состояния и события независимы, поэтому HFS может сосуществовать со встроенным HFB. Доступен экспорт в CSV, XLSX или NeuroExplorer NEX; файл явно помечается как локальный черновик ручной разметки.",
        "时间图和表格都直接来自已持久化的规范整数微秒数据集，不读取任何自动检测候选。可在时间图拖动选择连续 ISI，也可在表格精确多选；任何阶段均可选择导出 CSV、XLSX 或 NeuroExplorer NEX，未标记项会保留为空。只有确认完整审核并导出 .stpdresult 时，才要求每个 ISI 都归入状态、事件或其他。状态与事件保持独立，因此 HFS 可与内嵌 HFB 共存。":
            "Шкала времени и таблица строятся непосредственно из сохранённого канонического набора данных в целых микросекундах и не используют кандидаты автоматического детектора. Непрерывные ISI можно выбрать перетаскиванием на шкале времени, а точные строки — в таблице. На любом этапе доступен экспорт в CSV, XLSX или NeuroExplorer NEX; неразмеченные позиции остаются пустыми. Назначение каждого ISI к состоянию, событию или категории «Другое» требуется только при подтверждении полной проверки и экспорте .stpdresult. Состояния и события независимы, поэтому HFS может сосуществовать со встроенным HFB.",
        "Spike train": "Спайковая последовательность",
        "轨道": "Дорожка",
        "清除选择": "Снять выделение",
        "使用 Shift-单击或 Command-单击进行批量选择。":
            "Для множественного выбора используйте Shift-щелчок или Command-щелчок.",
        "左时间戳（s）": "Левая метка времени (s)",
        "左 MM": "Левый MM",
        "左 MM = 左邻 ISI ÷ 当前 ISI": "Левый MM = предыдущий ISI ÷ текущий ISI",
        "当前 ISI（ms）": "Текущий ISI (ms)",
        "右 MM": "Правый MM",
        "右 MM = 右邻 ISI ÷ 当前 ISI": "Правый MM = следующий ISI ÷ текущий ISI",
        "右时间戳（s）": "Правая метка времени (s)",
        "备注": "Примечание",
        "审核 / ISI #": "Проверка / ISI #",
        "已审核": "Проверено",
        "未审核": "Не проверено",
        "时间图手工选择": "Ручной выбор на шкале времени",
        "拖动选择连续 ISI；点击已标记色块后按 Delete 可清除该区块。":
            "Перетащите указатель, чтобы выбрать последовательные ISI; щёлкните размеченный блок и нажмите Delete, чтобы удалить его.",
        "鼠标悬停在相邻 spike 之间时显示该 ISI 的时间戳、间隔与模式信息":
            "При наведении между соседними спайками показывать временные метки, интервал и паттерн этого ISI",
        "缩放": "Масштаб",
        "适合窗口": "По размеру окна",
        "Spike": "Спайки",
        "手工 ISI 时间图": "Шкала времени для ручной разметки ISI",
        "拖动以选择连续 ISI；点击已标记色块后按 Delete 清除该区块":
            "Перетащите указатель для выбора последовательных ISI; щёлкните размеченный блок и нажмите Delete, чтобы удалить его",
        "未标记": "Без метки",
        "模式": "Режим",
        "Spike 数": "Число спайков",
        "下限": "Нижняя граница",
        "上限": "Верхняя граница",
        "ISI 阈值初标": "Предварительная разметка по порогам ISI",
        "人工规则 · 即时预览 · 不运行检测器": "Ручное правило · мгновенный предпросмотр · детектор не запускается",
        "ISI 闭区间": "Замкнутый диапазон ISI",
        "Spike 数闭区间": "Замкнутый диапазон числа спайков",
        "连续 n 个 ISI 对应 n+1 个 spike；超出 spike 上限的整段会被跳过，不会被切成人工 Burst 小包。":
            "n последовательных ISI соответствуют n+1 spike; весь сегмент выше предела пропускается, а не разрезается на искусственные пачки.",
        "指标": "Метрика",
        "指标闭区间": "Замкнутый диапазон метрики",
        "不限": "Без предела",
        "基于 CV/CV2/LV 的 Tonic 初标至少需要 5 个 ISI（6 个 spike）；更短片段仍由用户按结构证据审核。":
            "Для предварительной разметки тоника по CV/CV2/LV нужно не менее 5 ISI (6 spike); более короткие сегменты остаются для явной структурной проверки.",
        "MM = 候选段内最大 ISI / 最小 ISI；仅用于 3–5 个 spike 的短 Tonic 初标。":
            "MM = максимальный ISI / минимальный ISI в кандидате; метрика используется только для предварительной разметки короткого тоника из 3–5 spike.",
        "CV/CV2/LV 仅用于至少 5 个 ISI（6 个 spike）的 Tonic 初标；3–5 个 spike 请改用 MM。":
            "CV/CV2/LV используются только для предварительной разметки тоника из не менее 5 ISI (6 spike); для 3–5 spike используйте MM.",
        "选中预览": "Выбрать предпросмотр",
        "应用初标": "Применить предварительные метки",
        "撤销上一批": "Отменить последнюю группу",
        "初标未写入；请查看状态信息，确认当前 spike train、最小 spike 数与已有标记。":
            "Предварительные метки не записаны. Проверьте сообщение состояния, текущий spike train, минимальное число spike и существующие метки.",
        "正在运行模式检测并构建审计结果…": "Выполняется детекция паттернов и формируются результаты аудита…",
        "正在导入、校验并绑定手工标记…": "Импорт, проверка и привязка ручных меток…",
        "正在读取并验证规范手工 ISI 草稿…": "Чтение и проверка канонического черновика ручной разметки ISI…",
        "正在构建、校验并导出结果包…": "Формирование, проверка и экспорт пакета результатов…",
        "正在读取并完整校验结果包…": "Чтение и полная проверка пакета результатов…",
        "正在保存或恢复并校验科学导入合同…": "Сохранение или восстановление с проверкой контракта научного импорта…",
        "正在计算 Isomap 神经流形…": "Вычисление нейронного многообразия Isomap…",
        "正在计算 PHATE 神经流形…": "Вычисление нейронного многообразия PHATE…",
        "正在读取并限定数据源快照…": "Чтение и ограничение снимка источника данных…",
        "正在解析表格并构建导入预备数据…": "Разбор таблицы и подготовка данных импорта…",
        "正在检查数据源事实与事件属性…": "Проверка фактов источника и атрибутов событий…",
        "正在验证科学含义与规范数据身份…": "Проверка научного смысла и канонической идентичности набора данных…",
        "正在生成 ISI 热力图…": "Построение тепловой карты ISI…",
        "正在生成模式热力图…": "Построение тепловой карты паттернов…",
        "正在生成模拟数据与预览结果…": "Формирование модельных данных и результатов предпросмотра…",
        "正在生成当前检测的权威结果表…": "Формирование авторитетных таблиц для текущего запуска детектора…",
        "正在构建结果表的语义化审阅视图…": "Формирование семантического представления таблиц результатов для проверки…",
        "请输入有效的 ISI 闭区间上下限。": "Введите корректные границы замкнутого диапазона ISI.",
        "请输入有效的 Burst spike 数闭区间。": "Введите корректный замкнутый диапазон числа spike для пачки.",
        "基于 CV/CV2/LV 的 Tonic 初标至少需要 6 个 spike。": "Для предварительной разметки тоника по CV/CV2/LV нужно не менее 6 spike.",
        "MM Tonic 初标仅支持 3–5 个 spike，且 MM 闭区间不能小于 1。":
            "Предварительная разметка тоника по MM поддерживает только 3–5 spike, а замкнутый диапазон MM не может быть ниже 1.",
        "请输入有效的 Tonic spike 数和指标闭区间。": "Введите корректное число spike и замкнутый диапазон метрики для тоника.",
        "手工标记 QC": "QC ручной разметки",
        "当前 spike train 未发现绝对无效 ISI。": "В текущей спайковой последовательности не найдены абсолютно недопустимые ISI.",
        "仅凭时间戳无法判定两端哪一个 spike 错误；红色表示参与可疑间隔，不是单个 spike 的伪迹定论。阈值初标会排除这些 ISI。":
            "По одним временным меткам нельзя установить, какой из конечных спайков ошибочен. Красный цвет означает участие в подозрительном интервале, а не окончательный вывод об артефакте одного спайка. Пороговая разметка исключает такие ISI.",
        "绝对无效 ISI 上限": "Верхняя граница абсолютно недопустимого ISI",
        "手工 ISI 标记与审核": "Ручная разметка и проверка ISI",
        "结构候选": "Структурные кандидаты",
        "Seed / Bridge 诊断": "Диагностика Seed / Bridge",
        "阈值预览": "Предпросмотр порогов",
        "支持方法": "Вспомогательные методы",
        "检测器 / 参数": "Детектор / параметры",
        "模拟 / 预览": "Симулятор / предварительный просмотр",
        "自适应 train 调参": "Адаптивная настройка train",
        "神经网络模型": "Модель нейронной сети",
        "数据 QC": "QC данных",
        "检测结果科学审核": "Научная проверка результатов детекции",
        "结果包回读": "Чтение пакета результатов",
        "爆发": "пачек",
        "暂停": "пауза",
        "强直发放": "тоник",
        "高频强直发放": "Высокочастотный тоник",
        "高频连续发放": "Высокочастотная активность (HFS)",
        "其他": "Другое",
        "未标注": "Без метки",
        "非爆发 / 强负例": "Не burst (вето)",
        "Burst": "пачек",
        "Pause": "пауза",
        "Tonic": "тоник",
        "HF burst": "Высокочастотный burst (HFB)",
        "Long burst": "Длинный burst",
        "HF tonic": "Высокочастотный тоник",
        "HF spiking": "Высокочастотная активность (HFS)",
        "Other": "Другое",
        "Not burst (veto)": "Не burst (вето)",
        "高频 Burst（HFB）": "Высокочастотный burst (HFB)",
        "长 Burst": "Длинный burst",
        "高频 Tonic": "Высокочастотный тоник",
        "高频持续发放（HFS）": "Высокочастотная активность (HFS)",
        "非 Burst（否决）": "Не burst (вето)",
        "状态、事件与其它标记分轨显示；不进行颜色插值。":
            "Состояния, события и другие метки показаны на отдельных дорожках; интерполяция цветов не применяется.",
        "手工标记在同一轨道内覆盖自动显示，但不会删除自动检测记录。":
            "Ручные метки перекрывают автоматическое отображение на той же дорожке, не удаляя запись детектора.",
        "从手动标注学习（预览）": "Обучение по ручным меткам (предпросмотр)",
        "正在生成学习预览…": "Формируется предпросмотр обучения…",
        "生成学习预览": "Сформировать предпросмотр",
        "重新生成预览": "Сформировать заново",
        "应用所选学习阈值": "Применить выбранные обученные пороги",
        "仍然应用所选家族": "Всё равно применить выбранные семейства",
        "所选家族存在科学或跨序列一致性警告":
            "Для выбранных семейств есть научные предупреждения или предупреждения о согласованности между последовательностями",
        "这些警告不会删除证据，但表示建议可能不稳定或与预期模式顺序不一致。请确认后再应用。":
            "Предупреждения не удаляют данные, но предложения могут быть нестабильны или не соответствовать ожидаемому порядку паттернов. Подтвердите применение.",
        "勾选本次要应用的模式家族；未勾选家族保持当前参数和溯源不变。":
            "Выберите семейства паттернов для применения; параметры и происхождение невыбранных семейств не изменятся.",
        "选择是否将该家族的学习值应用为软锚点。":
            "Выберите, применять ли обученные значения этого семейства как мягкие ориентиры.",
        "选择是否应用 HFS 的保守最低支持门槛；应用前需要确认。":
            "Выберите, применять ли консервативные минимальные критерии поддержки HFS; требуется подтверждение.",
        "每个标记片段先投一票，再在每条序列内取中位数，最后让各序列等权参与；绝不把所有 ISI 混池。":
            "Каждый сегмент даёт один голос; затем берётся медиана внутри каждой последовательности, а последовательности получают равный вес. Все ISI не объединяются.",
        "序列覆盖只表示当前数据集内多条 spike train 的一致性；不等于独立神经元、记录会话或动物重复，也不能直接证明外部泛化。":
            "Охват последовательностей отражает только согласованность между spike train в этом наборе данных; это не независимые повторы по нейронам, сеансам или животным и не доказательство внешней обобщаемости.",
        "HFS 的 ISI 分布仍仅作描述；只有证据充分时才建议最少 spike 数和最短持续时间，并必须由用户确认。":
            "Распределение ISI HFS остаётся описательным; минимальное число спайков и длительность предлагаются только при достаточных данных и требуют подтверждения.",
        "Burst、Tonic、HF tonic 与 Pause 以软锚点写入。HFS 的最少 spike 数和最短持续时间是保守下限，会排除支持不足的候选，因此应用前必须确认。":
            "Пачки, тоник, высокочастотный тоник и пауза применяются как мягкие ориентиры. Минимальные число спайков и длительность HFS являются консервативными нижними критериями и исключают кандидатов с недостаточной поддержкой, поэтому требуется подтверждение.",
        "请先生成身份绑定的学习预览；生成本身不会改变检测器。":
            "Сначала сформируйте предпросмотр, привязанный к данным; это не изменяет детектор.",
        "当前标记可供审计，但尚不足以生成兼容的学习阈值。":
            "Метки доступны для аудита, но их недостаточно для совместимых обученных порогов.",
        "撤销上一次学习阈值应用": "Отменить последнее применение обученных порогов",
        "恢复应用前的阈值；不会自动运行检测器。": "Восстановить прежние пороги; детектор автоматически не запускается.",
        "参数已在应用后改变，为避免覆盖修改，回滚已锁定。": "Параметры изменены после применения; откат заблокирован для защиты правок.",
        "标记 %d / 可用 %d 段 · 序列 %d / %d": "Размечено %d / пригодно %d сегм. · последовательности %d / %d",
        "中心 %.3f ms": "Центр %.3f мс",
        "序列中位数范围 %.3f–%.3f ms": "Диапазон медиан последовательностей %.3f–%.3f мс",
        "留一序列命中 %d/%d": "Совпадения при исключении одной последовательности: %d/%d",
        "爆发家族": "Семейство пачек",
        "证据不足": "Недостаточно данных",
        "单序列探索": "Исследовательски: 1 последовательность",
        "双序列暂定": "Предварительно: 2 последовательности",
        "多序列支持": "Поддержано несколькими последовательностями",
        "多序列覆盖": "Охват нескольких последовательностей",
        "可用片段不足，未生成该家族阈值。": "Недостаточно пригодных сегментов; порог не сформирован.",
        "仅覆盖一条序列，结果属于探索性建议。": "Охвачена одна последовательность; предложение исследовательское.",
        "留一序列检查只有部分一致，请结合预览复核。": "Проверка с исключением одной последовательности согласуется частично; проверьте предпросмотр.",
        "留一序列检查不一致；建议补充标记或检查异质性。": "Проверка с исключением одной последовательности не согласуется; добавьте метки или оцените неоднородность.",
        "未观察到预期的 Burst–Tonic 稳健中心顺序；证据仍被保留。": "Ожидаемый порядок устойчивых центров пачек и тоника не наблюдается; данные сохранены.",
        "未观察到预期的 Tonic–Pause 稳健中心顺序；证据仍被保留。": "Ожидаемый порядок устойчивых центров тоника и паузы не наблюдается; данные сохранены.",
        "HFS 稳健中心未位于 Tonic 的较小 ISI 一侧。": "Устойчивый центр HFS не находится со стороны меньших ISI относительно тоника.",
        "HF tonic 稳健中心未与 Burst 的较小 ISI 区间分离。": "Центр высокочастотного тоника не отделён от области меньших ISI пачек.",
        "HFS 建议值是候选筛选下限，而非普通软锚点；仅在确认标记片段可代表 HFS 状态后应用。":
            "Предлагаемые значения HFS — это нижние критерии отбора кандидатов, а не обычные мягкие ориентиры; применяйте их только после подтверждения, что размеченные сегменты представляют состояния HFS.",
        "HFS 最少 spike 数": "Минимальное число спайков HFS",
        "HFS 最短持续时间": "Минимальная длительность HFS",
        "%d 个 spike": "%d спайков",
        "已应用 HFS 保守最低支持门槛；重新运行检测以更新自动结果。":
            "Применены консервативные минимальные критерии поддержки HFS; повторно запустите детектор для обновления результатов.",
        "已写入所选人工学习阈值；请重新运行检测。":
            "Применены выбранные пороги, обученные по ручной разметке; повторно запустите детектор.",
        "没有可应用的人工学习阈值。":
            "Нет доступных для применения порогов, обученных по ручной разметке.",
        "请先确认科学导入并进入规范人工 ISI 工作区。": "Сначала подтвердите научный импорт и откройте каноническую область ручной разметки ISI.",
        "当前人工标记草稿与规范数据集身份不一致。": "Черновик ручной разметки не соответствует каноническому набору данных.",
        "绝对无效 ISI 上界必须是有限的非负数。": "Граница абсолютно недопустимого ISI должна быть конечной и неотрицательной.",
        "绝对无效 ISI 上界不能精确表示为整数微秒；请调整输入精度。": "Граница абсолютно недопустимого ISI не представима точно в целых микросекундах; измените точность.",
        "按 train 留出验证": "Проверка с отложенными spike train",
        "基线检测 vs 仅由校准 train 学到的阈值": "Базовая детекция и пороги, обученные только на калибровочных train",
        "正在运行两次留出检测…": "Выполняются две детекции на отложенных данных…",
        "运行留出验证": "Запустить отложенную проверку",
        "重新运行留出验证": "Повторить отложенную проверку",
        "明确勾选留出 train；其余 train 用于一次性校准。留出标签只用于评价，不进入学习。": "Явно выберите отложенные train; остальные используются для однократной калибровки. Метки отложенных train служат только для оценки и не участвуют в обучении.",
        "留出：": "Отложено:",
        "校准 %d 条 · 留出 %d 条": "Калибровка: %d · отложено: %d",
        "至少需要两条规范 spike train 才能进行 train 级留出验证。": "Для отложенной проверки на уровне train нужны как минимум две канонические spike train.",
        "正在校准 train 上学习，并在留出 train 上分别运行未学习基线和学习后检测；人工留出标签不会反馈到检测器。": "Идёт обучение на калибровочных train и запуск исходного и обученного детекторов на отложенных train; ручные отложенные метки не передаются обратно детектору.",
        "检测参数已改变，当前留出报告已过期；请重新运行。": "Параметры детектора изменились; текущий отчёт устарел. Запустите проверку снова.",
        "报告：校准 %d 条 · 留出 %d 条 · 已评价家族 %d 个": "Отчёт: калибровка %d · отложено %d · оценено семейств %d",
        "留出 train 没有可评价的已审核模式标签；空白行没有被当作阴性。": "В отложенных train нет проверенных меток паттернов для оценки; пустые строки не считались отрицательными.",
        "不自动给出单一通过结论：F1、分段 IoU、边界误差和错误拆分/合并需要共同审阅。": "Единый автоматический вердикт не выдаётся: F1, IoU сегментов, ошибки границ и ложные разделения/слияния нужно оценивать совместно.",
        "已审核 ISI %d": "Проверено ISI: %d",
        "拆分 Δ %+d · 合并 Δ %+d": "разделения Δ %+d · слияния Δ %+d",
        "请至少明确留出一条 spike train，并保留至少一条校准 train。": "Явно отложите хотя бы одну spike train и оставьте хотя бы одну калибровочную train.",
        "验证期间数据、人工标记、train 分工或检测参数已改变；请重新运行。": "Во время проверки изменились данные, ручные метки, роли train или параметры детектора; запустите её снова.",
        "正在校准 train 上学习，并在留出 train 上运行基线与学习后检测…": "Идёт обучение на калибровочных train и запуск базовой и обученной детекции на отложенных train…",
        "train 留出验证已取消。": "Проверка с отложенными train отменена.",
        "train 留出验证完成；结果仅供比较，未改变检测器。": "Проверка с отложенными train завершена; сравнение не изменило детектор.",
        "train 留出验证失败。": "Проверка с отложенными train завершилась ошибкой."
    ]

    // MARK: - Phrase dictionary (ordered, longest/most-specific first; substring substitution).
    // R `stpd_i18n_phrase_dictionary`. Kept conservative for the Mac first pass: pattern terms and a few
    // compounds only (the aggressive single-character R entries are intentionally omitted so labels are not
    // mangled). Order matters — longer phrases precede the shorter ones they contain.

    public static let russianPhraseEntries: [(zh: String, en: String)] = [
        ("仅供演示——非权威。", "Только демонстрация — неавторитетный результат. "),
        ("旧式导入——尚未经过科学确认。", "Устаревший импорт — научно не подтверждён. "),
        ("已确认规范导入的探索视图。", "Исследовательский вид подтверждённого канонического импорта. "),
        ("已加载", "Загружено "),
        ("条序列", " последовательностей"),
        ("个 spike", " spike"),
        ("条 QC 错误", " ошибок QC"),
        ("条 QC 警告", " предупреждений QC"),
        ("QC 已通过", "QC пройден"),
        ("高频强直发放", "Высокочастотный тоник"),
        ("高频连续发放", "Высокочастотная активность"),
        ("长爆发", "Длинный burst"),
        ("强直发放", "тоник"),
        ("爆发", "пачек"),
        ("暂停", "пауза"),
        ("其他", "другое"),
        ("诊断", "диагностика"),
        ("手动标记", "ручные метки"),
        ("检测器报告", "отчёт детектора"),
        ("数据集", "набор данных"),
        ("时间范围", "временной диапазон"),
        ("全时长", "полная длительность"),
    ]

    public static let phraseEntries: [(zh: String, en: String)] = [
        ("仅供演示——非权威。", "Demonstration only — non-authoritative. "),
        ("旧式导入——尚未经过科学确认。", "Legacy import — not scientifically confirmed. "),
        ("已确认规范导入的探索视图。", "Exploratory view of a confirmed canonical import. "),
        ("已加载", "Loaded "),
        ("条序列", " trains"),
        ("个 spike", " spikes"),
        ("条 QC 错误", " QC errors"),
        ("条 QC 警告", " QC warnings"),
        ("QC 已通过", "QC passed"),
        ("已折叠", "Collapsed "),
        ("个重复时间戳", " duplicate timestamps"),
        ("高频强直发放（HF tonic）", "HF tonic"),
        ("高频连续发放（HF spiking）", "HF spiking"),
        ("长爆发（long burst）", "long burst"),
        ("疑似爆发（possible burst）", "possible burst"),
        ("强直发放（tonic）", "tonic"),
        ("爆发（burst）", "burst"),
        ("暂停（pause）", "pause"),
        ("其他（others）", "others"),
        ("非爆发 / 强负例", "not burst / hard negative"),
        ("高频强直发放", "HF tonic"),
        ("高频连续发放", "HF spiking"),
        ("长爆发", "long burst"),
        ("疑似爆发", "possible burst"),
        ("强直发放", "tonic"),
        ("强直", "tonic"),
        ("爆发", "burst"),
        ("暂停", "pause"),
        ("其他", "others"),
        ("诊断", "diagnostics"),
        ("手动标记", "manual labels"),
        ("检测器报告", "detector report"),
        ("数据集", "dataset"),
        ("时间范围", "time range"),
        ("全时长", "full duration"),
    ]
}

/// The owner-approved display terminology for one dataset-wide activity-mode decision.
///
/// These are display sources, never scientific machine tokens: callers must keep using
/// `ScientificDatasetActivityMode` itself for persistence, identity, and analysis. Centralizing the
/// sources here prevents individual views from silently substituting a different Chinese term.
public extension ScientificDatasetActivityMode {
    var activityModeDisplaySource: String {
        switch self {
        case .putativeSingleUnit: "确定的单神经元电活动"
        case .intentionalMultiUnit: "多神经元电活动"
        case .unknownOrUncertain: "未知或不确定"
        }
    }

    var activityModePickerDisplaySource: String {
        switch self {
        case .putativeSingleUnit: "确定的单神经元电活动（Single-unit）"
        case .intentionalMultiUnit: "多神经元电活动（Multi-unit，实验性）"
        case .unknownOrUncertain: activityModeDisplaySource
        }
    }
}
