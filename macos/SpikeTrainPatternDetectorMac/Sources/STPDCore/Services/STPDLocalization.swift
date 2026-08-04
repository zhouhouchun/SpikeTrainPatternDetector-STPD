import Foundation

/// UI language for the app. Default is Chinese (`zh`), mirroring the R/Shiny design where the source
/// strings are Chinese and English is a switchable translation. This is a DISPLAY concern only — it never
/// changes internal machine values (enum raw values, candidate/train IDs, CSV columns, finalLabel strings,
/// decision paths, export schemas).
public enum STPDLanguage: String, Sendable, CaseIterable, Hashable, Identifiable {
    case zh
    case en

    public var id: String { rawValue }

    /// The label shown for this language in the language switch (always its own endonym, not translated).
    public var nativeLabel: String {
        switch self {
        case .zh: return "中文"
        case .en: return "English"
        }
    }
}

/// Pure, centralized localization helper mirroring the R/Shiny client-side translator
/// (`R/56_i18n.R`: `stpd_i18n_exact_dictionary` + `stpd_i18n_phrase_dictionary`).
///
/// Convention: call sites pass the **Chinese source** string. In `zh` mode the source is returned verbatim;
/// in `en` mode the source is translated by an exact full-string lookup, then (if no exact hit) by ordered
/// phrase substitution, falling back to the source string. Unknown strings therefore degrade gracefully.
///
/// Strict non-goal: never route machine strings through here — only user-visible display text.
public enum STPDLocalization {
    /// Translate a Chinese source string for the given language. `zh` returns the source unchanged.
    public static func text(_ source: String, language: STPDLanguage) -> String {
        guard language == .en else { return source }
        if let exact = exactDictionary[source] {
            return exact
        }
        var out = source
        var replaced = false
        for entry in phraseEntries where out.contains(entry.zh) {
            out = out.replacingOccurrences(of: entry.zh, with: entry.en)
            replaced = true
        }
        return replaced ? out : source
    }

    // MARK: - Exact dictionary (full-string Chinese source -> English). R `stpd_i18n_exact_dictionary`.

    public static let exactDictionary: [String: String] = [
        // Language switch + top-level workbench groups/sections (R-parity where present in R/56_i18n.R).
        "语言 / Language": "Language",
        "中文": "Chinese",
        "主图": "Primary views",
        "状态与流形": "State & manifold",
        "诊断": "Diagnostics",
        "验证": "Validation",
        "参数": "Parameters",
        "输出": "Outputs",
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
        "科学验证": "Scientific validation",
        "批处理 / API": "Batch / API",
        "方法 / 审计说明": "Methods / audit notes",
        "检测器 / 参数": "Detector / parameters",
        "模拟 / 预览": "Simulator / Preview",
        "自适应 train 调参": "Adaptive train tuning",
        "神经网络模型": "Neural-network model",
        "数据 QC": "Data QC",
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
        "自动": "Auto",
        "最终": "Final",
        "训练选择": "Train selection",
        "全选": "Select all",
        "清空": "Clear",
        "反选": "Invert",
        "数据集": "Dataset",
        "重置": "Reset",
        "图例": "Legend",

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
    ]

    // MARK: - Phrase dictionary (ordered, longest/most-specific first; substring substitution).
    // R `stpd_i18n_phrase_dictionary`. Kept conservative for the Mac first pass: pattern terms and a few
    // compounds only (the aggressive single-character R entries are intentionally omitted so labels are not
    // mangled). Order matters — longer phrases precede the shorter ones they contain.

    public static let phraseEntries: [(zh: String, en: String)] = [
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
