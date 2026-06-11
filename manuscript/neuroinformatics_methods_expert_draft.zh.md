# 2. 方法

## 2.1 软件实现与设计原则

SpikeTrainPatternDetector 被实现为一个用于可审计 spike-train 事件审阅的 R/Shiny 软件框架。该检测器的目标是从 spike 时间戳数据中生成可解释的候选事件和验证工件，而不是作为一种无监督的生物学真值分类器。这个定位贯穿整个方法设计：每一个自动标签都可以追溯到对应的候选区间、有效阈值表、参数哈希值和审计记录；同时，具有文献来源的支持方法与本项目定义的事件语法检测器保持分离。

该软件主要使用 R 编写，采用 Shiny 和 Plotly 支持交互式审阅，并使用原生 C 后端加速重复执行的底层计算。在当前实现中，原生后端用于加速每条 spike train 的 ISI 百分位数计算、局部中位数缓存、短 ISI run 扫描、结构候选扫描以及区间重叠计算。公共分析流程通过版本中立的入口函数暴露，例如 `build_spike_dataset()`、`stpd_detect()`、`stpd_export_results()` 以及相关验证函数。检测器参数由 YAML 驱动的参数契约 materialize 得到，该契约定义默认值、参数命名空间、用户界面元数据和验证约束。运行时，公共产品参数会被转换为事件语法检测器使用的内部命名空间，从而在兼容旧版保存设置的同时维持稳定的公共参数 schema。

软件遵循五项方法学原则。第一，原始 spike 时序完整性必须在任何模式证据计算之前完成检查。第二，候选生成与最终公共调用保持分离，因此被拒绝或仅用于诊断的窗口不会被误解为已接受的生物学事件。第三，阈值解析在数据集入口处完成，并在本次运行所选 train 内冻结，从而可以通过导出的阈值表和参数哈希复现事件调用。第四，主检测器被描述为一种具有生理动机的事件语法，而不是某一篇文献中 burst 定义的严格复现。第五，人工标注、基准标签和参数敏感性分析被视为性能声明所必需的证据。

## 2.2 输入数据模型

主要输入是 spike 时间戳表。在 raw CSV 模式下，每一个非空数值列被解释为一条 spike train。时间戳可以用秒或毫秒表示，并在内部统一转换为秒。导入器会移除缺失值，对每条 train 的时间戳排序，记录输入顺序是否非单调，并根据时间戳差值重新计算全部 inter-spike intervals（ISIs）。因此，对于一条具有有序时间戳的 train：

`t_1 < t_2 < ... < t_n`，

检测器定义：

`\Delta_i = t_i - t_{i-1}`，其中 `i = 2, ..., n`，

而 `\Delta_1` 未定义。所有事件候选都表示为连续的 ISI 索引区间 `[a, b]`，其中 `2 <= a <= b <= n`。这种约定使事件边界具有明确的索引定义，并允许人工事件与自动事件通过区间重叠进行比较。

在 annotated 输入模式下，可以同时导入时间戳列和人工标签列。如果外部 ISI 列存在，软件会将其保留用于质量控制检查，但不会把它们视为权威数据；检测始终使用由时间戳重新计算得到的 ISI。这样可以避免时间戳顺序和外部 ISI 值之间的不一致悄悄影响检测结果。

导入器还包含防止误将派生结果再次作为原始数据分析的保护机制。如果文件或列名明显类似于先前导出的检测器产物，例如 candidate ledger、eventness audit、final event table、diagnostic candidate table、summary table 或 threshold table，则在 raw 导入模式下默认阻止读取。这降低了把检测输出递归地当作原始 spike 数据重新分析的风险。

## 2.3 预检测质量控制

质量控制在 burst、pause、tonic 或 high-frequency 证据计算之前执行。对于每条 train，QC 表会报告 spike 数、记录时长、发放频率、原始最小 ISI、最小有效 ISI、重复时间戳、零或负时间戳步长、零或负 ISI、硬伪迹 ISI、refractory-suspect ISI、timestamp-ISI mismatch 状态以及平稳性诊断。硬伪迹阈值是最小有效 ISI 阈值。产品 schema 中的默认值为 0.0009 s。第二个 refractory-suspect 阈值默认为 0.001 s，用于标记高于硬伪迹阈值但在单单位 refractory 生理学上仍可疑的区间。

默认情况下，精确重复时间戳、零或负 ISI 以及硬伪迹 ISI 会在模式标签生成之前终止检测。对于探索性诊断，用户可以显式允许在 QC 错误后继续运行；但正式分析应在导入时折叠精确重复时间戳，或报告重复比例及敏感性分析结果。这一策略具有生物学重要性，因为极短区间会不成比例地影响 burst 证据、局部变异性指标、高频状态检测以及基于直方图的阈值估计。因此，检测器将时间戳有效性视为上游科学条件，而不是下游的表面警告。

平稳性被作为解释性警告处理，而不是硬错误。非平稳发放会降低全局 pause 阈值、train 级 ISI 百分位数和 tonic 状态假设的可靠性。因此，软件会导出平稳性诊断和警告信息，使依赖全局阈值的分析可以在相应背景下解释。

## 2.4 参数 materialization 与阈值解析

检测器参数存储在 YAML 驱动的契约中，并在运行时 materialize。公共参数命名空间为 `spiketrainpattern`，而 `event_core`、`event_grammar`、`highfreq`、`tonic`、`pause` 和 `classification` 等内部命名空间会在检测前由产品 schema 派生。检测器记录参数哈希值并导出参数报告，使每次运行都可以被复现和审计。

事件语法所使用的阈值会针对所选数据集解析一次，并随后对所有选中的 train 冻结。对于每一个模式家族，候选阈值可以来自四类来源：显式用户设置、人工标签派生摘要、直方图派生建议或默认值。主动阈值来源策略会在 train 级检测器调用之前确定。导出的阈值表会对每个模式和阈值字段记录用户值、人工派生值、直方图派生值、默认值、有效值以及被选中的来源。

对于给定模式家族 `p`，检测器解析 lower seed bound、upper seed bound、bridge bound，以及在相关情况下的 contrast requirement。在完成来源选择后，软件会施加单调性和几何约束：下界必须非负，上界必须大于下界，bridge bound 不得低于 seed upper bound。额外保护机制防止 high-frequency tonic 阈值落入极端 burst-core 区间，除非用户显式覆盖该设置。

这种冻结阈值设计有两个目的。在计算层面，它防止同一次运行中不同 train 的阈值发生漂移。在科学层面，它使有效阈值策略对审稿人可见，并允许通过导出的参数和阈值记录重新生成完全相同的事件调用。

## 2.5 事件语法检测器

核心检测器是在 ISI 索引候选区间上运行的确定性事件语法。它具有生理学动机，但属于项目定义的方法。该语法将紧凑短 ISI seed、bridge interval、flank contrast、状态规则性、pause gap 和 high-frequency 状态证据组合成一个可审计的规则系统。因此，它应被理解为一种操作性的候选生成层，而不是 burst、pause、tonic firing 或 high-frequency firing 的普适生物学定义。

对于每条选定 train，检测器计算一组候选区间，并为每个区间构造证据向量。候选 `c = [a, b]` 包含区间内 ISI `\Delta_a, ..., \Delta_b`，在可用时包含 flank interval `\Delta_{a-1}` 和 `\Delta_{b+1}`，其 spike 数为 `b - a + 2`，在时间戳可用时持续时间为 `t_b - t_{a-1}`，并包含区间内分位数、平均 ISI、coefficient of variation、local variation、maximum-to-mean ratio、bridge count、bridge fraction 以及 manual-negative overlap 等摘要统计。随后，候选会被分配临时标签、优先级和诊断决策路径。最后，加权区间选择步骤从候选池中选出非重叠的公共自动调用。

### 2.5.1 Burst-family 检测

Burst-family 检测从紧凑 seed run 开始。当某个 ISI 落在解析得到的 seed lower 和 seed upper 阈值之间时，该 ISI 被视为支持 seed。连续的 seed-supporting interval 构成 seed run，且 seed run 必须包含至少指定数量的 seed ISI。每个 seed run 会通过有限数量的 bridge interval 向左、向右扩展。Bridge interval 允许候选内部存在小的中断，同时防止 seed 吸收整个高发放或非平稳片段。扩展受最大 bridge ISI 数、bridge fraction 以及两侧最大扩展步数限制。

对于每个扩展后的候选，检测器计算区间内紧凑性的参考量，主要基于候选内部 ISI 的第 90 和第 95 百分位数。Flank contrast 由事件前后 ISI 相对于区间内紧凑性的比例计算。概念上，对于候选 `c`：

`S_pre(c) = \Delta_{a-1} / Q90(c)`，`S_post(c) = \Delta_{b+1} / Q90(c)`，

其中 `Q90(c)` 是候选内部有效 ISI 的第 90 百分位数。双侧 canonical burst 证据要求两个 flank 均超过解析得到的 contrast 阈值。仅具有部分证据或位于边界的候选可以被保留为 `possible_burst`，而不是直接丢弃，从而保留具有生物学可能性但存在歧义的区间供审阅。

Burst-family 检测器还包含针对紧凑短 ISI episode 的结构性 rescue 路径。当候选相对于 train 级背景显示出强压缩，即使经典双侧 flank 较弱或不可用，该路径也会保留这些候选。只有当结构证据在配置规则下足够强时，这些候选才会被接受为 burst 或 long_burst；否则，它们会被保留为 possible_burst 审阅候选。检测器默认采用 soft q95 bridge 策略：适度的 q95 溢出会降低候选分数，而不是自动拒绝；但严重溢出仍然可以阻止接受。

Spike 数标准用于区分 classical burst、long_burst 和 prolonged burst-like candidate。默认产品设置将 3-10 个 spike 视为 classical burst 尺度，将 11-15 个 spike 视为 long_burst 尺度；更长的紧凑结构通常被降级为 possible_burst，除非额外的研究特异性标准支持接受。这样的保守处理避免将长时间高发放 epoch 与离散 burst event 混为一谈。

### 2.5.2 High-frequency spiking

High-frequency spiking 被建模为一种持续状态或 epoch，而不是 burst-family event。这是检测器中的一个核心生物学区分。High-frequency spiking 候选由持续支持 run 构成，其中大多数 ISI 较短，同时允许有限数量的中等 gap。默认产品设置要求至少 30 个 spike，short-ISI upper reference 为 0.020 s，epoch-level q90 limit 为 0.025 s，bridge allowance 为 0.035 s，tolerated gap 为 0.075 s，并限制较大 ISI 的比例和最大连续数量。

检测器评估 median、q80、q90 和 q95 ISI 摘要、short-ISI fraction、bridge fraction、tolerated-gap fraction 以及 large-ISI burden。候选可以通过 strict q90 路径或 robust q80/majority 路径被接受，这反映了一个事实：具有生物学意义的持续高频状态可以包含偶发的中等区间，而不因此失去高频属性。候选评分给予长 high-frequency 状态 span-aware priority，使其不会被多个低特异性的 possible_burst、tonic 或 pause 候选切碎。然而，如果拟定的 high-frequency spiking 状态被许多嵌入的 burst packet 主导，则该候选可以被拒绝，从而在证据更支持 packetized bursting 而非持续状态时保留 burst-family event 的可见性。

### 2.5.3 High-frequency tonic 与 tonic 状态

High-frequency tonic 和 tonic 候选表示相对规则的发放状态。High-frequency tonic 检测使用高频 ISI 上界，同时施加下限 floor，以避免将极端 burst-core interval 标记为 tonic-like high-frequency discharge。它还基于 coefficient of variation、local variation、maximum-to-mean ratio 进行规则性检查，并对占主导的 burst-core run 施加 veto。

Tonic 检测使用由配置 tonic bounds 和 train-level ISI 摘要派生的自适应中等 ISI 区间。Tonic 候选必须满足 spike 数要求和规则性约束，并且不能被 burst-core ISI 主导。检测器还使用 burst-overlap safeguards，避免将嵌入在更宽候选中的短 ISI burst sequence 误解释为 tonic regularity。这些 tonic 和 high-frequency tonic 标签应被理解为操作性状态注释，其生物学含义取决于细胞类型、实验制备、spike sorting 质量和实验背景。

### 2.5.4 Pause 检测

Pause 候选是长 gap interval。Pause 层将绝对 pause 阈值与 train-level context 结合。有效 pause floor 受 train 上部 ISI 分布和 tonic-state guardrail 约束，候选长区间还可以进一步根据局部和全局 median ISI context 检查。这种相对化设计降低了普通慢发放被误标记为 pause 的可能性，同时保留长的孤立 gap 作为可审阅事件。

由于 pause 检测对非平稳性和长静默尾部尤其敏感，检测器会导出 pause 阈值字段、局部 median context、全局 median context 和平稳性警告。因此，pause 标签应结合 QC 表和阈值表一起解释。

## 2.6 加权区间选择与人工标签策略

候选生成可能会从不同层产生相互重叠的区间。为了构建单一公共自动标签轨道，SpikeTrainPatternDetector 采用加权区间选择。每个候选根据其标签家族、显式优先级、得分和跨度获得一个 value。算法随后在配置的模式集合下选择一组非重叠区间，使总 value 最大化。这避免了任意的先到先得选择，并使 burst、possible_burst、high-frequency、tonic 和 pause 候选之间的竞争具有确定性。

人工标签与自动证据分开处理。当 manual locking 启用时，人工标注区间保持 final-label dominant，自动标签不会写入这些 ISI。同时，自动证据仍然可以在诊断层生成和审计。这种设计支持半监督工作流：专家标注受到保护，但检测器仍然可以显示算法原本会提出哪些候选，以及人工和自动证据在哪些地方不一致。

负向人工标签，例如显式 not-burst 标签，可以 veto 对应区间中的候选接受。这类 veto 会记录在诊断审计中，而不是静默删除候选历史。

## 2.7 Candidate ledger 与 eventness audit

检测器将公共候选调用与诊断候选窗口分开。被接受并选中的区间会导出到公共事件表和 candidate ledger。被拒绝、仅用于 profile、被阻断、被抑制或未被选择的窗口保留在 diagnostic candidate audit 中。这种分离对于可复现性至关重要：公共生物学计数不会被内部候选窗口膨胀，同时被拒绝候选仍可用于 failure mode 检查。

在公共候选被选出后，软件计算候选特征表，用于最终分类审计和下游报告。候选特征包括事件持续时间、spike 数、区间内 ISI 分位数、平均和最大 ISI、coefficient of variation、local variation、maximum-to-mean ratio、flanking ISIs、edge contrast、bridge count、bridge fraction、局部 context 摘要以及状态特异性证据字段。

Eventness audit 提供额外的解释层。它通过综合 boundary contrast、context contrast、return-to-baseline evidence 和 regularity，估计候选更像事件还是更像状态。Regularity 由 coefficient of variation、local variation 和 q90/q10 ISI ratio 概括。Eventness 不被用作独立生物学真值，而是帮助区分紧凑、边界明确事件和持续状态的审计特征。Long_burst 候选还接受 context contrast、short-ISI fraction、duration 和 internal outlier burden 的额外检查。具有歧义的 eventness zone 会被显式标记为需要审阅。

## 2.8 文献关联的支持方法

主事件语法是项目定义的，但 SpikeTrainPatternDetector 还提供用于阈值证据和比较的文献关联支持层。这些支持方法不会覆盖主自动标签，除非用户在下游工作流中有意使用其输出。

Mean-ISI 支持层遵循 Chen 等人（2009）提出的阈值原则。给定有效 ISI `T_i`，首先计算 train-level mean ISI。Mean-ISI 阈值定义为：

`ML = mean({T_i : T_i < mean(T)})`。

当连续区间内 ISI 的均值不超过 `ML` 时，该窗口被识别为候选 burst window。软件实现会枚举连续窗口，施加 spike 数和持续时间约束，并合并重叠窗口。阈值原则具有文献来源；穷举枚举、预算控制和合并策略属于实现选择，并应在报告中如实说明。

LogISIH/newBD 支持层实现 Pasquale 风格的阈值证据工作流。ISI 表示为 `log10(ISI_ms)`，随后构造 logISI histogram，进行平滑，搜索峰和谷，并使用 void-parameter evidence 支持阈值解析。如果阈值无法可靠解析，该方法会报告失败状态，而不是静默生成最终生物学标签。其输出旨在用于阈值校准、可视化以及与事件语法的比较。

## 2.9 基于人工标签的事件级验证

当人工标签可用时，自动事件会通过 ISI 索引区间重叠与人工事件比较。对于自动区间 `A = [a_1, a_2]` 和人工区间 `M = [m_1, m_2]`，交集长度和并集长度均基于 inclusive ISI index 计算，intersection-over-union 定义为：

`IoU(A, M) = |A intersect M| / |A union M|`。

候选/人工事件对按照 IoU 从高到低进行贪心一对一匹配，并受到最小重叠标准约束。基于这些匹配，软件报告 precision、recall、F1、false positive、false negative、boundary error 和 label-confusion summary。软件提供两种评价模式。Strict high-confidence evaluation 保留具体标签，适用于人工事件被作为特定类别 ground truth 的情形。Candidate-family evaluation 将相关标签合并，例如 burst 和 possible_burst，适用于评估检测器是否召回生物学上合理的事件家族，而不是评估精确子类型分配。

软件还实现参数敏感性扫描。Basic-layer 参数可以被扰动，并重新计算完整的事件级验证结果。导出的敏感性报告记录 precision、recall、F1、IoU、boundary error 和 confusion structure 如何随参数扰动变化。这些输出用于方法记录，并降低在没有稳健性证据的情况下报告狭窄调参结果的风险。

## 2.10 状态空间与群体探索模块

SpikeTrainPatternDetector 包含可选的探索模块，用于 ISI state-space analysis、neural manifold visualization、event-aligned activity 和 slice tensor construction。这些模块不是核心检测器所必需的，也不应被单独用作检测事件代表生物学真值的证据。

对于单 train 状态空间分析，软件计算 ISI 派生特征，包括 logISI、local median ISI、local mean ISI、coefficient of variation、local variation、CV2、lagged ISI features 和 local context summaries。CV 针对有效正 ISI 计算为 `sd(ISI) / mean(ISI)`。LV 计算为：

`mean(3 * (T_i - T_{i+1})^2 / (T_i + T_{i+1})^2)`，

CV2 在相邻有效 ISI 对上计算为：

`mean(2 * |T_i - T_{i+1}| / (T_i + T_{i+1}))`。

线性和非线性 embedding，包括 PCA、Isomap、diffusion-map-style embedding、PHATE、UMAP 和 t-SNE，被作为可视化和假设生成工具提供。报告这些输出时，应说明参数设置、相关随机种子，并提供稳定性或 shuffle control。

对于群体层面的探索，spike train 可以被 binning 为 time-by-neuron matrix，使用 counts 或 rates，并可选择 log1p 或 square-root count transform 等变换。随后可应用 dimensionality reduction，并将事件标签作为注释叠加，而不是作为 embedding 输入。这个顺序对于避免循环推理非常重要。Event-state centroid distance、dispersion、permutation test、event-triggered trajectory、latent velocity、curvature 和 decoding analysis 可以用于检验事件状态是否解释了群体活动结构，而不仅仅是视觉上分离。

可选 sliceTCA 工作流基于选定 spike train 和 task event 构造 trial-by-neuron-by-time tensor。当所需 Python 环境可通过 reticulate 使用时，R 包可以调用官方 Python sliceTCA 后端。否则，软件导出 tensor diagnostics 和降维摘要。使用该模块的研究应报告 tensor 维度、bin width、event window、rank 设置、优化参数、随机种子、重建指标和后端版本。

## 2.11 可复现性输出与报告

每次检测器运行都会导出足够的元数据以复现和审计分析。公共结果字段包括有效阈值表、预检测质量表、candidate diagnostic audit、candidate ledger、event table、candidate feature table、final decision audit、eventness audit、parameter report、parameter-validation report、run metadata，以及在人工标签可用时的 manual-label validation summary。导出的 run metadata 包含参数哈希值和所选 train。结果一致性检查和科学验证摘要用于识别 event table、candidate ledger 和 final decision layer 之间的不匹配。

用于发表时，建议报告软件版本、代码仓库 release tag 或 DOI、R 版本、操作系统、依赖版本、输入单位约定、重复时间戳处理策略、硬伪迹阈值、refractory-suspect 阈值、阈值来源策略、完整参数文件或参数哈希、分析的 train 数量、每条 train 的 spike 数、QC 警告、有效阈值表和验证模式。如果检测器输出被用于生物学推断而不仅仅是软件演示，则应报告人工标签一致性、可用时的标注者间一致性、参数敏感性，以及与文献关联支持方法的比较。

## 2.12 方法学定位

该方法最适合被理解为一个可审计的候选生成与验证框架。Burst 调用表示在所选阈值策略下具有足够局部对比的紧凑短 ISI 结构。Pause 调用表示在所选 pause 和 context 规则下的长 gap interval。Tonic 和 high-frequency 调用表示由局部发放速率和规则性证据定义的状态样时间区间。这些标签可以具有科学价值，但其解释取决于实验制备、细胞类型、spike sorting 质量、脑区、疾病或刺激状态、行为，以及该研究可用的验证数据。因此，关于检测器准确性或生物学机制的强声明应由人工标注、合成基准、独立生理或行为终点、shuffle control 和参数敏感性分析共同支持。
