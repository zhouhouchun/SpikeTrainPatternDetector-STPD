# SpikeTrainPatternDetector 算法公式与文献索引审计

审计日期：2026-05-29

审计对象：`SpikeTrainPatternDetector_1_1` 最新脚本，包括 `R/`、`src/`、`inst/config/parameters.yml`、`DESCRIPTION`、`NAMESPACE` 和现有测试目录。

## 总结论

不能确认“文件夹中所有参数性指标的公式都正确，且都有严格的文献索引”。更准确的结论是：

1. 一部分基础指标和支持方法公式是正确的，并且可以挂到明确文献：Mean-ISI 阈值、Pasquale logISIH/newBD 支持层、CV、LV、CV2、PCA/Isomap/diffusion map/PHATE/t-SNE/UMAP、RQA、EM/GMM/BIC、Cohen's kappa、block bootstrap 等。
2. 主检测器的很多关键参数和打分规则不是文献原公式，而是本项目定义的工程启发式：seed/bridge band、burst contrast/classicity、eventness、regularity score、HF-spiking/HF-tonic 状态规则、bridge penalty、score 权重、阈值夹取、manual quantile 校准等。
3. 参数注册表目前主要有 `scientific_note`，没有系统化的 `citation_id`、`formula_ref`、`method_class`、`unit_convention` 字段。因此“严格文献索引”在包内尚未成立。
4. 原审计发现一个会影响候选结果一致性的实现问题：C 原生结构扫描和 R fallback 使用不同的 90% 分位数定义。该问题已在 2026-05-29 修复。

## 严重问题与建议

### P1/P2. C 原生扫描与 R fallback 的 q90 定义不一致（已修复）

位置：

- `src/stpd_core.c:94-105`
- `R/23_native_wrappers.R:37`

C 端 `q90_small()` 原来使用排序后 `ceil(0.90 * k) - 1` 的经验分位/阶统计量。R fallback 使用 `stats::quantile(..., type = 7)` 插值分位数。小窗口候选中，q90 直接进入 `core_q90_ISI_sec`、edge contrast 和候选通过/拒绝判断，因此有 native shared object 时和没有 native shared object 时可能给出不同候选。

修复：C 端已改为 R type-7 90% 分位数；R fallback 已显式声明 `type = 7`。同时修复了 C 端结构扫描漏掉“core 结束于倒数第二个 ISI、右侧仍有合法 flank”的边界问题，并增加 native wrapper 回归测试。

### P2. 手动参数建议中的 LogISI 与 Pasquale 支持层不是同一个算法（已修复）

位置：

- `R/04_param_suggestion.R:30-68`
- `R/07_manual_param_estimation.R:42-60`
- `R/34_support_logisi.R:47-255`

`R/34_support_logisi.R` 明确实现更接近 Pasquale et al. 的 logISIH/newBD 支持层：内部单位说明为 `log10(ISI_ms)`，有概率直方图、lowess 平滑、peak/valley/void 逻辑和 newBD 阈值导出。相反，`estimate_logisi_threshold_train()` 用秒单位取 `log10(x)`、未做概率归一化/平滑，只用最高峰之后的局部峰和 void 阈值。虽然秒和毫秒只差常数平移，阈值换算本身不必然错，但算法步骤明显简化。

问题在于 `R/07_manual_param_estimation.R:42-60` 用这个简化函数生成 `T_B_log`，再参与 seed threshold 的中位数组合。若 UI 或方法报告把它称为 Pasquale/newBD，则会造成文献归属不准确。

修复：`estimate_logisi_threshold_train()` 现在委托 Pasquale logISIH 支持层；`estimate_params_from_manual_pool()` 记录 `T_log_method = "pasquale_logisi"`，仅接受状态为 `resolved` 且不超过 `logisi_mcv_sec` 的阈值。

### P2. 参数 YAML 没有严格文献索引字段

位置：

- `inst/config/parameters.yml:1`
- `R/00a_parameter_schema.R:53`
- `R/26_parameter_registry.R:173`

`parameters.yml` 内有大量 `scientific_note`，但未见系统化 DOI/PMID/文献键字段。当前结构能支持 UI 解释和参数审计，但不足以证明每个参数都有严格文献来源。

建议新增字段：

- `method_class`: `literature_formula` / `generic_statistic` / `engineering_heuristic` / `validation_metric`
- `citation_keys`: 例如 `chen_2009_misi`、`pasquale_2010_logisi`
- `formula_ref`: 公式或算法步骤索引
- `unit_convention`: 秒、毫秒、log10 秒、log10 毫秒等
- `validation_status`: `paper_exact` / `paper_adapted` / `project_defined_tested` / `exploratory`

### P2/P3. Mean-ISI 支持层阈值正确，但窗口搜索/合并策略是实现选择

位置：

- `R/04_param_suggestion.R:8-16`
- `R/33_support_misi.R:47-99`
- `R/33_support_misi.R:101-243`

阈值 `ML = mean(ISI_i < mean(ISI))` 与 Chen/Luo/Deng/Wang/Zeng 2009 的 Mean-ISI 思路一致；burst 条件“若若干连续 ISI 的均值不大于 ML，则识别 burst”也一致。但本实现枚举全部窗口、预算截断、合并重叠窗口，再按 spike 数/持续时间过滤。这些是可接受的实现策略，但不是论文公式本身。

建议：在方法输出中注明“article threshold + exhaustive window implementation”，不要写成完全复刻论文伪代码。

### P3. `drop_self = TRUE` 下 transition entropy 的归一化与 smoothing 约定需修正/说明（已修复）

位置：

- `R/57_state_dynamics.R:67-125`
- `R/57_state_dynamics.R:192-228`

原实现中，`stpd_state_transition_matrix()` 先把 smoothing 加到整个方阵，再过滤自转移观测。如果 `drop_self = TRUE` 且 `smoothing > 0`，自转移伪计数仍保留。`stpd_transition_entropy()` 的最大熵归一化使用 `log(S)`；如果排除了自转移，单行可达状态最多是 `S - 1`，此时归一化熵最大值达不到 1。

修复：`drop_self = TRUE` 时 transition matrix 会清零对角线伪计数；transition entropy 会在计算前同步清零对角线，并用 `S - 1` 作为最大支持规模进行归一化。

### P3. RQA 对角线排除是合理约定，但需要方法说明

位置：

- `R/57_state_dynamics.R:652-681`

实现将主对角线设为 `FALSE`，recurrence rate 分母为 `N^2 - N`。这符合常见“排除 line of identity”的 RQA 约定之一，但 RQA 文献存在多种报告约定。方法报告应明确写出是否排除主对角线。

## 公式与文献逐项审计表

| 组件/指标 | 本地位置 | 公式/算法状态 | 文献索引状态 | 审计结论 |
|---|---:|---|---|---|
| ISI 有效性过滤 | `R/02_qc_interval_guards.R`、`R/05_isi_context_features.R` | `finite & ISI >= min_isi_sec`，0/负 ISI 作为 QC 问题 | 工程/数据质量约定 | 正确；这是包级前置条件，不是神经科学公式 |
| CV | `R/00_utils.R:62-68`、`R/38_event_grammar_core.R:46-52` | `sd(ISI) / mean(ISI)` | 通用统计；神经发放变异性常用 | 公式正确；直接函数未强制排除 0/负值，依赖调用方前置过滤 |
| LV | `R/00_utils.R:51-60`、`R/38_event_grammar_core.R:54-62` | `mean(3*(Ti-Ti+1)^2/(Ti+Ti+1)^2)` | Shinomoto et al. local variation | 公式正确；建议文档注明只对有效正 ISI 解释 |
| CV2 | `R/55_state_space_pca.R:33-42` | `mean(2*abs(Ti-Ti+1)/(Ti+Ti+1))` | Holt/Softky/Koch/Douglas 1996 | 公式正确 |
| q90/q95 分位数 | 多处 R 使用 type 7；C 端 `src/stpd_core.c:94-108` | 已统一为 R type 7 | 通用统计，不是神经科学公式 | 已修复 native/R fallback 不一致 |
| Mean-ISI 阈值 | `R/04_param_suggestion.R:8-16`、`R/33_support_misi.R:47-99` | `ML = mean(ISI_i < mean(ISI))` | Chen et al. 2009 | 阈值正确；检测窗口/合并是实现策略 |
| Mean-ISI burst 检测 | `R/33_support_misi.R:101-243` | 枚举连续窗口，`mean(window_ISI) <= ML` 后合并 | Chen et al. 2009 的核心原则 | 部分文献一致；完整窗口策略需标注为本实现 |
| Pasquale logISIH/newBD 支持层 | `R/34_support_logisi.R:47-255`、`:356-470` | logISIH、peak/valley、void、newBD 阈值支持 | Pasquale/Martinoia/Chiappalone 2010 | 支持层方向正确；已被参数建议复用 |
| LogISI 参数建议 | `R/04_param_suggestion.R:30-90`、`R/07_manual_param_estimation.R:42-60` | 委托 Pasquale logISIH 支持层 | Pasquale/Martinoia/Chiappalone 2010 | 已修复；仅接受 resolved 且不超过 MCV 的阈值 |
| Event grammar seed/bridge | `R/38_event_grammar_core.R:134-233`、`:399-494` | seed band、bridge band、boundary contrast、score 权重 | 项目自定义 | 不是严格文献公式；需要验证数据和参数索引 |
| Dataset ISI seed-centered detector | `R/37_dataset_isi_band_burst_kernel.R:24-56`、`:208-357` | seed/bridge/classicity/boundary score | 项目自定义 | 算法可解释，但不能声称每个阈值来自文献 |
| Seed-bridge/classicity legacy path | `R/36_seed_bridge_classicity.R` | burst/HF/tonic 规则与阈值 | 项目自定义 | 需要文档标注为工程规则 |
| Eventness/regularity audit | `R/32_eventness_audit.R:43-66` | clipped CV/LV/q90q10 + edge/context score | 项目自定义综合分 | 可作为审计特征，不能作为文献公式 |
| Precision/Recall/F1 | `R/31_scientific_validation.R:126-148` | 标准分类指标 | 通用评价指标 | 公式正确 |
| Interval IoU | `R/23_native_wrappers.R:51-68`、`src/stpd_core.c:197-243` | inclusive interval overlap / union | 通用检测评价指标 | 对离散 ISI 区间合理 |
| Greedy event matching | `R/31_scientific_validation.R:88-123` | 按 IoU 降序一对一匹配 | 工程评价策略 | 合理；不是唯一匹配策略 |
| Macro summary | `R/54_parameter_sensitivity_validation.R:333-344` | `mean(..., na.rm = TRUE)` | 通用但有约定风险 | 对无预测/无真值类可能偏乐观，需报告 NA 处理 |
| State-space feature set | `R/55_state_space_pca.R:54-149` | logISI、局部 median/mean/CV/LV/CV2、lag 特征等 | 混合基础指标与工程特征 | 特征本身合理，但整套 feature set 是项目定义 |
| PCA | `R/55_state_space_pca.R:186-238` | `stats::prcomp`，方差解释率 | 通用统计 | 公式/调用正确 |
| Isomap | `R/55_state_space_pca.R:240-502` | kNN 图、最短路测地距离、MDS | Tenenbaum/de Silva/Langford 2000 | 实现方向正确；需说明断图处理和采样 |
| logISI phase portrait | `R/55_state_space_pca.R:504-550` | `logISI_i` vs `logISI_next` | 动态可视化工程特征 | 合理，但不是特定文献公式 |
| Transition matrix | `R/57_state_dynamics.R:67-128` | 相邻状态计数，可 row/joint normalize | 马尔可夫链通用 | 公式正确；`drop_self+smoothing` 已清零对角线 |
| Transition entropy | `R/57_state_dynamics.R:192-236` | row entropy 加权平均；`drop_self` 时除以 `log(S-1)` | 信息论/马尔可夫链通用 | 已修复 `drop_self` smoothing 与归一化约定 |
| Diffusion map | `R/57_state_dynamics.R:443-522` | kernel、density normalization、eigen embedding | Coifman/Lafon 2006 | 实现方向正确 |
| PHATE | `R/57_state_dynamics.R:524-579` | 优先调用 `phateR`；fallback 为 diffusion potential + MDS | Moon et al. 2019 | 依赖 `phateR` 时可索引；fallback 已注明非 canonical |
| RQA metrics | `R/57_state_dynamics.R:625-681` | RR、DET、LAM、TT、Lmax、diagonal entropy | Marwan et al. 2007 | 公式方向正确；需说明排除主对角线 |
| Diagonal GMM/EM/BIC | `R/57_state_dynamics.R:789-845` | diagonal Gaussian mixture EM，`BIC=-2ll+p log n` | Dempster et al. 1977；Schwarz 1978 | 公式正确；局部最优和初始化需说明 |
| HSMM exploratory layer | `R/57_state_dynamics.R:946-1118` | 自定义 duration prior/decoding/logLik | Yu 2010 HSMM 概念可索引 | 当前实现是探索性简化版，不宜声称完整 HSMM 文献算法 |
| Cohen's kappa | `R/57_state_dynamics.R:1145-1184` | `(acc - pe)/(1 - pe)` | Cohen 1960 | 公式正确 |
| Block bootstrap | `R/57_state_dynamics.R:1186-1224` | 固定块重采样 + 百分位 CI | Künsch 1989 | 基本合理；块长选择需报告 |
| t-SNE/UMAP wrappers | `R/57_state_dynamics.R` | 依赖 `Rtsne`/`uwot` 可选包 | van der Maaten/Hinton 2008；McInnes et al. 2018 | 若可选包安装则方法可索引；参数需记录 |

## 文献索引建议

| citation_key | 用途 | 建议索引 |
|---|---|---|
| `chen_2009_misi` | Mean-ISI threshold and burst principle | Chen/Luo/Deng/Wang/Zeng, 2009, Progress in Natural Science, DOI: https://doi.org/10.1016/j.pnsc.2008.05.027 |
| `pasquale_2010_logisi_newbd` | logISIH/newBD burst/network-burst support | Pasquale/Martinoia/Chiappalone, 2010, Journal of Computational Neuroscience, DOI: https://doi.org/10.1007/s10827-009-0175-1 |
| `shinomoto_2005_lv` | LV local variation | Shinomoto/Miura/Koyama, 2005, Biosystems, DOI: https://doi.org/10.1016/j.biosystems.2004.09.023 |
| `holt_1996_cv2` | CV2 adjacent-ISI variability | Holt/Softky/Koch/Douglas, 1996, Journal of Neurophysiology, DOI: https://doi.org/10.1152/jn.1996.75.5.1806 |
| `tenenbaum_2000_isomap` | Isomap | Tenenbaum/de Silva/Langford, 2000, Science, DOI: https://doi.org/10.1126/science.290.5500.2319 |
| `coifman_2006_diffusion_maps` | Diffusion maps | Coifman/Lafon, 2006, Applied and Computational Harmonic Analysis, DOI: https://doi.org/10.1016/j.acha.2006.04.006 |
| `vandermaaten_2008_tsne` | t-SNE | van der Maaten/Hinton, 2008, JMLR: https://www.jmlr.org/papers/v9/vandermaaten08a.html |
| `mcinnes_2018_umap` | UMAP | McInnes/Healy/Melville, arXiv: https://arxiv.org/abs/1802.03426 and JOSS DOI: https://doi.org/10.21105/joss.00861 |
| `moon_2019_phate` | PHATE | Moon et al., 2019, Nature Biotechnology, DOI: https://doi.org/10.1038/s41587-019-0336-3 |
| `marwan_2007_rqa` | Recurrence plot/RQA conventions | Marwan/Romano/Thiel/Kurths, 2007, Physics Reports, DOI: https://doi.org/10.1016/j.physrep.2006.11.001 |
| `dempster_1977_em` | EM | Dempster/Laird/Rubin, 1977, JRSS-B, DOI: https://doi.org/10.1111/j.2517-6161.1977.tb01600.x |
| `schwarz_1978_bic` | BIC | Schwarz, 1978, Annals of Statistics, DOI: https://doi.org/10.1214/aos/1176344136 |
| `cohen_1960_kappa` | Cohen's kappa | Cohen, 1960, Educational and Psychological Measurement, DOI: https://doi.org/10.1177/001316446002000104 |
| `yu_2010_hsmm` | HSMM concept | Yu, 2010, Artificial Intelligence, DOI: https://doi.org/10.1016/j.artint.2009.11.011 |
| `theiler_1992_surrogate` | Surrogate data tests | Theiler et al., 1992, Physica D, DOI: https://doi.org/10.1016/0167-2789(92)90102-S |
| `kuensch_1989_block_bootstrap` | Block bootstrap | Künsch, 1989, Annals of Statistics, DOI: https://doi.org/10.1214/aos/1176347265 |

## 对“所有参数性指标”的最终判断

严格成立的说法只能是：

> 包中若干基础公式和文献支持层有明确文献来源；主检测器把这些指标与项目定义的 seed/bridge/event-grammar/状态规则组合起来，形成一个工程化检测系统。

不应写成：

> 所有参数性指标都来自严格文献公式。

特别是以下参数族必须标为项目定义或经验启发式，而不是文献公式：

- `event_core.seed_band_*`
- `event_core.bridge_band_*`
- `event_core.burst_contrast_min`
- `event_core.possible_burst_contrast_min`
- `event_core.max_bridge_isi_count`
- `event_core.max_bridge_isi_fraction`
- `event_core.max_expansion_isi_each_side`
- `highfreq.spiking_*`
- `highfreq.tonic_*`
- tonic/pause 的多数组合阈值
- eventness/regularity score 及其权重
- dataset/user/manual quantile 夹取规则
- candidate score 中的 `+0.08*core -0.15*bridge -0.25*bridge_fraction` 等权重

这些参数可以是有科学动机的，但需要：

1. 明确标注为 `engineering_heuristic`；
2. 提供验证集表现、敏感性分析和 failure mode；
3. 在 UI/README/方法报告中避免“严格文献公式”的措辞。

## 建议的最小整改路线

1. 已修复 `scan_structure_candidates()` 的 C/R q90 不一致，并补充回归测试。
2. 已把手动参数建议中的简化 LogISI 替换为 Pasquale logISIH 支持层。
3. 下一步扩展参数 schema，加入 `method_class`、`citation_keys`、`formula_ref`、`unit_convention`、`validation_status`。
4. 下一步增加自动导出的 Methods Appendix：每个参数输出默认值、当前值、单位、公式类别、文献键、是否项目自定义。
5. 继续为 Mean-ISI、LV/CV2、RQA 约定增加小型 golden tests。

## 已核实的非问题

- `R/57_state_dynamics.R` 已在 `DESCRIPTION:15` 的 `Collate` 中，并且状态动力学相关函数已在 `NAMESPACE` 导出；这不是当前版本的问题。
- Shiny 0/负 ISI 崩溃的直接保护已在 QC 和 detector error handling 中处理；本报告聚焦公式与文献索引，不重复作为算法公式缺陷列出。
