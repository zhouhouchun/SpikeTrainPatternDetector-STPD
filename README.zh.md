
# SpikeTrainPatternDetector Modular

这是从 `reference prototype` 单文件原型拆分出的模块化 R package。目标不是继续增加检测类别，而是把候选生成、特征计算、最终分类、账本、评估、导出和 Shiny UI 分开，降低维护风险。

事件级验证现在可以按 IoU 比较 AUTO 事件与 MANUAL 标签，输出 precision / recall / F1、边界误差、错标和漏检/多检明细；Basic 参数敏感性扫描会导出 `Parameter_sensitivity_summary.csv`、`Event_level_validation_metrics.csv` 和 `Manual_detector_event_matches.csv`，方便记录“为什么采用这组参数”。

## 验证数据

合成验证材料位于 `validation/simulation_benchmark/`。该目录包含模拟 spike train、模拟器 ground truth、STPD 基准输出、紧凑图表和合成数据验证摘要。

真实 STN spike timestamp 示例位于 `validation/real_patient_example/`。该目录只用于真实多 train 输入的 smoke test 和审稿复现检查；列名已经匿名化为 `train_01` 到 `train_23`，不包含本地 Results、`.nex`、`.rds` 或带本机路径的运行输出。推送到远程仓库前，应确认伦理审批、知情同意和数据共享政策允许分享这类去标识单病人电生理时间戳示例。

## 安装

```r
# 进入父目录后：
devtools::install_local("SpikeTrainPatternDetector_modular", upgrade = "never")
library(SpikeTrainPatternDetector)
launch_spike_detector()
```

首次安装需要本机具备 C 编译工具。macOS 下通常需要：

```bash
xcode-select --install
```

## 架构

- `R/00_utils.R`：通用工具与 refractory-suspect 策略。
- `inst/config/parameters.yml`：默认参数、产品参数默认值、key UI schema、eventness schema 与完整 parameter contract 的单一来源。
- `R/01_default_params.R`：从 YAML materialize 默认参数。
- `R/02_qc_interval_guards.R`：数据 QC 与区间检查。
- `R/03_data_io.R`：RAW / LABELED 数据导入。
- `R/05_isi_context_features.R`：ISI percentile、local median cache、context features。
- `R/09_burst_seed_bridge.R`：burst-family seed / bridge 核心。
- `R/10_structure_layer.R`：structure candidate 层。
- `R/16_semantic_consistency.R`：candidate ledger / event audit / semantic consistency。
- `R/18_ui.R`, `R/19_server.R`：Shiny 前端。
- `src/stpd_core.c`：C 加速核心，目前用于 ISI percentile 与 local-median cache。

“检测器 / 参数”主标签页由 YAML parameter contract 生成，并支持参数 YAML 导入/导出、UI 回填、contract 验证面板、参数变更影响预览、所选 train 的局部 dry-run 事件差异预览、差异事件行点击跳转、raster 与 ISI 时间剖面差异叠加层、差异预览 CSV ZIP 导出和 YAML hash round-trip 检查。parameter contract 现在包含 Basic / Advanced / Expert 分层、显示顺序、section、help 与 control 类型元数据；Shiny 默认只显示按 QC、burst seed/bridge/contrast、HF spiking、tonic、pause、arbitration 排列的 Basic 层，复杂工作流控件仍保留专用 UI。科学验证页进一步提供基于 MANUAL 标签的事件级 Basic 参数敏感性扫描。

## 方法学定位

本工具是候选事件生成和半监督审阅平台，不是无偏最终真值分类器。`possible_burst`、`long_burst`、`high_frequency_tonic`、`high_frequency_spiking` 都需要结合实验背景、生理定义、spike sorting 质量和人工审阅解释。

## engine 重构桥接说明

engine 不再继续增加新的检测类别，而是把开发重点转向结构治理：

- `stpd_detect()` 是新的 canonical detector engine，统一执行 QC、检测、候选特征、最终分类、candidate ledger、event audit 与参数报告。
- `stpd_parameter_registry()` 提供完整参数注册表，用于替代分散的手写 UI 参数映射。
- 参数默认值、key schema 和完整 parameter contract 现在由 `inst/config/parameters.yml` 驱动；`stpd_validate_params()` 会按 contract 检查类型、范围和 choices，修改参数时应先改 YAML，再运行 schema 同步测试。
- “检测器 / 参数”主标签页现在由 parameter contract 自动生成；数据导入、手动标注、可视化、train-specific 校准等复杂工作流控件仍保留专用 UI。
- `final_classify_candidate()` / `final_classify_candidates()` 是最终语义分类的集中边界。
- `scan_short_isi_runs()` 是新的 C 后端短 ISI run 预扫描器，可作为 local-compression / high-frequency 的性能地基。
- 历史版本化内部函数名已清理；后续开发应优先调用 canonical API。

推荐分析入口：

```r
library(SpikeTrainPatternDetector)
params <- default_params()
ds <- build_spike_dataset("spikes.csv", mode = "raw", unit_in = "s")
ds <- stpd_detect(ds, params)
stpd_export_results(ds, params, out_dir = "results")
```

## Release 1.1 Support 层：Mean-ISI 与 Pasquale LogISI / newBD

本版本在 Support 标签页中提供两类 burst-threshold support 方法：

- Mean-ISI support：按照 Chen 等人 mean inter-spike interval 方法估计 ML 阈值并生成 support burst candidates。
- Pasquale LogISI / newBD support：按照 Pasquale、Martinoia 与 Chiappalone 的 logISIH / newBD 方法估计 ISIth，并输出支持候选。该方法基于 log10(ISI_ms) 直方图、lowess 平滑、peak/valley 搜索和 void parameter 阈值。若 ISIth > 100 ms，则使用 100 ms 检测 burst cores，并使用 ISIth 扩展 burst boundaries；若 ISIth 无法可靠估计，则可选择 fallback 到 100 ms CH-style detector。

这些 Support 方法不会写入 AUTO 标签，也不会替代主检测器。它们用于辅助估计每条 spike train 中 burst-ISI 的合理范围，并与 engine eventness / regularity / context audit 一起用于阈值审阅。
