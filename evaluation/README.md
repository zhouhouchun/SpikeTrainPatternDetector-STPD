# STPD 检测结果评估模块

本文件夹集中管理“检测完成以后”的评估入口、算法索引和可复现实例。
它与 Burst/HFS/Pause 等模式检测器严格分离：本模块只能读取已经生成的事件表和参考事件表，不能读取或修改检测参数，也不会重新运行检测器。

## 具体用途

本模块用于回答两个不同问题：

1. **事件是否被找到？**
   使用同一 train、同一模式内的确定性一对一事件匹配，先最大化匹配事件数，再最大化总 ISI IoU。
2. **事件范围是否准确？**
   单独报告 ISI 覆盖率、起止边界误差和完全相同边界的事件数，不把“找到事件”和“边界完全正确”混为一个指标。

默认同时报告三个等级：

| ISI IoU | 用途 | 解释 |
|---:|---|---|
| 0.25 | approximate detection | 大致找到事件，适合报告宽松事件召回率 |
| 0.50 | substantial overlap | 覆盖事件主体 |
| 0.75 | high overlap | 与参考事件高度一致 |

例如，人工 Burst 标记连续 5 个 spikes，即包含 4 个 canonical ISI；若检测到其中连续 4 个 spikes，则覆盖 3 个 ISI。当预测完全位于参考范围内时，ISI IoU 为 `3/4 = 0.75`：它在三个默认等级中都算检测到，但不算边界完全一致。

## 文件结构

- `run_detection_evaluation.R`：可以在 RStudio Terminal 或命令行直接运行的评估入口。
- `run_nested_hfs_review_evaluation.R`：只读取冻结模拟结果中的 HFS 内局部 Burst Review 候选，单独评价 all nested Burst 与 exact 3-spike Burst3；不进入 canonical 主指标。
- `publication_validation/`：可移植的模拟/真实 LOGO 验证、报告构建和
  Tonic 诊断脚本；原始数据与生成结果不进入 Git。
- `algorithms.yml`：评估算法、正式源文件和职责边界的机器可读索引。
- `examples/`：5-spike Burst 的最小可复现实例。

R 包的正式实现必须保留在项目顶层 `R/` 文件夹中，因为标准 R 包不会自动加载任意子文件夹内的 R 源码。本文件夹不复制核心函数，以避免产生两个行为不同的评估器。

## 输入格式

预测表和参考表均至少包含：

| 列名 | 含义 |
|---|---|
| `train` | spike train 唯一名称 |
| `pattern` | 模式名称，例如 `burst`、`pause` |
| `start_isi` | 起始 ISI 索引，包含端点 |
| `end_isi` | 终止 ISI 索引，包含端点 |

索引采用 STPD canonical right-spike-index ISI 坐标。所有索引必须是正整数，且 `end_isi >= start_isi`。

## 直接运行

在 RStudio Terminal 中进入 Reviewer 项目目录后运行：

```bash
Rscript evaluation/run_detection_evaluation.R \
  evaluation/examples/predicted_events.csv \
  evaluation/examples/reference_events.csv \
  evaluation/examples/output
```

也可以指定类别列和 IoU 等级：

```bash
Rscript evaluation/run_detection_evaluation.R \
  predicted.csv reference.csv output_dir pattern 0.25,0.50,0.75
```

HFS 内嵌 Burst 的独立 Review-only 评价可直接运行：

```bash
Rscript evaluation/run_nested_hfs_review_evaluation.R \
  /path/to/frozen_synthetic_release \
  "Simulator data/clean_synthetic_mechanism_benchmark_v1_0_0/ground_truth/interval_truth_dual_track.csv" \
  test-results/publication_validation/nested_hfs_review
```

该脚本按 `x1`、`x4`、`x10` 分别读取已冻结的 `detector_result.rds`，并将结果写入独立子文件夹；它不会重新检测，也不会写入或覆盖 canonical scoring 文件。也可通过 `STPD_SYNTHETIC_RELEASE_DIR` 提供首个参数。

新版三尺度基准的 Burst FP、FN 与边界误差可以在只读交互观察器中检查：

```r
pkgload::load_all("/path/to/Reviewer")
launch_clean_benchmark_burst_reviewer(
  validation_dir = "/path/to/clean_benchmark_validation",
  benchmark_dir = "/path/to/clean_synthetic_mechanism_benchmark_v1_0_0",
  port = 7321L
)
```

观察器使用五条对齐轨道分别显示原始 spikes、参考 Event、参考 State、
STPD Event 和 STPD State，并提供可平移/缩放的对数 ISI 图。它只读取冻结的
预测与真值，不会重新检测或修改指标文件。

## R 中调用

```r
result <- stpd_evaluate_detection_results(
  predicted = predicted_events,
  reference = reference_events,
  class_col = "pattern",
  iou_thresholds = c(0.25, 0.50, 0.75)
)

result$event_metrics
result$matches
result$isi_metrics
result$isi_metrics_by_train
```

## 输出文件

运行脚本会生成：

- `evaluation_protocol.csv`：本次固定的 IoU 判定协议；
- `event_metrics.csv`：各模式在不同 IoU 标准下的 TP、FP、FN、Precision、Recall、F1；
- `event_matches.csv`：每个匹配事件的 IoU、ISI覆盖和边界误差；
- `isi_metrics.csv`：按模式汇总的 ISI recall、precision 和 Dice；
- `isi_metrics_by_train.csv`：逐 train 的 ISI 指标；
- `detection_evaluation.rds`：完整、可复查的 R 结果对象。

## 使用边界

- 本模块评估的是算法预测与参考标注的一致性，不等同于生物学 ground truth。
- `IoU >= 0.25` 是宽松的事件发现标准，不应单独作为“边界准确”的证据。
- 论文至少应同时报告 IoU 0.25/0.50/0.75、ISI覆盖、边界误差、Precision、Recall 和 F1。
- 参考标注必须在检测完成后才交给本模块；不得用 validation/held-out 标签调整检测阈值。
- 患者或 session 层面的置信区间应采用分组 bootstrap，不能把每个 ISI 当成独立样本。

## 正式源文件

- 结果评估器：`R/28_validation.R` 中的 `stpd_evaluate_detection_results()`；
- 最优一对一匹配器：`R/28_validation.R` 中的 `stpd_match_events_optimal()`；
- 置信区间和 held-out 科学验证：`R/31_scientific_validation.R`；
- 参数敏感性及导出报告：`R/54_parameter_sensitivity_validation.R`；
- 多轨 State/Event/Gap 验证：`R/69_multitrack_validation.R`。
