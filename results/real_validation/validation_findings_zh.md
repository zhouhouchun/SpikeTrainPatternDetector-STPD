# PD GPe/STN/GPi 三种参数模式真实数据验证

## 结论

三地区、三种模式共 9 项运行已完成。少量已知模式的分组留出验证能显著修复 Broad HFS 的自动入口，但不能保证 Burst、Pause 和 Tonic 同时改善。STN 的 Burst/Pause 已具有较好的纯自动结果；GPe/GPi 的 Burst/Pause 为中等水平；GPi Tonic 可以并且已经作为正式 State 终点评价，但目前仍是明确未解决的问题。

因此，当前证据支持把 STPD 描述为“自动初始化 + 少量示例/参数库适配 + 人工可编辑”的检测框架，不支持把它描述为所有核团、所有模式均能高性能零样本全自动检测的算法。

## 冻结协议

- `automatic`：零人工示例；检测器只读取 timestamp，采用 label-blind 数据集自适应入口。
- `partial_known`：按 recording group 五折留出；每折、每种模式最多从训练组抽取 10 个 episode；验证组标签对检测器不可见。
- `full_params`：用同一数据中的全部参考 episode 估计完整参数后回代检测。该结果仅是适配/重代入上限，不是独立验证。
- Event 与 State 分轨评价：Burst/Pause 为 Event，Broad HFS/Tonic 为 State。
- HFT 与 HF-irregular 合并为 Broad HFS 主终点。
- GPi Tonic 为正式 State；STN 的短 tonic-like 标签仍只作描述性 review，不进入正式 Tonic 指标。
- 每条 train 在相应轴上至少有 10 个显式参考 ISI 才进入该轴评分；canonical Pause 不计入直接 State support。

## 区间级 F1

| Region | Regime | Burst | Pause | Broad HFS | Tonic |
|---|---|---:|---:|---:|---:|
| GPe | automatic | 0.422 | 0.320 | 0.105 | — |
| GPe | partial-known | 0.308 | 0.635 | 0.944 | — |
| GPe | full-params | 0.379 | 0.598 | 0.963 | — |
| STN | automatic | 0.821 | 0.758 | 0.024 | — |
| STN | partial-known | 0.768 | 0.687 | 0.831 | — |
| STN | full-params | 0.775 | 0.781 | 0.835 | — |
| GPi | automatic | 0.606 | 0.526 | 0.063 | 0.109 |
| GPi | partial-known | 0.617 | 0.699 | 0.943 | 0.039 |
| GPi | full-params | 0.691 | 0.676 | 0.950 | 0.000 |

## Episode 级 F1（IoU ≥ 0.25）

| Region | Regime | Burst | Pause | Broad HFS | Tonic |
|---|---|---:|---:|---:|---:|
| GPe | automatic | 0.500 | 0.328 | 0.112 | — |
| GPe | partial-known | 0.370 | 0.610 | 0.391 | — |
| GPe | full-params | 0.440 | 0.581 | 0.453 | — |
| STN | automatic | 0.852 | 0.786 | 0.000 | — |
| STN | partial-known | 0.722 | 0.704 | 0.531 | — |
| STN | full-params | 0.730 | 0.797 | 0.513 | — |
| GPi | automatic | 0.643 | 0.536 | 0.034 | 0.029 |
| GPi | partial-known | 0.630 | 0.668 | 0.423 | 0.065 |
| GPi | full-params | 0.664 | 0.638 | 0.431 | 0.000 |

每项 precision、recall、support 和 recording-group cluster bootstrap 95% CI 见 `primary_f1_summary.md`/`.csv`。

## GPi Tonic 正式验证

原始工作簿含 502 个 Tonic ISI、31 个 episode；应用预先统一的 State 轴参考充分性规则后，正式评分集合为 495 个 ISI、30 个 episode。三种模式的支持量完全一致。

- 自动模式：117/495 个 Tonic ISI 被正确识别，其余 378 个被归为 `other`。
- 部分已知：10/495 个被识别为 Tonic，123 个被归为 Broad HFS，362 个归为 `other`。
- 完全参数：0/495 个被识别为 Tonic，51 个归为 Broad HFS，444 个归为 `other`。

完全参数估计得到的 Tonic ISI 区间和 LV 上限并非明显不合理；约 26/30 个参考 episode 可通过简化的 ISI-band、LV 和时长条件，但最终 State 产品没有保留它们。这表明主要问题位于 Tonic 入口/状态竞争/产品材料化，而不是“GPi 没有可验证 Tonic”或“人工 Tonic 全部不符合基本参数”。更多同数据标签没有解决该问题。

## 可用于论文和审稿回复的谨慎表述

1. 在三个独立人工标注的 PD 核团数据集上完成了 label-blind 自动检测、recording-group 留出的小样本适配验证和全参考参数上限分析。
2. STN 的 Burst/Pause 在零人工示例下已有较高区间与 episode 一致性。
3. 少量参考 episode 对 Broad HFS 的跨核团适配作用显著，但 Event 的最优参数具有更强的 train/region 异质性。
4. GPi Tonic 的正式验证结果较低，应作为已识别限制及下一阶段改进目标，不能隐藏或并入 HFS 指标。
5. 三份数据来自不同核团/数据集，因此属于跨数据集独立专家参考验证，不是同一 spike train 的三标注者一致性研究，不能计算或宣称 inter-rater reliability。

## 完整性审计

- 9/9 组合的结果、参数 manifest、split、预测区间、预测事件、bootstrap 和 detector RDS 均存在。
- 所有工作簿的 ISI 索引、时间戳链和 ISI 差值均精确重建。
- automatic 模式的人工示例数均为 0。
- partial-known 每折 calibration/validation recording group 无交叉。
- GPi Tonic 在三种模式下均保持 495 ISI、30 episode 的固定评分支持量。
- Tonic/HFS 语义回归测试、辅助文件镜像一致性和 `git diff --check` 均通过。
