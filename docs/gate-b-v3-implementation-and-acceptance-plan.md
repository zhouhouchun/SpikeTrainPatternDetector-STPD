# Gate B v3 实施与验收规范

## 1. 文档状态

- 状态：`candidate_contract_v1`（正文状态永不在审后原地改为 approved）
- 范围：Gate B 科学本体、AUTO/FINAL 产品、几何闭包、证据绑定、导出与迁移
- 不在本文档中宣称：生物学真值、正式 detector performance、通用 STN 生理阈值
- 当前结论：现有 v2 Gate B 不得自动继承为 v3 authoritative 产品

本文档与《Gate B v3 规范数据、身份与发布合同》共同构成实施输入；前者规定步骤与停机检查，后者冻结精确 schema、坐标、ID、哈希、状态机、签名和原子提交合同。两份文件任一未通过独立复审，都不得进入实现阶段 2。

Gate B v3 只证明：一个预注册的多轨预测记录在给定输入、参数、代码与阈值来源下是确定、自洽、可回放、可审计且故障安全的。Gate C 才能利用独立真值回答算法准确度和生物学有效性。

## 2. 必须先冻结的科学语义

### 2.1 正交维度

内部决策必须分解为：

```text
state_frequency_class  = non_high | high | unresolved
state_regularity_class = regular | irregular | unresolved
```

规范 State 映射为：

| frequency | regularity | 规范 State | 处理 |
|---|---|---|---|
| `high` | `irregular` | `high_frequency_irregular_state` | UI 可显示“HF-irregular”；旧 HFS 仅作 legacy alias |
| `high` | `regular` | `high_frequency_tonic` | HFT |
| `non_high` | `regular` | `tonic` | 普通 tonic |
| `non_high` | `irregular` | 不进入已接受 State | `state_abstention`；不能伪装成 tonic/HF-irregular |
| `unresolved` | 任一 | 不进入已接受 State | `state_abstention` |
| 任一 | `unresolved` | 不进入已接受 State | `state_abstention` |

禁止将“高频”与“非 tonic”视为同义词。HF-irregular、HFT 与 tonic 在已接受 State 中每个 ISI 最多一个。

这里的 `canonical` 仅表示“本 schema 中规范化的记录”，不表示公认生物学真值。所有 State/Gap/Event 都是预注册规则下的操作性预测；产品必须显式保存 `authority_scope`、`biological_ground_truth=FALSE`、`reference_standard_status`、`detector_performance_eligible`、`gate_b_status` 与 `gate_c_status`。

HF-irregular 在 v3 的正式英文名为 `high_frequency_irregular_state`；`high_frequency_spiking`/HFS 只保留为兼容显示别名，避免与 deep-brain stimulation 的 HFS 缩写混淆。

### 2.2 HF-irregular、短中断与 Pause

必须区分三种对象：

1. `direct_support`：直接支持 State 的 ISI 几何。
2. `tolerated_connector`：未达 canonical Pause，但根据冻结规则可连接同一 State episode 的操作性短中断。
3. `canonical_pause`：独立 Pause 证据通过后的操作性 predicted Pause；它必须切断连续 State，不得被降级为 connector。

关键语义：

- predicted Pause 与任何 active State support 不得共占同一 ISI；它也不得处于任何 State episode envelope 内。
- Pause 前后两个独立通过门控的 HF-irregular State 可被上层关系表记录为 `pause_interrupted_hf` 复合 episode candidate。
- 该关系不会把 Pause 纳入 HF-irregular support、active duration 或 State IoU。
- 不使用 `micro_pause` 命名未达 Pause 门槛的 connector，避免语义矛盾。
- 无法确定是 Pause、connector 还是 QC 故障时，记录 `gap_unresolved`，不得强制二分。

### 2.3 Burst 与 State

- canonical Burst 是 Event，HF-irregular/HFT/tonic 是 State；“canonical”仍仅指 schema 规范对象。
- Burst 可与 HF-irregular 重叠；Burst 不得删除、切割或否决独立通过的 HF-irregular。
- Burst 与 HFT/tonic 的观测 State 冲突按冻结规则切分，每个残段必须调用完整 detector 重检，不得几何裁剪后直接继承父类别。
- Burst 不得参与 HF-irregular/Pause 的门控，防止循环定义。
- HFT/tonic 与 Burst 的切分是冻结的操作性 direct-support 规则，不得表述成生物学上不可能共存；父证据必须保留，并允许另表记录 `event_interrupted_state` 关系。

### 2.4 Pause 独立性

Pause 候选必须在 label-blind 副本上计算，且不读取最终 HF-irregular、Burst、tonic 标签。Pause 证据至少记录：

- 预注册的绝对安全下限；
- 排除候选与 guard band 后的稳健局部基线；
- `log(ISI / local_reference)` 或等价 surprise 证据；
- 记录边界、artifact、dropout、unit-loss/sorting-drift 的 QC 状态；
- 多候选扫描时的多重比较处理或预注册简化规则。

数值阈值在 Gate B 中只能被冻结、记录和回放；其生物学充分性由 Gate C 检验。

Pause 的滑窗宽度、guard band、稳健基线估计量、最小有效样本数、边界修正、多候选扫描校正和缺失 QC 时的降级规则，必须以版本化参数进入 effective-threshold 表。仅有 timestamp 时统一称 `predicted_statistical_pause`，不得声称神经机制性静默。

## 3. v3 规范产品

### 3.1 版本原则

- 新增 `stpd_multitrack_auto_v3` 与 `stpd_multitrack_final_v3`。
- 不在 v2 原地重定义 `states`、`per_isi` 或哈希合同。
- v2 已丢失 connector 来源；仅有 v2 产品时必须返回 `requires_redetection`。
- 只有原始输入、完整 effective params/thresholds、同版本代码与运行身份均可验证时，才允许重检生成 v3。

### 3.2 唯一 schema authority

本计划不再重复任何列清单、enum 或准 schema。`docs/gate-b-v3-normative-data-identity-release-contract.md` 是 v3 表名、列顺序、类型/nullability、enum、PK/FK、ID、几何、分母、状态、哈希与文件名的唯一规范来源；本计划只规定实施顺序和停机条件。自动一致性检查必须拒绝计划、合同、registry、prototype 与 consumer contract 之间的任何漂移。

科学语义保持不变：v3 正式使用 `state_episodes`、`state_segments` 和分离 episode membership/active support 的 `per_isi`；predicted Pause/QC/hard boundary 不得属于 episode；connector 只属于 envelope，不是 active support；Event–State 关系只按 episode pair 计一次；Pause 分隔的 HF-irregular fragments 只在 link 层关联。所有精确字段与公式以唯一规范合同为准。

## 4. AUTO 材料化顺序

AUTO v3 必须按以下顺序，不得交叉回馈：

1. 构建 label-blind 检测副本；清除 validation/current-train MANUAL 正负标签与 train-keyed learned ranges。
2. 计算 QC 与 acquisition boundaries。
3. 独立产生 Pause/未解析 gap 证据。
4. 独立检测 State direct support runs，同时产生 connector ledger；不得在合并后丢失 connector 身份。
5. 按 hard boundary 切分，对每个残段调用完整 State detector 重检。
6. 独立检测 Burst Event 与 Review candidates。
7. 应用跨轨兼容性：HF-irregular–Burst 非破坏共存；predicted Pause 与所有 active State support 互斥；HFT/tonic–Burst 按规则切分重检。
8. 按唯一规范合同的 AUTO inventory 材料化全部 canonical tables。
9. 只从已材料化的规范表重建闭包、计数与 manifest。
10. 验证失败时 typed fail-closed；不得以 legacy 投影填充 v3。

label-blind 副本必须从 allowlist 原始列和不可变 acquisition/QC metadata 重建，禁止复制带 attributes 的完整 dataset 再“删除若干字段”。禁止读取 MANUAL/negative/FINAL/review/adjudication、manual-derived ranges、旧 detector/threshold/cache 属性或其派生缓存。注入任意这些列/属性后，AUTO 的规范表字节必须不变。

每次运行的阈值权限、derivation/application input、逐scope实例、group membership与consumer binding，必须逐字实现唯一规范合同第6.13与第8.2节的exact prototypes、枚举和hash DAG；本计划不另列简化字段。实现不得用自报训练组count或单个字符串hash替代逐成员partition与逐consumer binding。

新 patient/session 的默认 Gate C estimand 禁止 `batch_transductive` 或 held-out group overlap；如研究 transductive policy，必须作为独立 estimand、独立结果和独立声明。

## 5. FINAL 和人工复核

- FINAL 从精确 parent AUTO v3 与 append-only adjudication history 确定性回放。
- Review 确认/撤销不得静默改变 State segments、Gap 或 QC boundary。
- 对 State/Gap/episode-link 的人工修改必须使用独立、版本化、可补偿的 transition type；不得复用 Event promotion 动作。v3.1 仅允许保守地拒绝已有 State、通过拒绝一个内部 tolerated-connector 将其拆成恰好两个 child episodes、拒绝已有 Gap、确认/拒绝 link；不允许把 abstained/rejected State或Gap人工扩张为 active support/Pause，也不允许人工 State merge。后两类动作需要新的 support/gap语义与指标分母，必须升级 schema 后再实现。
- v3.1 的 review Event acceptance 只允许不与 HFT/tonic State support 重叠的 candidate；允许与 HF-irregular 非破坏共存。已触发 HFT/tonic 切分重检的 AUTO Event不得被人工 projection-reject，因为恢复父 State需要未在 v3.1 冻结的反事实 State候选。上述两类请求必须 typed `unsupported_transition_for_schema`，不能临时重跑并改写 immutable AUTO candidate/evidence。
- review Event 还必须与当前FINAL全部accepted Event和Gap按ISI不相交；需要替换/合并Event或移除Gap的动作同样升级schema后再做，v3.1不得临时解决冲突。
- 每次回放后重建所有 FK、relationships、per-ISI 投影、counts 与 product hash。
- 无法精确迁移的 v1/v2 review history 必须 `archive_only` 或 `requires_readjudication`。
- transition 必须使用 compare-and-swap parent hash、单调序号、`previous_transition_hash` 和 action-specific typed payload；不允许自由文本承担状态变更语义。

## 6. 两类 Gate B 证据

### 6.1 每次运行的 runtime invariants

至少包含：

- 固定 schema/type/enum；
- train 范围与 ID/FK 唯一性；
- episode–segment–per-ISI 双向闭包；
- 同轨互斥与跨轨兼容性；
- Pause/QC/hard-boundary 零 active State support 侵占；
- Event modifiers 不重复计数；
- relationship 全集从几何重建后逐字节相等；
- AUTO→FINAL lineage 与 transition replay 闭包；
- input/params/effective thresholds/code/QC/diagnostic/history/table/product/manifest 哈希闭包；
- detached validator 必须拒绝 schema/FK/geometry/expected-set/hash 的内部不一致；
- 对“连同全部表与哈希一起协调重封”的来源伪造，只能由 parent-bound deterministic recomputation 或外部 product anchor 检出；detached-only 产品不得获得 official authority。

### 6.2 发布时 evidence attestation

Gate B 的每个 contract ID 必须绑定真实测试证据，而不是布尔常量或字符串。冻结artifact必须逐字实现唯一规范合同第12.1节的`evidence_record`和`test_result_bundle` exact prototypes；本计划不重复字段、状态或hash公式。运行环境、R/package/toolchain与时间由该节签名manifest/attestation承担，不能临时塞入证据行造成准schema漂移。

证据文件不得自我证明。发布证明由隔离的 CI/release job 生成，使用不在仓库和包内的私钥签名；受信公钥指纹由发布渠道独立固定。embedded 签名核心绑定 source、contract、schema、fixture、test result、native build 与 R/toolchain，但不包含自身 hash。构建完成后，由 detached distribution signature 绑定 tarball SHA-256 与最终 embedded-attestation SHA-256，避免包内自引用。

只有同时满足：

```text
verified release attestation + current runtime invariants
```

才能将 AUTO/FINAL 标记为 Gate-B authoritative prediction record。开发机、本地源码树或未签名 tarball 永远只能产生 `product_status=candidate_pending, gate_b_status=pending`。任一证据缺失、签名无效、源码哈希不符或测试失败时，只能保持 candidate/pending 或进入 `failed_closed`。

## 7. Gate C 资格隔离

Gate B AUTO/FINAL metadata 永久保持 `gate_c_status=pending`，Gate C 结果必须是独立、版本化、独立签名的产品，不回写 Gate B。对 Gate B 的所有 consumer：

- `detector_performance_eligible = FALSE`
- 不允许输出 estimand `detector_performance`
- 若为调试而比较 AUTO 与独立标注，必须命名 `exploratory_technical_agreement`
- 不得将 FINAL 与人工标注的一致性重命名为 detector accuracy

Gate C 后续必须独立评价：

- State episode envelope；
- State direct support；
- connector 误合并/误分割；
- canonical Pause event；
- `pause_interrupted_hf` link；
- Burst–HF-irregular coexistence relationship；
- extent/frequency modifiers；
- patient/session cluster uncertainty。

## 8. 分阶段实施与停机检查

### 阶段 0：安全止血

内容：

1. 停止现有 Gate B 基于自证检查的自动 promotion。
2. Gate C pending 时禁止 `detector_performance` estimand。
3. 导出端对 pending/failed 产品明确 omit，不得导出为 official AUTO/FINAL。

检查：

- 定向 testServer/单元反例；
- 无 release attestation 时无法 promotion；
- runtime invariant 失败不得导出 official files；
- parse、diff-check、旧 v1/v2 read-only 回归。

停机条件：任一仍可自动升格或提前输出 performance 的路径存在。

### 阶段 1：冻结 v3 合同

内容：

1. 修订科学合同、兼容性矩阵、边界约定、枚举与指标分母。
2. 按配套规范冻结 v3 全表 schema、列顺序/type/nullability/enum/default、PK/unique/FK、sort order、空表原型、ID/FK 和哈希范围。
3. 为每个 contract ID 指定 fixture/test 证据。

检查：

- 神经生理/统计独立审查通过；
- 软件架构/可复现性独立审查通过；
- schema 原型可以独立构造与验证；
- 不存在未定义语义的 `NA`/自由文本状态。
- 两份 candidate 规范正文不因 verdict 改状态；批准通过外部 `gate_b_approval_manifest` 绑定其 SHA、两份复审与 resolution，避免 SHA 自引用。

停机条件：两位审查者对核心本体、分母或迁移规则仍存在未裁决的重大分歧。

### 阶段 2：detector support/connector ledger

内容：

1. HF-irregular/HFT/tonic merge 函数在合并前直接产生 support/connector ledger。
2. 材料化 hard-boundary 来源与理由。
3. 切分后重检产生新 lineage，禁止裁剪继承。

检查：

- tolerated-gap/hard-break 等号与 ±epsilon；
- `max_gap_count` 与 `+1`；
- Pause 在任何 tolerated 设置下都切断；
- 两侧残段有效/过短的对照；
- HF-irregular、HFT、tonic 都覆盖。

停机条件：任一 episode 无法逐 ISI 还原 direct support 与 connector 来源。

### 阶段 3：AUTO v3

内容：

1. 新增固定原型与 builders。
2. 材料化所有 v3 表及 per-ISI 投影。
3. 实现 detached-internal、parent-bound-recomputed 与 externally-anchored 三档 validators；official accessor 明确要求后两档之一。
4. 实现规范化 typed-table serialization、无环 hash DAG 与全表 manifest 闭包；规范表 hash 与导出 CSV/RDS artifact hash 分列。
5. 冻结并执行资源预算：规范材料化复杂度目标为 `O(n_isi + n_entities + n_edges)`；禁止在多张表复制 timestamp/ISI 大向量；每个 evidence payload、单表行数、产品总字节与峰值内存均有版本化上限，超限必须 `resource_budget_exceeded` fail closed，不能截断科学结果。

检查：

- orphan FK、非法 role、重叠、缺口、错误 per-ISI 投影全部 fail closed；
- 删除/伪造 relationship、counts 或制造与其余规范几何不一致的重封必须被 detached validator 拒绝；
- 将全部规范表协调改造成另一套内部自洽产品时，detached 只能报告 internal integrity，parent recomputation 或外部 product anchor 必须拒绝来源伪造；
- HF-irregular–Burst 共存不改 State episode/segments；
- Pause/QC/hard boundary 侵占为 0；
- v2 输入必须 `requires_redetection`；
- 大输入、全候选命中和 provenance-edge 压力 fixture 必须报告运行时间、峰值内存、规范行数与导出字节；取消只允许在可恢复边界生效并清理 staging，不得留下可读 partial product。

停机条件：内部不一致可通过 detached validator，或任意改变无法被 parent-bound/external-anchor validator 检出；禁止把 detached internal integrity 误写成 detector execution authenticity。

### 阶段 4：FINAL v3 与 review transition

内容：

1. FINAL 从 exact AUTO parent + append-only history 回放。
2. Event、State、Gap、episode-link transition 分类建模。
3. 补偿 transition 撤销，不删除历史。
4. 对需要新 detector evidence或反事实 State恢复的 Event操作实施明确的 schema-version guard，不得以 UI 操作绕过。

检查：

- confirm/revoke 后非目标轨逐字节不变；
- State/Gap 编辑后全部闭包重建；
- stale nonce、dataset/scope/params/input/hash 变更必须拒绝；
- legacy transition 只能 exact migration 或 requires_readjudication。

停机条件：任一 review 动作可洗白 stale/篡改或静默改写其他轨。

### 阶段 5：Gate B evidence attestation

内容：

1. 将每个 contract ID 绑定具体测试。
2. 生成不可缺失的发布证据 artifact。
3. 只建设 release evidence 和签名基础设施；本阶段仍保持 `product_status=candidate_pending, gate_b_status=pending`，不得 promotion。

检查：

- 删除任一 evidence 行、改 source/test/contract hash、注入 failed result 均阻断 promotion；
- 禁止 literal `TRUE` 或单纯字符串存在性通过合同；
- 证据 artifact 本身参与 manifest/hash 闭包。

停机条件：任一 contract ID 无真实 fixture 或证据不能追溯到当前源码。

### 阶段 6：导出、UI 与迁移

内容：

1. official writer 在同一文件系统的不可变 generation 目录中写入全部 v3 表、manifest 与 evidence，逐文件 flush/fsync 后验证，再以目录 rename + 原子 `CURRENT` 指针一次提交；reader 单次解析 generation，不得逐文件读取“最新”。ZIP 也用临时文件验证后原子 rename。
2. 复用 out_dir 时无条件清理 stale v3 artifacts；清理失败阻断导出。
3. 所有 consumer/accessor/UI 必须显式 version-negotiate；official accessor 只接受 signed authoritative v3，candidate viewer 使用独立 API。UI 明确显示 episode envelope、direct support、connector、Pause 与 link candidate，不以颜色暗示生物学真值。
4. v1/v2 只读 dual-write 期的差异审计保留。
5. 审计、日志和诊断执行结构化脱敏：禁止绝对路径、用户名、主机/IP、直接患者或操作者身份进入可移植 bundle；超长技术详情按 registry 限额 fail closed 或写入受控的非规范本地日志，不能静默截断规范 evidence。

检查：

- 完整 ZIP/CSV/RDS/YAML 归档验证；
- stale file、dangling symlink、不可删目标、中途 writer 错误的对抗测试；
- UI 语言切换不改数据字节或当前选择；
- 旧产品不得伪装成 v3。

停机条件：归档可混入 stale/partial v3 文件，或 UI 无法区分 support/envelope。

### 阶段 7：最终回归与签署

必须完成：

1. 所有相关定向 testthat；
2. 完整包回归与 `R CMD check`；
3. 源码、测试、合同、证据与 tarball SHA 冻结；
4. 两位独立审查者在最终字节上复核；
5. P0/P1 为 0；所有延后 P2/P3 有明确 issue 和不影响 Gate B 的论证。
6. 由最终 source/test/contract/schema/fixture 字节生成并验证签名 release attestation 和 detached tarball signature；只有此时执行唯一 promotion。
7. 生成外部 approval manifest；不得回写已审合同正文。任何绑定字节变化都使 approval/attestation失效并重新走最终审查。

Gate B 签署语只能是：

> Multitrack v3 is an authoritative, deterministic and auditable prediction
> record under the frozen Gate B contract. It is not biological ground truth,
> and detector-performance claims remain ineligible until Gate C passes.

## 9. 最小强制 fixture 矩阵

| ID | 情景 | 必须结果 |
|---|---|---|
| GB3-01 | 均质 HF-irregular，无 Burst | 1 episode，全 direct support，0 Event |
| GB3-02 | 2 段 HF-irregular + 1 tolerated connector | 1 episode，2 support segments，1 connector |
| GB3-03 | `gap == tolerated_gap` | 按冻结含等号规则连接 |
| GB3-04 | `gap == hard_break` | 必须拆分 |
| GB3-05 | gap count 超过预算 | 必须拆分 |
| GB3-06 | canonical Pause 低于 tolerated 上限 | Pause 仍必须拆分 |
| GB3-07 | Pause 两侧均有效 HF-irregular | 2 states + 1 gap；link 仅 candidate |
| GB3-08 | Pause 一侧残段过短 | 过短侧拒绝，不建 link |
| GB3-09 | HF-irregular 中 canonical Burst | State episode/segments 不变，1 episode-level relation |
| GB3-10 | Burst 落在 connector | overlap 分解闭合，不重复 Event |
| GB3-11 | HFT/tonic + Burst | 切分后完整重检 |
| GB3-12 | artifact/invalid/unit-loss | 硬边界，无 active State support |
| GB3-13 | high + regular | HFT，不得 HF-irregular |
| GB3-14 | high + unresolved regularity | 弃权/诊断，不得已接受 HF-irregular |
| GB3-15 | 连锁 Pause episode | 禁止递归传递 merge |
| GB3-16 | 保留 Event/State 几何但删除 expected relation/count 并重封 | detached expected-set validator 拒绝 |
| GB3-17 | FINAL confirm/revoke | 非目标轨字节不变 |
| GB3-18 | v2-only migration | `requires_redetection` |
| GB3-19 | evidence 缺失/哈希不符 | 禁止 promotion/export |
| GB3-20 | Gate C pending 评分 | 仅 exploratory technical agreement |
| GB3-21 | 同一路径的 train/session 阈值实例 | 每个 scope 独立材料化、绑定并可回放，不互相覆盖 |
| GB3-22 | development/calibration/validation group 污染 | 从逐成员 partition 重算 overlap 并 fail closed |
| GB3-23 | validation 标注可见算法/阈值/他人结果 | 只能 workflow agreement，不得成为性能 reference |
| GB3-24 | 完整 Pause/State/Event candidate universe 删除 negative 行并重封 | parent expected-set validator 拒绝 |
| GB3-25 | 大输入、全候选命中与 provenance-edge 压力 | 在线性预算内完成，或以 `resource_budget_exceeded` 完整失败；不得截断 |
| GB3-26 | 诊断、日志或 artifact 注入路径/患者/主机身份 | redaction validator 拒绝，正式 bundle 不产生 |
| GB3-27 | 所有合法/非法 product-status 组合 | 合法状态均可无伪值构造；非法组合全部拒绝 |
| GB3-28 | review Burst 与 HFT/tonic State support 重叠 | `event_candidate_accept` 返回 `unsupported_transition_for_schema`；AUTO candidate/evidence逐字不变 |
| GB3-29 | projection-reject 已触发 HFT/tonic split 的 AUTO Event | 明确拒绝且不恢复/猜测 parent State；补偿链与产品字节不变 |
| GB3-30 | 人工 State split 选中内部 tolerated connector | 恰好两个child；connector变unclassified；direct support/class与AUTO candidate/axis evidence不变 |
| GB3-31 | review Burst 与accepted Event或Gap相交 | typed拒绝，不合并、不替换、不改变现有几何 |

## 10. 人工标注与 Gate C 交接

本节不是 Gate B promotion 条件，但必须在 Gate B schema 中预留完整表达：

1. Pass 0：valid/artifact/unit-loss/uncertain。
2. Pass 1：各轨独立标 State support、`reference_pause`/unscorable silence、Burst Event；reference 不读取或复用 detector 的 canonical Pause 门槛。
3. Pass 2：不看算法结果地判断 episode link：`same compound episode | new episode | uncertain`。
4. 至少两位专家独立标注，第三人仅在独立阶段完成后裁决。
5. development/validation 按 patient/session 分组，禁止按 ISI/window 随机拆分。
6. 随机 prevalence sample 与富集灰区的 challenge sample 分开报告。
7. 修改 validation 人工标签不得改变同一输入的 AUTO prediction。

## 11. 独立审查契约

本文档必须接受两份互相独立的审查：

### 审查 A：神经生理与统计

必须评估：

- 本体是否循环定义；
- frequency/regularity、State/Event/Gap/Review 是否真正正交；
- Pause/HF-irregular 语义是否存在生理反例；
- 指标分母、双计数、伪重复和验证泄漏；
- 标注可重复性和灰区/弃权。

### 审查 B：软件架构与可复现性

必须评估：

- schema/ID/FK/geometry/hash 闭包；
- v1/v2/v3 兼容与不可迁移边界；
- AUTO/FINAL/history 回放；
- fail-closed、原子导出、stale cleanup 和协调篡改；
- 测试证据是否真正绑定合同；
- 发布后能否独立验证。

两位审查者不得在提交初稿前读取对方审查文件。主责人必须将每条意见标记为 `accepted | accepted_with_revision | rejected_with_reason`，并在最终文档中追溯处理。

## 12. 决策日志初始值

| 决策 | 结论 | 理由 |
|---|---|---|
| canonical Pause 是否可属于 HF-irregular support | 否 | 静默 Gap 不是直接高频支持 |
| Pause 前后 HF-irregular 是否可关联 | 可，仅上层关系 | 保留复合模式而不改写 State/Gap |
| high frequency 是否自动排除 tonic | 否 | rate 与 regularity 必须拆轴 |
| legacy HFS 与 HFT 是否可作广义并列类 | 否 | HFT 是 high+regular；v3 使用窄义 HF-irregular |
| Burst 是否可在 HF-irregular 中 | 是 | Event–State 正交共存 |
| v2 是否可直接迁移 v3 | 否 | connector provenance 已丢失 |
| Gate B 是否证明 detector accuracy | 否 | 该估计目标属于 Gate C |
