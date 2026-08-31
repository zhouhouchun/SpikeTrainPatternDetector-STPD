# Gate B v3 规范数据、身份与发布合同

## 1. 状态与规范性

- 状态：`candidate_contract_v1`（正文永不因审查 verdict 原地改写）
- 合同版本：`gate_b_v3_contract_1`
- 目标产品：`stpd_multitrack_auto_v3`、`stpd_multitrack_final_v3`
- 适用范围：AUTO/FINAL 的操作性预测记录、运行时自洽、回放、导出与发布证明
- 明确不证明：生物学真值、临床意义、跨数据集泛化或 detector accuracy

本文使用“规范/canonical”仅表示 schema 中唯一、可验证的表达。所有 State、Event、Gap 和 episode link 都是预注册算法下的预测记录；`biological_ground_truth` 在 Gate B 永远为 `FALSE`。

本合同第 1–13、15–17 节以及第 14–14.1 节的 consumer/Gate-C guard/API 中，`MUST` 是 Gate B 阻断条件；`SHOULD` 可延后，但必须有记录的 P2/P3 issue。第 14.2–14.3 节显式标记为非 Gate-B 阻断的 Gate C handoff，须由未来独立 Gate C 合同采纳或版本化替代，不进入 Gate B schema bundle、required contract-ID set或approval。实现不得新增未在本合同登记的 nullable 字段、自由文本状态或隐式 fallback。

## 2. 基础类型、空值与规范化

### 2.1 类型记号

```text
chr   UTF-8 NFC 字符串
int   有符号 32-bit 整数
dbl   有限 IEEE-754 binary64；禁止 NaN、Inf、-Inf 与负零
lgl   TRUE 或 FALSE
json  满足本文 canonical JSON 规则的 UTF-8 字符串
lst   仅用于非规范 R API envelope 的命名 list；不进入科学产品 hash；nullable `lst?` 的唯一空值是 `NULL`
```

字段后缀 `!` 表示不得缺失，`?` 表示允许类型化 `NA`。除非列定义明确写出，空字符串不等于缺失。ID/hash 列不得为空字符串。所有空表必须保留完全相同的列顺序、R 类型与 factor-free 属性。

### 2.2 时间与 ISI 坐标

对严格递增 timestamp `t[1], ..., t[N]`：

- `isi_index=i` 对应半开/闭区间 `(t[i], t[i+1]]`，`i=1,...,N-1`；
- `left_spike_index=i`，`right_spike_index=i+1`；
- `isi_start_time_sec=t[i]`，`isi_end_time_sec=t[i+1]`；
- `isi_sec=t[i+1]-t[i]`；
- 区间记录的 `start_isi` 与 `end_isi` 均为包含端点；
- `n_isi=end_isi-start_isi+1`；
- `n_spikes=n_isi+1`；
- `start_time_sec=t[start_isi]`；
- `end_time_sec=t[end_isi+1]`；
- `duration_sec=end_time_sec-start_time_sec`。

两个相邻 ISI 区间可以共享一个边界 spike，但不得共享 ISI。parent-bound validator 必须从 raw normalized timestamps先重建完整 `per_isi` coordinate spine；detached validator只能从该规范 spine重建其他表几何并明确其较弱保证。两者都不得信任 interval 表随行携带的派生数值；detached不能声称 per-ISI spine本身来自真实 raw parent。

所有 duration/overlap 求和按 `isi_index` 升序使用 IEEE-754 binary64逐项加法，禁止 BLAS、并行重排或 fused operation；fraction 在唯一规定的分子、分母分别求和后只做一次除法。validator 要求 canonical binary64结果逐位相等；只有 Gate C 的统计匹配阈值可以使用另行注册的数值容差，Gate B 几何闭包不使用“近似相等”。

### 2.3 行排序

所有表先按 `train` 的 UTF-8 byte order 排序，再按各表规定的 key 排序。禁止使用 locale collation、factor level 或当前 UI 顺序。相同规范内容在不同机器上必须得到相同 typed-table hash。

### 2.4 审计字符串

所有 `*_utc` 使用 RFC 3339 UTC、六位小数、`Z` 结尾。所有 pseudonym 是 domain prefix + 64 lowercase hex，由受控 salt/HMAC在数据集外生成；不得含姓名、病历号、用户名、主机或绝对路径。canonical `train` 也必须是稳定 `tr_` pseudonym；原始文件/worksheet/unit label到train pseudonym的映射只能保存在受控外部映射表，不进入可移植 bundle。语义版本使用 SemVer；repository path统一相对 POSIX path。审计时间可进入 history/approval hash，但不进入 detector source context或AUTO科学实体 ID。

### 2.5 固定 schema 标识与合同摘要

下列值是常量，不由实现者或运行时选择：

```text
schema_version = stpd_multitrack_v3_1
status_schema = stpd_multitrack_v3_status_1
identity_schema = stpd_multitrack_v3_identity_1
manifest_schema = stpd_multitrack_v3_manifest_1
artifact_manifest_schema = stpd_multitrack_v3_artifact_manifest_1
response_schema = stpd_multitrack_v3_response_1
evidence_record_schema = stpd_gate_b_v3_evidence_record_1
reference_schema = stpd_multitrack_v3_reference_1
migration_schema = stpd_multitrack_v3_migration_1
approval_schema = stpd_gate_b_v3_approval_1
signature_schema = stpd_gate_b_v3_signature_1
test_bundle_schema = stpd_gate_b_v3_test_bundle_1
release_attestation_schema = stpd_gate_b_v3_release_attestation_1
```

每张 inventory 表的 `table_schema` 机械生成为 `stpd_multitrack_v3_<table_name>_1`；`<table_name>` 必须逐字等于第 8.4 节的 ASCII 表名。所有表内 `schema_version` 必须等于上面的固定值，reference/migration 表分别使用其固定 schema 字段。未知或大小写不同的值均为 `unknown_schema`，不得 fallback。

`contract_sha256` 是本规范合同文件规范化为 UTF-8/LF（不去除尾随空格、不重排 Markdown）后的 SHA-256；`schema_contract_sha256` 是阶段 1 生成、经两位审查者一并复核的 canonical schema bundle hash。schema bundle 必须包含：固定 table prototypes、列顺序/type/nullability/enum、PK/unique/FK/sort、状态矩阵、ID payload、hash DAG、inventory、registry semantic definitions 与 contract-ID set。实施计划 SHA 不进入科学实体 ID，只进入外部 approval manifest。合同文件与 schema bundle 任一变化都需要新 approval，不能沿用旧 verdict。

## 3. State 本体与接受门

### 3.1 正交证据轴

```text
state_frequency_class  = high | non_high | unresolved
state_regularity_class = regular | irregular | unresolved
evidence_status        = pass | fail | gray_zone | insufficient
```

完整映射如下；不存在默认 `else`：

| frequency | regularity | 结果 |
|---|---|---|
| `high` | `irregular` | `high_frequency_irregular_state` |
| `high` | `regular` | `high_frequency_tonic` |
| `non_high` | `regular` | `tonic` |
| `non_high` | `irregular` | `state_abstention: non_high_irregular` |
| `unresolved` | `regular` | `state_abstention: frequency_unresolved` |
| `unresolved` | `irregular` | `state_abstention: frequency_unresolved` |
| `high` | `unresolved` | `state_abstention: regularity_unresolved` |
| `non_high` | `unresolved` | `state_abstention: regularity_unresolved` |
| `unresolved` | `unresolved` | `state_abstention: both_axes_unresolved` |

只有两个轴均 `pass`、有效 ISI 数和 duration 均达到冻结下限、所有统计量有限、且离阈值灰区有冻结 margin 时，才可进入 accepted `state_episodes`。`high_frequency_spiking` 只允许作为旧 UI 别名；v3 状态码使用 `high_frequency_irregular_state`。

### 3.2 Episode、segment 与 active support

- `state_episode` 是上层 episode envelope；
- `direct_support` segment 是被 State detector 直接接受的连续 ISI；
- `tolerated_connector` segment 仅把两个已独立通过的 direct-support fragment 归入同一 episode；
- connector 不是 active State support，不进入 active duration、direct-support IoU 或 State confusion matrix；
- accepted predicted Pause、QC、algorithmic hard break 或 acquisition boundary 不得位于 episode envelope 内；
- 同一 episode 的 segments 必须是最大连续同 role、同阈值来源、同 lineage 的区间；相邻且属性完全相同的 segments 必须合并。

### 3.3 Connector 非循环规则

connector 仅在以下条件全部满足时成立：

1. 前后 direct-support fragments 单独均通过完整 State 门控；
2. 中间区间不通过 predicted Pause，且无 QC/condition/acquisition/algorithmic hard boundary；
3. gap 长度、gap/local reference ratio、单 episode connector count、累计 connector duration fraction 和 episode span 均在冻结限值内；
4. 前后 frequency 与 regularity 证据分别通过预注册等效性区间；“差异不显著”不算等效；
5. link 仅由原始相邻 fragments 建立；合并出的 episode 不得参与第二轮递归桥接。

### 3.4 Gate B 冻结的 State evidence 算法

本合同冻结计算方法，不宣称阈值数值已具生物学充分性；所有数值界值由 threshold instance提供：

```text
frequency_stat_hz = n_valid_isi / sum(valid_isi_sec)
cv2_j = 2 * abs(isi_j - isi_(j+1)) / (isi_j + isi_(j+1))
regularity_stat = median(cv2_j over adjacent valid pairs)
```

valid ISI 必须有限、正值、通过 QC，且不能跨 hard boundary。frequency：`>= high_enter_hz` 为 high pass，`<= high_exit_hz` 为 non_high pass，中间为 gray，要求 `high_exit_hz < high_enter_hz`。regularity：`<= regular_upper_cv2` 为 regular pass，`>= irregular_lower_cv2` 为 irregular pass，中间为 gray，要求 `regular_upper_cv2 < irregular_lower_cv2`。不足 `state_min_valid_isi`、`state_min_duration_sec`、无相邻 pair或统计量非有限统一 insufficient。

阶段 1 的 exact threshold paths 固定为 `state.frequency.high_enter_hz`、`state.frequency.high_exit_hz`、`state.regularity.regular_upper_cv2`、`state.regularity.irregular_lower_cv2`、`state.state_min_valid_isi`、`state.state_min_duration_sec`；每条 `state_candidate/state_axis_evidence/state_episode` 必须各自绑定这六条且不得多绑。`frequency_pass_bound` 对 high/gray/insufficient 写 `high_enter_hz`、对 non_high 写 `high_exit_hz`；`regularity_pass_bound` 对 regular 写 `regular_upper_cv2`、对 irregular/gray/insufficient 写 `irregular_lower_cv2`。两个 `*_gray_margin` 分别固定为 `high_enter_hz-high_exit_hz` 和 `irregular_lower_cv2-regular_upper_cv2`。insufficient 时两轴均 `evaluable=FALSE/status=insufficient` 并必须有 typed reason；否则 reason 必须 NA。gray 时 `evaluable=TRUE/status=gray_zone`，不得投影 accepted State。

episode aggregation 不重新利用 connector 计算 State：每个 direct-support segment独立通过同一 class；只有相邻 segments class相同且connector decision=connect才归入一 episode。任何 segment变类/gray/insufficient都不得靠episode合并“补过门”。等号按上述 `<=/>=` 归 pass，`±epsilon` fixture由 binary64 nextafter构造。

connector equivalence使用确定性冻结边界，不使用“p>0.05”：

```text
abs(log(pre_frequency_stat / post_frequency_stat))
  <= log(connector_rate_ratio_margin)
abs(pre_regularity_stat - post_regularity_stat)
  <= connector_regularity_abs_margin
```

两侧必须各自满足最小样本。上述 estimator/rule semantic bytes作为签名 registry entries，阈值路径通过实例绑定，不允许实现选择替代 estimator。

## 4. Pause、QC 与 Event 语义

### 4.1 Predicted Pause

`canonical_pause` 在 Gate B 的正式语义为 `predicted_statistical_pause`。它必须 label-blind，并保存：绝对下限、局部参考、guard band、稳健估计量、有效样本数、log-ratio/surprise、multiple-scan policy、边界校正、QC eligibility 和算法版本。

冻结计算：v1 candidate universe 是每个 `isi_sec>=pause_scan_seed_sec` 的单个 eligible ISI，故 candidate 必须 `start_isi=end_isi`；相邻长 ISI 之间存在真实 spike，不合并成一个“静默”Gap。局部参考取同 train 对称 `pause_reference_window_n_isi` 内、排除 candidate、`pause_guard_band_n_isi`、QC/hard-boundary跨越后的有效 ISI，中位数为 `local_reference_sec`。经验上尾概率 `p=(1 + count(reference_isi >= candidate_duration))/(n_valid+1)`；同 detection root、同 train、同 gap policy、同 hard-context epoch 的全部 candidate 构成一个预注册 family，使用 Benjamini–Hochberg 得 `q`。validator 必须先从这四个 parent 字段重建 family partition，再由完整 sorted candidate-ID set 重建 family ID；不得按提交的 `multiple_testing_family_id` 分组。accepted 要同时满足 `duration>=pause_absolute_min_sec`、`local_ratio>=pause_local_ratio_min`、`q<=pause_fdr_alpha` 和 QC eligibility；边界删失、样本不足或绝对/相对证据冲突进入 abstain，普通不达门候选 rejected。exact paths 固定为 `pause.scan_seed_sec`、`pause.absolute_threshold_sec`、`pause.local_ratio_min`、`pause.local_window_isi`、`pause.guard_band_isi`、`pause.local_min_valid_isi`、`pause.score_alpha`；每条 candidate 绑定恰好这七条。`window/guard/min_valid` 必须为非负整数，且 `window>=1`、`guard<window`、`min_valid>=1`。局部 estimator 固定 `pause_local_window_median_v1`；其参考集合来自 parent per-ISI spine，而不是从已扫描候选表反推。只有 non-NA `raw_p_value` 进入 BH，完整 family 中 insufficient/QC-ineligible candidate仍保留但 q 为 typed NA；family identity 包含该 root/train/policy/epoch 下的完整 candidate-ID set。

通过的 Pause 始终是 State episode hard boundary。未通过但可作为 connector 的区间不得称 pause；证据不足进入 `gap_unresolved` abstention。只有 timestamp 而无 waveform/sorting QC 时，输出必须保留 `qc_eligibility=timestamp_only_uncertain`。

### 4.2 Event 与 State

accepted Event 的基类是 `burst_event`。extent 与 frequency 是同一 event 的 modifiers，不产生重复 event row。Burst 可覆盖 HF-irregular State；两轨均保留。HFT/tonic 与 Burst 的切分是项目冻结的操作性 direct-support 规则，不是生物学不可能共存声明；父 evidence 不能删除。

阶段 1 modifier 的可执行操作性定义固定如下，不宣称数值门槛已有 Gate C 生物学验证：extent 使用 `event_extent_n_spikes_v1`，统计量为该 Event 的 `n_spikes`；`>=event.extent.prolonged_min_spikes` 为 prolonged，else `>=event.extent.long_min_spikes` 为 long，否则 classic，且 `2<=long_min<prolonged_min`。frequency 使用 `event_frequency_rate_hz_v1`，统计量为 `n_isi/duration_sec`；`>=event.frequency.high_enter_hz` 为 high，`<=event.frequency.high_exit_hz` 为 non_high，中间为 unresolved/gray，且 `0<=high_exit<high_enter`。每条 extent evidence 只绑定两条 extent path，每条 frequency evidence 只绑定两条 frequency path；不得借用 Event selector 或 State threshold。modifier evidence JSON 必须覆盖 `{event_id,modifier_domain,modifier_value,evidence_status,estimator_registry_id,statistic_value,pass_bound,gray_margin,threshold_instance_id}`，其 source hash、evidence ID 与 modifier ID 必须从该 exact payload逐级重算。

### 4.3 Structure-first Burst 候选生成

Gate B v3 的 Burst candidate universe 必须包含生产检测链的 `structure_first_local_flank_contrast_v1`。该规则先扫描每个 train 的连续 ISI 窗口，再运行 Event selector；不得用 selector 代替 generator。窗口宽度为闭区间 `[structure_first_min_isi_count, structure_first_max_isi_count]`，候选内部必须全部通过 artifact/QC eligibility。双侧窗口要求左右 flank 相对候选 `q90(ISI)` 的算术比均不低于 `structure_first_contrast_min`，且两比值几何均值不低于 `structure_first_geom_contrast_min`；endpoint 仅在 `structure_first_allow_endpoint=TRUE` 时可进入候选，且是否直接成为 canonical Burst 另由 `allow_one_sided_as_canonical` 决定。train 有足够有效 ISI 时，候选 `q90` 必须不高于 `structure_first_background_fraction * train_quantile(structure_first_compactness_quantile)`；单个内部尾值只有在 q90 已通过、双侧 flank 对最大内部 ISI 仍通过且 tail ratio 不高于 `structure_first_max_internal_tail_ratio` 时容忍。该生成不读取 burst seed/bridge band，label-blind执行不得读取 MANUAL 正/负标签。

exact parameter paths 为：`event_core.min_spikes`、`event_core.classic_max_spikes`、`detector.artifact_min_valid_isi_sec`、`spiketrainpattern.burst.structure_first_enabled`、`spiketrainpattern.burst.structure_first_min_isi_count`、`spiketrainpattern.burst.structure_first_max_isi_count`、`spiketrainpattern.burst.structure_first_contrast_min`、`spiketrainpattern.burst.structure_first_geom_contrast_min`、`spiketrainpattern.burst.structure_first_compactness_quantile`、`spiketrainpattern.burst.structure_first_background_fraction`、`spiketrainpattern.burst.structure_first_min_train_valid_isi`、`spiketrainpattern.burst.structure_first_max_internal_tail_ratio`、`spiketrainpattern.burst.structure_first_allow_endpoint` 与 `spiketrainpattern.burst.allow_one_sided_as_canonical`。registry semantic definition、实际生产函数和阈值边界 fixture 必须三者同版本；candidate deletion/addition、弱 flank、homogeneous non-burst、artifact 和 label-bearing input均须 expected-set或负例失败。数值生物学性能仍属于 Gate C。

## 5. 公共枚举与状态机

```text
product_kind = auto | final
product_status = not_present | materializing | candidate_pending |
                 gate_b_authoritative | failed_closed |
                 requires_redetection
gate_b_status = pending | attested | failed
gate_c_status = pending
authority_scope = none_candidate | automatic_prediction_record |
                  reviewed_prediction_record
reference_standard_status = none | independent_reference_available |
                            adjudicated_reference_available
record_status = predicted | review_confirmed | review_rejected
```

固定约束：

- `candidate_pending`：`record_authoritative=FALSE`、`authority_scope=none_candidate`；
- `gate_b_authoritative + auto`：`authority_scope=automatic_prediction_record`；
- `gate_b_authoritative + final`：`authority_scope=reviewed_prediction_record`；
- 所有状态：`biological_ground_truth=FALSE`；
- Gate B 产品中永远：`gate_c_status=pending` 且 `detector_performance_eligible=FALSE`；未来 Gate C 使用独立产品与独立签名 attestation，绝不回写 Gate B product metadata；
- `failed_closed/requires_redetection/not_present` 不得带 official canonical tables；
- 旧 run 的 v3 产品在新 run 建立前必须 strip；内存中不得存在可被 accessor 误读的 `stale` current product。

所有 required 字段无隐式 default；builder 缺值即报错。所有 nullable 字段的唯一 default 是对应类型的 `NA`。布尔字段不得用 `NA`。除各表已列枚举外，以下公共枚举固定：

```text
state_class = high_frequency_irregular_state | high_frequency_tonic | tonic
segment_role = direct_support | tolerated_connector
frequency_modifier = high | non_high | unresolved
extent_modifier = classic | long | prolonged | unresolved
qc_eligibility = eligible | timestamp_only_uncertain | ineligible
equivalence_status = equivalent | not_equivalent | insufficient
state_candidate_decision = accepted_direct_support | abstained | rejected |
                           interrupted_parent
event_candidate_decision = accepted_event | review_candidate | rejected
episode_link_status = algorithmic_candidate | review_confirmed | review_rejected
context_completeness = complete | partial | minimal_timestamp_only
failure_stage = input_normalization | materialization | validation | replay |
                migration | resource_budget | export | attestation | cancelled
label_access = none | development_reference_only
threshold_source_class = fixed_preregistered | development_trained |
                         per_train_unsupervised | batch_transductive
adaptation_unit = none | train | session | dataset_batch
```

Reference-only exact enums：

```text
rater_role = primary | secondary | repeat_rater | adjudicator
confidence_code = high | medium | low | not_rateable
adjudication_status = resolved | uncertain | unscorable
time_scale_policy = fixed_absolute | fixed_relative_to_candidate
waveform_display_policy = hidden | qc_only | visible
```

`annotation_pass` 只允许 `0|1|2`，`annotation_round>=1`；validation reference 的 primary/secondary/repeat rows 不得使用 `adjudicator` role。其余 algorithm-specific class/reason/status 必须引用 6.16 的不可歧义 registry ID；禁止在 `chr` 列中临时发明新 code。

algorithm-specific route、reason、rule 与 payload code 不作为自由枚举扩张，而是引用 6.16 的签名 registry；registry 字节变化会改变 contract/schema hash 并触发重新审查。

## 6. 精确表合同

下列列顺序是规范顺序；不得追加隐式列。`PK`、`FK` 和排序规则属于合同。

### 6.1 状态 envelope 与 materialized identity

`product_status_envelope` 对所有状态均可构造，且不伪造尚不存在的 run/hash：

```text
status_schema:chr!, product_kind:chr!, product_status:chr!,
execution_id:chr?, detection_root_id:chr?, product_hash:chr?,
gate_b_status:chr!, gate_c_status:chr!, authority_scope:chr!,
record_authoritative:lgl!, biological_ground_truth:lgl!,
reference_standard_status:chr!, detector_performance_eligible:lgl!,
integrity_level:chr!, failure_stage:chr?, failure_code_registry_id:chr?,
requires_redetection_reason_registry_id:chr?, release_attestation_hash:chr?,
product_attestation_hash:chr?, reference_manifest_hash:chr?
```

`PK=(product_kind)`；状态合法组合见第 11 节。`execution_id` 是 UUIDv4 审计 ID，不进入 scientific entity ID 或 canonical product hash。

`gate_b_status/authority_scope/record_authoritative/integrity_level` 是验证器从 release/product attestation与parent recomputation结果派生的缓存值，reader不得把持久化字符串当作信任来源；缓存与重算值不一致即拒绝。这样协调改写 status envelope 不能升格产品。

`reference_standard_status=none` 时 `reference_manifest_hash` 必须 NA；另两种状态必须携带并验证第 6.20 节的 exact reference manifest。该字段只可由 reference accessor在 AUTO/FINAL 构建完成后附加到非科学 status envelope；detector builder与canonical product hash均不得读取它。

`materialized_product_identity` 只在 `candidate_pending|gate_b_authoritative` 存在：

```text
identity_schema:chr!, product_kind:chr!, detection_root_id:chr!,
source_context_hash:chr!, product_context_hash:chr!,
parent_auto_product_hash:chr?, history_head_hash:chr?,
canonical_manifest_core_hash:chr!, product_hash:chr!,
contract_sha256:chr!, schema_contract_sha256:chr!
```

`PK=(product_kind,detection_root_id,product_hash)`。AUTO 的 parent/history 必须 `NA`；FINAL 必须绑定 parent AUTO 与 history head。counts 只来源于 `canonical_manifest`，不得在第二处手工维护。

凡 schema 明列 `detection_root_id` 的 canonical 表，该列必须等于 identity core；凡 schema 明列 `source_context_hash` 的表也必须逐行相等。`detection_root_id="dr_"+source_context_hash`；它们是同一 immutable detection root，不是两个可独立编辑的来源。root-independent registry/partition表不伪造该列，而是由 canonical manifest与其内容hash绑定到当前产品。

### 6.1a `state_candidates`

State detector 产生的完整候选宇宙必须先材料化，accepted、abstained、rejected 与被 Event 操作性打断后重检的 parent 都不能只存在于日志：

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
state_candidate_id:chr!, candidate_generation_rule_registry_id:chr!,
source_candidate_key_hash:chr!, start_isi:int!, end_isi:int!, n_isi:int!,
n_spikes:int!, start_time_sec:dbl!, end_time_sec:dbl!, duration_sec:dbl!,
candidate_decision:chr!, state_axis_evidence_id:chr!, decision_reason_registry_id:chr!,
lineage_id:chr!
```

`candidate_generation_rule_registry_id` FK to `state_candidate_rule` registry。`PK=state_candidate_id`；unique `(detection_root_id,train,start_isi,end_isi,candidate_generation_rule_registry_id,source_candidate_key_hash)`；sort `(train,start_isi,end_isi,state_candidate_id)`。每行恰有一条 axis evidence；AUTO 中 `accepted_direct_support` 必须至少映射一条 direct-support segment，`abstained` 必须恰有一条 state abstention，`rejected` 不得进入 accepted segment，`interrupted_parent` 必须进入一条 `event_interrupted_state_relationships` 并由完整重检决定 child。FINAL 中 immutable accepted candidate若不再投影，必须由 exact `state_projection_reject` transition解释；不存在 transition 的缺失仍是闭包错误。候选表本身不接受 UI/MANUAL/FINAL 来源。

### 6.2 `state_episodes`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
state_episode_id:chr!, state_class:chr!, state_frequency_class:chr!,
state_regularity_class:chr!, frequency_evidence_status:chr!,
regularity_evidence_status:chr!, classification_rule_registry_id:chr!,
start_isi:int!, end_isi:int!, n_isi:int!, n_spikes:int!,
start_time_sec:dbl!, end_time_sec:dbl!, envelope_duration_sec:dbl!,
direct_support_n_isi:int!, connector_n_isi:int!,
direct_support_duration_sec:dbl!, connector_duration_sec:dbl!,
support_fraction:dbl!, classification_evidence_id:chr!, record_status:chr!
```

`PK=state_episode_id`；unique `(detection_root_id,train,state_class,start_isi,end_isi)`；sort `(train,start_isi,end_isi,state_class,state_episode_id)`。

同一 train 的 accepted State episodes 必须在 ISI 上两两不相交；因此 `per_isi.state_episode_id` 是全函数而不是有损投影。不同 State evidence 对同一区间竞争时，3×3 axis mapping与abstention先解决，不允许靠 row order选 winner。

固定公式：`support_fraction=direct_support_duration_sec/envelope_duration_sec`；`envelope_duration_sec=direct_support_duration_sec+connector_duration_sec`；三项均从 segment ISI duration 求和重建，分母严格大于 0。

`classification_evidence_id` 必须指向一个冻结 `state_episode_classification_evidence_v1` evidence row，内容恰为 ordered direct-support candidate IDs、ordered connector-decision IDs、两个 State 轴、episode aggregation registry ID 与 `source_threshold_instance_set_hash`；该 set 只含 episode生成前已确定的 candidate/axis/connector source threshold-instance IDs，不得含以尚未生成的 episode/segment/relationship作为consumer的 binding ID。它不得包含 UI/MANUAL/review信息。删除或调换任一 source会改变 evidence与episode ID；episode生成后的 consumer binding rows是派生闭包，不得反馈进入episode/evidence ID。

### 6.3 `state_segments`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
state_segment_id:chr!, state_episode_id:chr!, segment_index:int!,
segment_role:chr!, state_class:chr!, state_frequency_class:chr!,
state_regularity_class:chr!, start_isi:int!, end_isi:int!, n_isi:int!,
n_spikes:int!, start_time_sec:dbl!, end_time_sec:dbl!, duration_sec:dbl!,
segment_rule_registry_id:chr!, source_state_candidate_id:chr?,
state_axis_evidence_id:chr?, connector_decision_id:chr?, lineage_id:chr!
```

`segment_role=direct_support|tolerated_connector`；direct support 必须有 source candidate与 axis evidence且 connector decision为 NA；connector 的 source candidate/axis evidence必须 NA并恰有 connector decision。`PK=state_segment_id`；FK `(state_episode_id,train,state_class)` 与 `source_state_candidate_id -> state_candidates`；unique `(state_episode_id,segment_index)`；sort `(train,state_episode_id,segment_index)`。

### 6.4 `per_isi`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
isi_index:int!, left_spike_index:int!, right_spike_index:int!,
isi_start_time_sec:dbl!, isi_end_time_sec:dbl!, isi_sec:dbl!,
state_episode_id:chr?, episode_state_class:chr?,
state_episode_membership:lgl!, state_segment_id:chr?,
state_segment_role:chr?, active_state_segment_id:chr?,
active_state_class:chr?, state_direct_support:lgl!,
event_id:chr?, gap_id:chr?, controlling_boundary_id:chr?
```

`PK=(detection_root_id,train,isi_index)`；sort `(train,isi_index)`。connector 行必须有 episode/segment、active 字段为 `NA`、`state_direct_support=FALSE`。direct-support 行 active segment 等于 segment。非 episode 行所有 episode/active 字段为 `NA/FALSE`。

### 6.4a `event_candidates`

Event selector 的完整候选宇宙必须可重建，不只保存 winner：

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
event_candidate_id:chr!, candidate_generation_rule_registry_id:chr!,
source_candidate_key_hash:chr!, candidate_class:chr!,
start_isi:int!, end_isi:int!, n_isi:int!, n_spikes:int!,
start_time_sec:dbl!, end_time_sec:dbl!, duration_sec:dbl!,
candidate_decision:chr!, decision_reason_registry_id:chr!,
base_event_evidence_id:chr!, lineage_id:chr!
```

`candidate_class=burst_candidate`；`candidate_generation_rule_registry_id` FK to `event_candidate_rule` registry；`candidate_decision=accepted_event|review_candidate|rejected`；PK `event_candidate_id`；unique `(detection_root_id,train,start_isi,end_isi,candidate_generation_rule_registry_id,source_candidate_key_hash)`；sort `(train,start_isi,end_isi,event_candidate_id)`。AUTO accepted Event 必须由至少一个 `accepted_event` candidate edge派生；FINAL 另允许由一个 immutable `review_candidate` source + exact `event_candidate_accept` transition派生 `record_status=review_confirmed` Event。rejected candidate保留且永不投影到 `per_isi.event_id`。validator从 parent重建由签名 generator 产生的候选全集，并在 AUTO按独立签名 selector、在 FINAL按 parent+history重建 maximal/disjoint Event set。

### 6.5 `events`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
event_id:chr!, event_class:chr!, extent_modifier:chr!,
frequency_modifier:chr!, base_event_evidence_id:chr!,
extent_evidence_status:chr!, extent_modifier_evidence_id:chr!,
frequency_evidence_status:chr!, frequency_modifier_evidence_id:chr!,
start_isi:int!, end_isi:int!, n_isi:int!, n_spikes:int!,
start_time_sec:dbl!, end_time_sec:dbl!, duration_sec:dbl!,
record_status:chr!, lineage_id:chr!
```

`event_class=burst_event`；`extent_modifier=classic|long|prolonged|unresolved`；`frequency_modifier=high|non_high|unresolved`；两个 modifier evidence ID 分别 FK 到相应 domain 的 `event_modifier_evidence` 唯一行。`PK=event_id`；unique `(detection_root_id,train,event_class,start_isi,end_isi)`；sort `(train,start_isi,end_isi,event_id)`。同一轨 accepted Events 必须 ISI-disjoint且 maximal；重叠/嵌套 candidate 按签名 `event_maximal_disjoint_selector_v1` expected-set selector 合并为一个基类 Event，modifier 冲突只使相应 modifier unresolved，不复制 Event。相邻但不共享 ISI 的 Events 不自动合并，除非基类 Event selector evidence 本身证明它们属于同一连续 candidate。

### 6.6 `gaps`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
gap_id:chr!, gap_candidate_id:chr!, gap_class:chr!, gap_semantics:chr!,
start_isi:int!, end_isi:int!, n_isi:int!, n_spikes:int!,
start_time_sec:dbl!, end_time_sec:dbl!, duration_sec:dbl!,
gap_evidence_status:chr!, absolute_threshold_sec:dbl!,
local_reference_sec:dbl!, local_ratio:dbl!, surprise_score:dbl!,
raw_p_value:dbl!, adjusted_q_value:dbl!, valid_local_n_isi:int!,
multiple_testing_method_registry_id:chr!,
qc_eligibility:chr!, gap_policy_registry_id:chr!, gap_evidence_id:chr!,
record_status:chr!
```

`gap_class=canonical_pause`；`gap_semantics=predicted_statistical_pause`；`gap_evidence_status=pass`；v1固定 `start_isi=end_isi,n_isi=1,n_spikes=2`。`raw_p_value` 与 `adjusted_q_value` 均在 `[0,1]`，且逐字等于对应 accepted `gap_candidates` 行。`surprise_score=-log10(max(adjusted_q_value,2.2250738585072014e-308))`；该 binary64 常量与 log10实现版本进入 estimator registry semantic bytes；`PK=gap_id`；sort `(train,start_isi,end_isi,gap_id)`。accepted gaps ISI-disjoint，并与 accepted State episode、direct support和Event均不重叠；因此 `per_isi.gap_id/event_id` 均可保持单值。

### 6.7 `boundary_evidence`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
boundary_id:chr!, boundary_domain:chr!, boundary_class_registry_id:chr!,
boundary_geometry:chr!, edge_side:chr!, reason_registry_id:chr!,
start_isi:int?, end_isi:int?, n_isi:int!,
start_time_sec:dbl?, end_time_sec:dbl?, hard_for_state:lgl!,
hard_for_event:lgl!, hard_for_episode_link:lgl!, source_gap_id:chr?,
qc_status_registry_id:chr!, boundary_priority:int!, evidence_id:chr!,
boundary_rule_registry_id:chr!
```

`boundary_domain=predicted_pause|qc|algorithmic|acquisition_boundary`；`boundary_geometry=isi_span|recording_edge`；`edge_side=none|left|right|both`。ISI span 必须有合法 start/end、`n_isi=end-start+1`、edge side none；recording edge 必须 `n_isi=0`、start/end ISI 为 NA、edge side 非 none，若 parent 无 spike则时间也为 NA。`PK=boundary_id`；FK `source_gap_id -> gaps.gap_id` only when domain is predicted_pause；sort时 nullable ISI 以 `NA` 排在整数之前，再按 `(train,start_isi,end_isi,boundary_domain,boundary_id)`。

同一 ISI 可有多条 source boundary evidence，但 `per_isi.controlling_boundary_id` 唯一。`boundary_priority` 的 domain base 固定为 `qc=400 > acquisition_boundary=300 > predicted_pause=200 > algorithmic=100`；同 domain 再取签名 boundary-rule registry 中的整数 `rule_priority`，最后以 boundary ID byte order 打破完全平局。控制边界选择是 `(domain base DESC, rule_priority DESC, boundary_id ASC)` 的全函数。若 QC/acquisition 与 Pause candidate 重叠，该 candidate 不得进入 accepted `gaps`，只能 rejected/abstain；因此 accepted predicted Pause 不会被更高优先级边界遮蔽。recording-edge 不投影到 `per_isi`，但仍参与 episode/link 的端点闭包。

### 6.8 `state_abstentions`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
abstention_id:chr!, state_candidate_id:chr!, start_isi:int!, end_isi:int!,
state_frequency_class:chr!, state_regularity_class:chr!,
frequency_evidence_status:chr!, regularity_evidence_status:chr!,
reason_registry_id:chr!, state_axis_evidence_id:chr!
```

`PK=abstention_id`；unique `state_candidate_id`；FK to `state_candidates`；sort `(train,start_isi,end_isi,abstention_id)`。

### 6.8a `state_axis_evidence`

```text
schema_version:chr!, detection_root_id:chr!, state_axis_evidence_id:chr!,
train:chr!, state_candidate_id:chr!, start_isi:int!, end_isi:int!,
n_valid_isi:int!, effective_duration_sec:dbl!,
frequency_estimator_registry_id:chr!, frequency_stat:dbl?,
frequency_pass_bound:dbl!, frequency_gray_margin:dbl!,
frequency_evaluable:lgl!, frequency_evidence_status:chr!,
regularity_estimator_registry_id:chr!, regularity_stat:dbl?,
regularity_pass_bound:dbl!, regularity_gray_margin:dbl!,
regularity_evaluable:lgl!, regularity_evidence_status:chr!,
episode_aggregation_registry_id:chr!, insufficient_reason_registry_id:chr?,
evidence_payload_hash:chr!
```

`evidence_payload_hash=H("stpd-state-axis-evidence-payload-v1\0"||canonical row excluding schema_version/detection_root_id/state_axis_evidence_id/state_candidate_id/evidence_payload_hash)`。`PK=state_axis_evidence_id`；unique `(state_candidate_id,start_isi,end_isi)`；FK to `state_candidates`；sort `(train,start_isi,end_isi,state_candidate_id)`。accepted direct-support segment 必须恰有一行且两轴 pass；abstention 必须引用对应非 pass/gray/insufficient 行。非有限 statistic 只能用 typed NA 且 evaluable=FALSE。

### 6.8b `gap_candidates` 与 `gap_abstentions`

`gap_candidates` 保存全部被扫描的候选，不只 winner：

```text
schema_version:chr!, detection_root_id:chr!, gap_candidate_id:chr!,
train:chr!, start_isi:int!, end_isi:int!, n_isi:int!,
start_time_sec:dbl!, end_time_sec:dbl!, duration_sec:dbl!,
reference_window_start_isi:int?, reference_window_end_isi:int?,
guard_band_n_isi:int!, local_estimator_registry_id:chr!,
valid_local_n_isi:int!, local_reference_sec:dbl?, local_ratio:dbl?,
raw_p_value:dbl?, adjusted_q_value:dbl?, multiple_testing_family_id:chr!,
multiple_testing_method_registry_id:chr!, boundary_censored:lgl!,
scientific_context_epoch_id:chr?, qc_evidence_id:chr!,
decision:chr!, decision_reason_registry_id:chr!,
gap_policy_registry_id:chr!, evidence_id:chr!
```

`decision=accepted_pause|rejected|abstain`；PK `gap_candidate_id`；unique `(detection_root_id,train,start_isi,end_isi,gap_policy_registry_id)`；sort `(train,start_isi,end_isi,gap_candidate_id)`。`raw_p_value/adjusted_q_value` 非 NA 时必须位于 `[0,1]`；边界删失、局部样本不足或 QC ineligible 时二者为 typed NA并只能 abstain。AUTO 中每个 accepted row 必须一对一生成 `gaps`；FINAL 中唯一例外是存在 exact `gap_projection_reject` transition。每个 abstain row 必须一对一生成 `gap_abstentions`；rejected rows保留在 candidate universe。candidate universe 必须由第 4.1 节冻结扫描算法从 parent timestamps重建，删除 negative candidate 后即使重封也会 expected-set fail。

`gap_abstentions`：

```text
schema_version:chr!, detection_root_id:chr!, gap_abstention_id:chr!,
gap_candidate_id:chr!, train:chr!, start_isi:int!, end_isi:int!,
reason_registry_id:chr!, evidence_id:chr!
```

PK `gap_abstention_id`；unique `gap_candidate_id`；sort `(train,start_isi,end_isi,gap_abstention_id)`。
`gap_candidate_id` 必须 FK 到 `gap_candidates` 中 `decision=abstain` 的同 train/geometry 行；每个 abstain candidate 恰有一行、accepted/rejected candidate 不得有行。`scientific_context_epoch_id` 非 NA 时必须 FK 到同 train、覆盖该 candidate 且 hard-context policy允许的 `scientific_context_epochs.context_epoch_id`。

### 6.8c `state_connector_decisions`

```text
schema_version:chr!, detection_root_id:chr!, connector_decision_id:chr!,
train:chr!, pre_direct_candidate_id:chr!, post_direct_candidate_id:chr!,
gap_start_isi:int!, gap_end_isi:int!, decision:chr!,
decision_reason_registry_id:chr!, frequency_equivalence_status:chr!,
regularity_equivalence_status:chr!, equivalence_method_registry_id:chr!,
frequency_margin:dbl!, regularity_margin:dbl!, pre_n_valid_isi:int!,
post_n_valid_isi:int!, pre_frequency_stat:dbl!, post_frequency_stat:dbl!,
pre_regularity_stat:dbl!, post_regularity_stat:dbl!,
connector_duration_sec:dbl!, connector_fraction_of_proposed_episode:dbl!,
proposed_episode_span_sec:dbl!, connector_count_if_accepted:int!,
hard_boundary_clear:lgl!, recursion_generation:int!, evidence_id:chr!
```

`decision=connect|do_not_connect|abstain`；`recursion_generation` 必须为 0；PK `connector_decision_id`；`pre/post_direct_candidate_id` FK 到 episode-independent、`candidate_decision=accepted_direct_support` 的 `state_candidates`，两者各有对应 axis evidence且同 train/class；unique `(pre_direct_candidate_id,gap_start_isi,gap_end_isi,post_direct_candidate_id)`；sort `(train,gap_start_isi,gap_end_isi,pre_direct_candidate_id,post_direct_candidate_id,connector_decision_id)`。所有相邻、同 class、具连接资格的 direct-support candidate pair 必须恰有一行，包括 negative 和 abstain；accepted connector segment 必须一对一引用 `connect` row。decision ID 不得依赖 episode或segment ID，从而先于 episode materialization确定。

`proposed_episode_span_sec=post.end_time_sec-pre.start_time_sec`；`connector_fraction_of_proposed_episode=connector_duration_sec/proposed_episode_span_sec`；分母严格大于 0。connector duration 是 gap ISI duration 之和。
connector `evidence_id` 的规范 payload只能引用 pre/post candidate IDs、对应 axis-evidence IDs、gap geometry、boundary evidence与 threshold instances；不得引用尚未生成的 episode/segment/relationship ID。schema-bundle hash-DAG lint必须显式证明 `candidate/axis -> connector decision -> episode -> segment` 无回边。

### 6.8d `event_modifier_evidence`

```text
schema_version:chr!, detection_root_id:chr!, modifier_evidence_id:chr!,
event_id:chr!, modifier_domain:chr!, modifier_value:chr!,
evidence_status:chr!, estimator_registry_id:chr!, statistic_value:dbl?,
pass_bound:dbl?, gray_margin:dbl?, threshold_instance_id:chr?, evidence_id:chr!
```

`modifier_domain=extent|frequency`；每个 Event 必须恰有两行，且 `events` 中的对应 ID/status/value 与该行逐字一致；PK `modifier_evidence_id`；unique `(event_id,modifier_domain)`；sort `(event_id,modifier_domain)`。一轴 unresolved 不影响基类 Burst 或另一轴；其 statistic/threshold 可以 typed NA，但必须以 `insufficient|gray_zone` evidence status与 registry reason说明。

阶段 1 中数据充分时上述两个 estimator 必须从 canonical Event geometry 重算，不能以自报 statistic代替。extent 的 `gray_margin=0`；frequency 的 `gray_margin=high_enter-high_exit`。`pass_bound` 对 prolonged 写 prolonged-min、对 classic/long 写 long-min；frequency 对 non_high 写 high-exit、对 high/gray 写 high-enter。`threshold_instance_id` 必须是决定该显示值/gray状态的对应 bound，同时该 consumer 的 exact binding set仍须含该轴的全部两条路径。

### 6.9 `review_candidates`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
review_candidate_id:chr!, candidate_domain:chr!, suggested_class_registry_id:chr!,
start_isi:int!, end_isi:int!, n_isi:int!, start_time_sec:dbl!,
end_time_sec:dbl!, candidate_status:chr!, reason_registry_id:chr!,
evidence_id:chr!, lineage_id:chr!
```

`candidate_domain=event|state|gap|episode_link`；`candidate_status=pending_review|review_confirmed|review_rejected`；`suggested_class_registry_id` FK to `prediction_class` registry，且其 code必须属于 candidate-domain允许集合；`PK=review_candidate_id`；sort `(train,start_isi,end_isi,review_candidate_id)`。event-domain row 必须至少有一条 entity-evidence edge指向 `event_candidates` 中 `candidate_decision=review_candidate` 的同几何 source；其他 domain也必须按 registry 的 evidence-role cardinality绑定其规范 source，不得由 UI 自由创建 AUTO candidate。
AUTO 只允许 `pending_review`；FINAL 的 confirm/reject只能由一条有效 transition投影。v3.1 中 event-domain candidate可经 `event_candidate_accept` 变为`review_confirmed`；state/gap candidate只能保持 pending或被reject。`state_episodes/events/gaps` 的 `record_status`：AUTO 只允许 `predicted`，FINAL 中未触及行仍为 predicted；reviewed Event可新增`review_confirmed` projection；`state_split`产生的两个child State固定为`review_confirmed`但其active support/class必须是parent detector support的严格partition且不得扩张；Gap不产生review-confirmed projection。State/Gap其他动作仅允许删除已有投影，不能把不通过 detector门控的候选伪装成 direct support/predicted Pause。

### 6.10 `event_state_relationships`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
relationship_id:chr!, event_id:chr!, state_episode_id:chr!,
relation_type:chr!, episode_overlap_n_isi:int!,
direct_support_overlap_n_isi:int!, connector_overlap_n_isi:int!,
episode_overlap_sec:dbl!, direct_support_overlap_sec:dbl!,
connector_overlap_sec:dbl!, event_covered_by_episode_time_fraction:dbl!,
event_covered_by_direct_support_time_fraction:dbl!,
state_envelope_covered_by_event_time_fraction:dbl!,
state_direct_support_covered_by_event_time_fraction:dbl!,
event_covered_by_episode_isi_fraction:dbl!,
event_covered_by_direct_support_isi_fraction:dbl!,
state_envelope_covered_by_event_isi_fraction:dbl!,
state_direct_support_covered_by_event_isi_fraction:dbl!
```

`relation_type=event_overlaps_state_episode`；`PK=relationship_id`；unique `(event_id,state_episode_id)`；FK to events and state_episodes；sort `(train,event_id,state_episode_id)`。validator 必须从 geometry 重建完整 expected pair set，并逐字段相等；只验证已有行不合格。`*_overlap_sec` 是所有相交 ISI duration 的和，不是外包络端点差。四个 time fraction 分别除以 event duration、event duration、episode envelope duration、direct-support duration；四个 ISI fraction 使用对应 n_isi 分母。accepted Event/episode/direct support 分母均严格大于 0。

### 6.10a `event_interrupted_state_relationships`

```text
schema_version:chr!, detection_root_id:chr!, interruption_relationship_id:chr!,
train:chr!, event_id:chr!, parent_state_candidate_id:chr!,
parent_state_axis_evidence_id:chr!, child_state_episode_ids_json:json!,
redetection_outcome:chr!, left_child_status:chr!, right_child_status:chr!,
lineage_closure_hash:chr!, evidence_id:chr!
```

`redetection_outcome=both_accepted|left_only|right_only|both_rejected|reclassified|abstained`；`left_child_status/right_child_status=accepted|rejected|abstained|empty`。PK `interruption_relationship_id`；unique `(event_id,parent_state_candidate_id)`；child ID array 使用 byte-order排序、成员唯一、同 train，并逐项 FK，只包含 accepted child episode；空数组固定为 `[]`。只用于 HFT/tonic 操作性切分；HF-irregular–Burst 不产生本表行。expected-set validator 必须由父 candidate、Event 与重检 lineage 重建全集。
`parent_state_axis_evidence_id` FK to `state_axis_evidence.state_axis_evidence_id`，且必须等于 parent candidate 的同名axis evidence；`evidence_id` 单独 FK to `evidence_records`。`lineage_closure_hash=H("stpd-interrupted-state-lineage-closure-v1\0"||canonical{event_id,parent_state_candidate_id,parent_state_axis_evidence_id,redetection_outcome,left_child_status,right_child_status,sorted_child_state_episode_ids,sorted_child_lineage_rows})`；其中child lineage rows按`(child_domain_id,child_id,edge_index)`排序并排除任何`interruption_relationship_id/lineage_closure_hash/evidence_id`回指，逐项指回同一parent candidate、axis evidence与Event。该hash在relationship ID之前生成；删除、增加、换序或跨Event child均改变closure并expected-set失败。

### 6.11 `state_episode_links`

```text
schema_version:chr!, detection_root_id:chr!, source_context_hash:chr!, train:chr!,
episode_link_id:chr!, relation_type:chr!, pre_state_episode_id:chr!,
gap_id:chr!, post_state_episode_id:chr!, link_status:chr!,
pre_frequency_stat:dbl!, post_frequency_stat:dbl!,
frequency_equivalence_margin:dbl!, pre_regularity_stat:dbl!,
post_regularity_stat:dbl!, regularity_equivalence_margin:dbl!,
equivalence_status:chr!, gap_fraction:dbl!, episode_span_sec:dbl!,
qc_boundary_clear:lgl!, condition_boundary_clear:lgl!,
link_policy_registry_id:chr!, link_evidence_id:chr!, biological_ground_truth:lgl!
```

`relation_type=pause_interrupted_hf`；`link_status=algorithmic_candidate|review_confirmed|review_rejected`。AUTO 只允许 `algorithmic_candidate`。FINAL 未触及行逐字保留AUTO candidate；confirm/reject只能从 algorithmic candidate 状态发起（改变意见必须先追加 `compensate`），并由exact transition/history改变同一candidate relationship的review status。`episode_link_id`有意不含`link_status`：它标识不可变几何、policy与algorithmic evidence，status-only adjudication不创造第二个关系实体；FINAL row/product/history hash仍会改变，validator必须证明恰有一个有效transition投影该status。`PK=episode_link_id`；unique `(pre_state_episode_id,gap_id,post_state_episode_id)`；三 FK 必须同 train且按时间严格相邻；前后 State 必须都为 `high_frequency_irregular_state`；sort `(train,pre_state_episode_id,gap_id,post_state_episode_id)`。自动图只含相邻三元组，禁止传递派生。`episode_span_sec=post.end_time_sec-pre.start_time_sec`；`gap_fraction=gap.duration_sec/episode_span_sec`，分母严格大于 0。`equivalence_status=equivalent` 仅表示冻结统计规则通过，不表示两段生物学上属于同一机制；因此 AUTO 行仍是 candidate。
`link_evidence_id` FK to `evidence_records`，其 schema必须完整含两个 axis statistics/margins、Gap evidence、QC/condition-boundary checks与 generation-before `source_threshold_instance_set_hash`；该 set hash 只能由实际使用的 sorted unique `threshold_instance_id` 计算，明确排除以尚未生成的 `episode_link_id` 为 consumer 的 binding ID/row。`episode_link_id` 确定后才派生对应 `threshold_instance_bindings`，validator 再证明 binding 中的 threshold-instance set 与 evidence 中的 source set完全一致。`link_policy_registry_id` 必须是与该 schema兼容的 operational/equivalence registry entry；schema-bundle hash-DAG lint必须拒绝 `link -> evidence -> self-binding -> link` 回边。

### 6.12 `entity_evidence_edges`

```text
schema_version:chr!, detection_root_id:chr!, entity_domain_id:chr!, entity_id:chr!,
entity_product_hash:chr?, edge_index:int!, evidence_role_registry_id:chr!,
source_domain_id:chr!, source_row_key_json:json!, source_product_hash:chr?,
source_row_hash:chr!, evidence_id:chr!
```

`PK=(entity_domain_id,entity_id,edge_index)`；unique `(entity_domain_id,entity_id,evidence_role_registry_id,source_domain_id,source_row_key_json,source_product_hash)`；sort by PK。domain→table/PK/train解析只来自 `entity_domain_registry`；禁止把 provenance ID set 压成不可还原的字符串。target entity 总是当前 materialized product 的行，因此 `entity_product_hash` 必须为 typed NA。source row 属于当前 product 时 `source_product_hash` 必须为 typed NA；只有 source-domain registry 允许 cross-product、且该 row 来自已完整验证的严格祖先 product 时才允许非 NA，并必须逐字等于该祖先 `product_hash`。两列均不得等于当前正在计算的 `product_hash`，schema/hash-DAG lint必须拒绝 `canonical table -> current product hash -> canonical table` 回边。

`source_row_hash` 使用唯一版本化 envelope；不得复用其他表的 row hash，也不得只哈希展示字符串：

```formula
source_row_hash_schema = stpd_multitrack_v3_entity_evidence_source_row_hash_1

canonical_source_row_sha256 =
  SHA256(canonical typed-table bytes of exactly the resolved one-row source table)

source_row_hash =
  H("stpd-entity-evidence-source-row-v1\0" || canonical{
    source_row_hash_schema,
    source_domain_id,
    source_table_name,
    primary_key_columns,
    source_row_key,
    source_binding,
    source_product_hash,
    canonical_source_row_sha256
  })
```

其中 `source_table_name` 与 `primary_key_columns` 逐字来自 active `entity_domain_registry`；`source_row_key` 是 `source_row_key_json` 解码后的 canonical JCS object，key set 必须逐字等于该 source table 的完整 PK，value 必须按对应 column type/nullability 唯一解析到一行。`canonical typed-table bytes` 使用该表冻结的完整列顺序/type/nullability/enum和一行真实值，不排除任何列；因此改变非 PK source payload 也必须改变 `source_row_hash`。`source_binding=current_product|strict_ancestor_product`：current 时 `source_product_hash=null`，且 source row 必须存在于当前 canonical inventory；ancestor 时 `source_product_hash` 必须为已完整验证的严格祖先 product hash，并从该祖先 inventory 解析 source row。current binding 不读取当前 `product_hash`，避免 `source row -> edge table -> product hash -> source row` 回边；其归属由当前 inventory、canonical manifest与最终 product hash闭合。

同一 `(entity_domain_id,entity_id)` 的 edge expected set 由六元组 `{evidence_role_registry_id,source_domain_id,source_row_key_json,source_product_hash,source_row_hash,evidence_id}` 的 canonical JCS bytes 按 UTF-8 byte order 排序；成员必须唯一，排序后 `edge_index` 必须严格为 `1..n`。删除、增加、换序、换 source row 或协调重封 hash 都必须失败。该 v1 公式是阶段 1 对原第 6.12 节缺失 source-row identity 语义的最小规范修订；任何未来 payload、domain或祖先绑定变化必须使用新 schema/domain 版本，不能原地重解释本公式。

### 6.13 Threshold policy、partition 与实际实例

`threshold_policies`：

```text
schema_version:chr!, detection_root_id:chr!, threshold_policy_id:chr!,
path:chr!, units:chr!, threshold_source_class:chr!, adaptation_unit:chr!,
label_access:chr!, threshold_algorithm_registry_id:chr!,
policy_parameter_hash:chr!, requested_value_hash:chr!,
frozen_before_validation:lgl!, partition_id:chr?
```

`PK=threshold_policy_id`；unique `(path,threshold_source_class,adaptation_unit,policy_parameter_hash,partition_id)`；sort `(path,threshold_policy_id)`。`fixed_preregistered` 的 partition 必须 NA；`development_trained` 必须引用非空、group-disjoint partition。

`partition_memberships`：

```text
schema_version:chr!, partition_id:chr!, partition_version:chr!,
group_id_hash:chr!, group_role:chr!, patient_group_id_hash:chr?,
session_group_id_hash:chr!, source_input_hash:chr!, membership_row_hash:chr!
```

`group_role=development|calibration|validation|excluded`；PK `(partition_id,group_id_hash)`；每个 group在一个partition恰有一个 role，任何跨role重复均拒绝；sort by PK。validator 从逐行 membership 重算训练组数量与 overlap，任何自报 count 都不是信任输入。
`membership_row_hash=H("stpd-partition-membership-row-v1\0"||canonical row excluding schema_version/partition_id/membership_row_hash)`；它是 root-free。`partition_id` 再由同一 partition 的 sorted rows（排除 partition_id、保留并复核 membership-row hash）计算，因此两者无自引用。

每条 membership 必须唯一解析到当前 `normalized_input_manifest` 的一行：`source_input_hash` 必须逐字等于该行的 `normalized_timestamp_bytes_sha256`，`patient_group_id_hash` 与 `session_group_id_hash` 也必须分别逐字相等（包括 patient 的 typed NA）。同一 hash 匹配零行或多行均 fail closed。held-out 隔离同时按 patient 与 session 重建：任何 `development|calibration` row 不得与 `validation` row 共享非 NA `patient_group_id_hash`，也不得共享 `session_group_id_hash`；patient 缺失绝不降级为“无法检查”，仍必须依 session 做排他隔离。`group_id_hash`、自报 group count 或可变 display label 不能替代上述逐输入绑定。

`threshold_instances`：

```text
schema_version:chr!, detection_root_id:chr!, threshold_instance_id:chr!,
threshold_policy_id:chr!, path:chr!, application_scope:chr!,
source_scope_key_hash:chr!, application_scope_key_hash:chr!,
train:chr?, session_group_id_hash:chr?,
dataset_batch_id_hash:chr?, derivation_input_manifest_hash:chr!,
application_input_manifest_hash:chr!,
value_type:chr!, value_num:dbl?, value_chr:chr?, units:chr!,
effective_value_hash:chr!, instance_evidence_id:chr!
```

`application_scope=global|train|session|dataset_batch`。`threshold_policy_id` 必须 FK 到当前 `threshold_policies`，且 instance 的 `path/units` 必须与该 policy 逐字相等；一个 instance 不得跨 policy 复用。恰有一个 value 非 NA，且 scope 对应的 key 列恰有一个有效组合。先按第8节公式生成该scope的root-free `application_input_manifest_hash`，再生成key：global `H("stpd-global-source-scope-v1\0"||canonical{application_input_manifest_hash})`；train `H("stpd-train-source-scope-v1\0"||canonical{application_input_manifest_hash,train})`；session `H("stpd-session-source-scope-v1\0"||canonical{application_input_manifest_hash,session_group_id_hash})`；dataset batch `H("stpd-dataset-batch-source-scope-v1\0"||canonical{application_input_manifest_hash,dataset_batch_id_hash})`。global 时三个 scope-key列均 NA；train/session/dataset_batch时只允许各自对应的一个 key列非 NA。材料化后统一计算 `application_scope_key_hash=H("stpd-materialized-threshold-scope-v1\0"||canonical{detection_root_id,application_scope,source_scope_key_hash})`。PK `threshold_instance_id`；unique `(threshold_policy_id,application_scope,application_scope_key_hash)`；sort `(path,application_scope,application_scope_key_hash,threshold_instance_id)`。

`threshold_instance_bindings`：

```text
schema_version:chr!, detection_root_id:chr!, binding_id:chr!,
consumer_domain_id:chr!, consumer_id:chr!, path:chr!,
threshold_instance_id:chr!, binding_role_registry_id:chr!
```

PK `binding_id`；`consumer_domain_id` FK to entity-domain registry，`binding_role_registry_id` FK to `binding_role` registry domain；unique `(consumer_domain_id,consumer_id,path,binding_role_registry_id)`；sort by该 unique key。每个 accepted/candidate/abstained object 所使用的每个 threshold 都必须显式绑定实例；不得仅从 path 猜 scope。
为避免内容地址环，某实体的 evidence/entity ID可以包含实际使用的 `threshold_instance_id` set，但不得包含以该实体自身 ID作为consumer后才生成的 binding ID；binding row总是在consumer ID确定后派生。schema-bundle hash-DAG lint必须拒绝 evidence→self-binding→entity 的回边。

合法组合与读取权限矩阵（逐行 exact；未列组合非法）：

| source | adaptation | application scope | label access | partition | derivation rows | application rows | 默认 new-patient Gate C |
|---|---|---|---|---|---|---|---|
| fixed_preregistered | none | global | none | NA | canonical empty manifest | 当前产品全部 normalized-input rows；不得读标签 | 允许 |
| development_trained | none | global | development_reference_only | nonempty | exact development/calibration members；validation/excluded overlap=0 | 当前产品全部 normalized-input rows；不得读 application-group 标签 | 允许 |
| development_trained | session | session | development_reference_only | nonempty | exact development/calibration members；validation/excluded overlap=0 | exact application session rows；不得读该 session 标签 | 允许，仅预注册可迁移 session policy |
| per_train_unsupervised | train | train | none | NA | exact application train 的 normalized-input row | 同一 train 的同一 row | 允许；只可读本 train |
| batch_transductive | dataset_batch | dataset_batch | none | NA | exact application batch 的 normalized-input rows | 同一 batch rows | 仅独立 transductive estimand |

`threshold_source_class/adaptation_unit/label_access/partition_id` 来自 FK policy，不在 instance 中另行覆盖；validator按 `threshold_policy_id` 联表验证本矩阵。source manifest 的 policy row 与 materialized policy 必须逐字段等价，instance source row 与 materialized instance 必须逐字段等价。其他组合一律 fail closed。改变一个 held-out group 不得改变另一个 held-out group 的默认-estimand threshold instance。

### 6.14 `diagnostic_records`

```text
schema_version:chr!, detection_root_id:chr!, diagnostic_id:chr!, severity:chr!,
diagnostic_code_registry_id:chr!, entity_domain_id:chr!, entity_id:chr?, train:chr?,
start_isi:int?, end_isi:int?, detail_schema_registry_id:chr!, detail_json:json!,
detail_json_hash:chr!
```

`severity=info|warning|error`；`detail_json` 必须满足 registry schema，不得以自由文本承担可执行语义；`detail_json_hash=H("stpd-diagnostic-detail-json-v1\0"||canonical{detail_schema_registry_id,detail_json})`；`PK=diagnostic_id`；sort `(severity,diagnostic_code_registry_id,entity_domain_id,entity_id,diagnostic_id)`。

### 6.15 `adjudication_transitions`（FINAL only）

```text
schema_version:chr!, detection_root_id:chr!, transition_id:chr!, sequence_no:int!,
parent_auto_product_hash:chr!, expected_parent_final_hash:chr!,
previous_transition_hash:chr?, action_type:chr!, target_domain_id:chr!,
target_id:chr!, payload_schema_registry_id:chr!, payload_json:json!,
payload_json_hash:chr!,
actor_pseudonym:chr!, reference_record_domain:chr?, reference_record_id:chr?,
reference_manifest_hash:chr?, created_utc:chr!,
compensates_transition_id:chr?, transition_hash:chr!
```

`action_type=event_candidate_accept|review_candidate_reject|event_projection_reject|state_projection_reject|state_split|gap_projection_reject|episode_link_confirm|episode_link_reject|compensate`。`PK=transition_id`；unique `(detection_root_id,sequence_no)`；sort `sequence_no`。所有变更使用 compare-and-swap；撤销只能追加 `compensate`，不能删除历史。action-specific payload schema 必须独立版本化并验证。

`payload_json_hash=H("stpd-transition-payload-json-v1\0"||canonical{payload_schema_registry_id,payload_json})`，必须在 transition ID 与 row hash 生成前重算；unknown schema、额外 key或hash不符均拒绝。

`created_utc` 固定 RFC 3339 UTC、六位小数、尾缀 `Z`（例 `2026-08-23T12:34:56.123456Z`）；actor pseudonym 为 `actor_` + 64 hex，不含直接身份。首条 `previous_transition_hash=SHA256("stpd-history-genesis-v1\0"||parent_auto_product_hash)`；后续等于上一行 `transition_hash`。`transition_hash=SHA256("stpd-transition-v1\0"||canonical row excluding transition_id/transition_hash)`。时间属于审计历史，因此相同科学动作在不同时间产生不同 history/product hash，但不改变未修改 entity 的 ID/scientific row bytes。

FINAL 的 `history_head_hash=SHA256("stpd-history-head-v1\0"||canonical{parent_auto_product_hash,transition_n,last_transition_hash})`。零 transition 时固定 `transition_n=0,last_transition_hash=null`；非零时 `sequence_no` 必须严格为 `1..transition_n`，`last_transition_hash` 必须逐字等于最后一行 `transition_hash`。AUTO 的 `history_head_hash` 必须为 typed NA。该公式只读取已完成的 ordered transition chain，不读取 FINAL `product_hash`，因此不形成回边。

`reference_record_domain=annotation|adjudication`。不绑定独立 reference 时 domain/id/manifest三者必须全 NA；绑定时三者全非 NA，ID须按6.20公式在 exact `reference_manifest_hash`中逐项验证。该可选绑定只证明 review依据，不会把 FINAL 变为 biological truth或 Gate C performance reference。

首次 CAS 的 `expected_parent_final_hash=SHA256("stpd-initial-final-v1\0"||parent_auto_product_hash)`；后续必须等于上一次 transition 后 materialized FINAL product hash。payload JSON schema 固定：

| action | required payload keys |
|---|---|
| event_candidate_accept | review_candidate_id, expected_review_candidate_hash, source_event_candidate_id, expected_source_candidate_hash, suggested_class_registry_id, reason_registry_id |
| review_candidate_reject | review_candidate_id, expected_review_candidate_hash, reason_registry_id |
| event_projection_reject | event_id, expected_event_hash, reason_registry_id |
| state_projection_reject | state_episode_id, expected_episode_hash, reason_registry_id |
| gap_projection_reject | gap_id, expected_gap_hash, reason_registry_id |
| state_split | state_episode_id, expected_episode_hash, connector_segment_id, expected_connector_segment_hash, reason_registry_id |
| episode_link confirm/reject | episode_link_id, expected_link_hash, reason_registry_id |
| compensate | compensates_transition_id, expected_effect_hash, reason_registry_id |

未知 key、缺 key、未被 expected-set支持的 target均拒绝。`event_candidate_accept` 的 target domain必须是 event-domain `review_candidate`，且其 immutable source恰为 `event_candidates.candidate_decision=review_candidate`；它只可接受与当前FINAL全部 Event和accepted Gap均ISI-disjoint、几何内无controlling QC/hard boundary、且不与 HFT/tonic State support重叠的 Event（与 HF-irregular 重叠允许且不改 State），并生成 `record_status=review_confirmed` Event及必需 transition/evidence/lineage edges。需要合并/替换既有Event、移除Gap或跨不可接受boundary的review candidate在v3.1 typed拒绝。`event_projection_reject` 不得移除具有 `event_interrupted_state_relationships` 的 Event，也不得移除其几何与 HFT/tonic State重叠、因而需要恢复 counterfactual parent State的 Event；v3.1 不预计算这类反事实 State projection。三个 projection-reject只可移除其余现有 accepted projection。

v3.1 的 `state_split` 必须精确选择target episode内部一个非首尾的 `tolerated_connector` segment；该 connector两侧都必须至少有一个完整 direct-support segment。回放删除该 connector的episode membership，将其ISI投影为无State/无episode的unclassified interval，并把connector左、右两侧完整既有segments分别重建为恰好两个同class、`record_status=review_confirmed`的child episodes；所有child segment因新episode FK生成新segment ID并以lineage指回原segment/source candidate/axis evidence。不得裁剪任何segment、在direct-support内部切分、产生新 detector candidate/axis evidence或改变active support/class；immutable connector-decision row保留并由review lineage/diagnostic记录其projection被拒绝。`compensate`只能恢复该transition前的exact parent episode与connector projection；多处拆分必须逐次独立transition，不允许数组一次产生任意多个children。自动 Event打断后的完整重检发生在 AUTO immutable candidate tables建立之前，与该人工 split不同。v3.1 不允许人工接受 abstained/rejected State或Gap，也不允许人工 State merge，因为那会需要新的 `review_confirmed_support`/Gap语义和分母；这些操作以及上述需要反事实 State恢复/重检的 Event操作均返回 response status `unsupported_transition_for_schema`，并引用 active `failure_code/unsupported_transition_for_schema_v1` registry entry，不能伪装成 direct support/predicted Pause。每种 action对应独立 `transition_payload_schema` registry entry，其 semantic definition bytes与本表一致并进入 contract hash。

### 6.16 `contract_registries`

```text
schema_version:chr!, registry_entry_id:chr!, registry_domain:chr!,
code:chr!, code_version:chr!,
semantic_definition_json:json!, semantic_definition_sha256:chr!, active:lgl!
```

`registry_domain=classification_rule|prediction_class|state_candidate_rule|event_candidate_rule|segment_rule|gap_policy|boundary_class|boundary_rule|reason_code|failure_code|diagnostic_code|diagnostic_detail_schema|transition_payload_schema|threshold_algorithm|operational_policy|label_blind_rule|input_allowlist_schema|input_source_domain|binding_role|lineage_action|evidence_role|evidence_type|evidence_schema|estimator|equivalence_method|multiple_testing_method|episode_aggregation|event_selector|qc_status|reference_label|reference_reason|species|brain_region|brain_subregion|acquisition_condition|anesthesia_status|medication_status|task_status|timebase_provenance|spike_sorting_method|sorting_qc_status|isolation_metric_name|units|nonstationarity_status|context_epoch_status`。`semantic_definition_sha256` 必须由同一行 canonical `semantic_definition_json`重算；JSON 至少含 `{definition_schema,contract_clause_id,input_schema_id,output_schema_id,formula_or_state_machine,parameter_paths,edge_cases}`，不能只含 prose hash。`registry_entry_id="reg_"+H("stpd-registry-entry-v1\0"||canonical{registry_domain,code,code_version,semantic_definition_sha256})`；PK `registry_entry_id`；unique `(registry_domain,code,code_version)`；sort `registry_entry_id`。所有 code-like FK 列保存不可歧义的 `registry_entry_id`，validator 同时检查预期 domain；不得只保存 code 或 version。

阶段 1 的 schema bundle 至少必须材料化并复审以下 active entries；实现不能以相同 code 指向不同算法：`state_frequency_rate_v1`（第3.4公式）、`state_regularity_median_cv2_v1`（第3.4公式）、`state_episode_nonrecursive_connector_v1`、`pause_local_empirical_bh_v1`（第4.1公式）、`connector_axis_equivalence_v1`、`event_maximal_disjoint_selector_v1`、`boundary_precedence_v1`、`resource_budget_v1`、failure-code entries `resource_budget_exceeded|unsupported_transition_for_schema_v1`、prediction-class codes `high_frequency_irregular_state|high_frequency_tonic|tonic|burst_event|canonical_pause|pause_interrupted_hf`、本合同列出的每一种 transition payload schema、所有 evidence JSON schema、每个 threshold binding role，以及 `label_blind_contract_manifest` 实际引用的全部 `label_blind_rule/input_allowlist_schema/input_source_domain` rows。缺 entry 或 semantic JSON/hash不符使 schema bundle不可批准。

`entity_domain_registry`：

```text
schema_version:chr!, entity_domain_id:chr!, entity_domain:chr!,
target_table:chr!, primary_key_columns_json:json!, train_column:chr?,
product_scope:chr!, active:lgl!
```

`product_scope=current_product|cross_product_allowed`；PK `entity_domain_id`；unique `entity_domain`。必须覆盖本 inventory 中每种可被 polymorphic edge 引用的 domain。

最低固定 domain map：

| entity_domain | target table | PK | train resolver | scope |
|---|---|---|---|---|
| state_candidate | state_candidates | state_candidate_id | train | current_product |
| state_episode | state_episodes | state_episode_id | train | cross_product_allowed |
| state_segment | state_segments | state_segment_id | train | cross_product_allowed |
| state_axis_evidence | state_axis_evidence | state_axis_evidence_id | FK state_candidate.train | current_product |
| event_candidate | event_candidates | event_candidate_id | train | current_product |
| event | events | event_id | train | cross_product_allowed |
| event_modifier_evidence | event_modifier_evidence | modifier_evidence_id | FK event.train | current_product |
| gap_candidate | gap_candidates | gap_candidate_id | train | current_product |
| gap | gaps | gap_id | train | cross_product_allowed |
| boundary | boundary_evidence | boundary_id | train | current_product |
| state_abstention | state_abstentions | abstention_id | train | current_product |
| gap_abstention | gap_abstentions | gap_abstention_id | train | current_product |
| connector_decision | state_connector_decisions | connector_decision_id | train | current_product |
| review_candidate | review_candidates | review_candidate_id | train | cross_product_allowed |
| event_state_relationship | event_state_relationships | relationship_id | train | current_product |
| interrupted_state_relationship | event_interrupted_state_relationships | interruption_relationship_id | train | current_product |
| episode_link | state_episode_links | episode_link_id | train | cross_product_allowed |
| scientific_context_epoch | scientific_context_epochs | context_epoch_id | train | current_product |
| evidence | evidence_records | evidence_id | resolve through entity_evidence_edges | current_product |
| diagnostic | diagnostic_records | diagnostic_id | nullable train | current_product |
| transition | adjudication_transitions | transition_id | resolve target_domain_id then target.train | cross_product_allowed |

每个 `entity_domain_id` 是该 row canonical hash ID；实现不可新增 domain 而不改变 schema contract和重新审查。

### 6.17 `evidence_records`

```text
schema_version:chr!, detection_root_id:chr!, evidence_id:chr!,
evidence_type_registry_id:chr!, evidence_schema_registry_id:chr!,
evidence_json:json!, evidence_json_hash:chr!, source_bytes_sha256:chr!, label_access:chr!,
created_by_registry_entry_id:chr!
```

`evidence_json_hash=H("stpd-evidence-json-v1\0"||canonical{evidence_schema_registry_id,evidence_json})`。`PK=evidence_id`；type/schema FK 分别检查 `evidence_type/evidence_schema` domain；`created_by_registry_entry_id` 只允许 `classification_rule|state_candidate_rule|event_candidate_rule|segment_rule|gap_policy|boundary_rule|threshold_algorithm|event_selector|equivalence_method|estimator|episode_aggregation` 中与该 evidence schema 明列兼容的 registry domain。sort `evidence_id`。`evidence_json` 只保存冻结 schema 的统计量/理由，不保存不可验证 prose。

### 6.18 `lineage_records`

```text
schema_version:chr!, detection_root_id:chr!, lineage_id:chr!,
child_domain_id:chr!, child_id:chr!, child_product_hash:chr?,
parent_domain_id:chr!, parent_id:chr!, parent_product_hash:chr?,
lineage_action_registry_id:chr!, edge_index:int!, transition_id:chr?,
evidence_id:chr!, parent_edge_set_hash:chr!
```

令一条 generation-before parent edge payload 精确为 `{parent_domain_id,parent_id,parent_product_hash,lineage_action_registry_id,transition_id,evidence_id}`；按该六元组的 canonical byte order 排序、拒绝重复，并从1开始连续赋 `edge_index`。`parent_edge_set_hash=H("stpd-lineage-parent-edge-set-v1\0"||canonical{child_domain_id,child_id,child_product_hash,sorted_parent_edge_payloads})`，不包含 `lineage_id/edge_index/parent_edge_set_hash`，同一 lineage 的每行必须逐字相等。`PK=(lineage_id,edge_index)`；unique `(child_domain_id,child_id,parent_domain_id,parent_id,lineage_action_registry_id)`；sort by PK。current-product edge 的对应 product hash 必须 NA；跨 AUTO→FINAL edge 必须携带 exact parent product hash并由 external product inventory解析。每个 domain ID 引用 `entity_domain_registry`，复合 PK按该 registry的 canonical key schema解析；同 train、无循环和父存在性均须验证。

### 6.19 `scientific_context`

```text
schema_version:chr!, detection_root_id:chr!, train:chr!, patient_group_id_hash:chr?,
session_group_id_hash:chr?, unit_id_hash:chr!, species_registry_id:chr!,
brain_region_registry_id:chr!, brain_subregion_registry_id:chr?,
acquisition_condition_registry_id:chr!, anesthesia_status_registry_id:chr!,
medication_status_registry_id:chr!, task_status_registry_id:chr!,
recording_duration_sec:dbl!, timestamp_resolution_sec:dbl!,
timebase_provenance_registry_id:chr!, waveform_qc_available:lgl!,
raw_waveform_available:lgl!, spike_sorting_method_registry_id:chr!,
spike_sorting_version:chr!, sorting_qc_status_registry_id:chr!,
isolation_metric_name_registry_id:chr?, isolation_metric_value:dbl?,
isolation_metric_units_registry_id:chr?, nonstationarity_status_registry_id:chr!,
condition_boundary_available:lgl!, context_completeness:chr!,
source_context_row_hash:chr!
```

`PK=(detection_root_id,train)`；每个 normalized-input train必须恰有一行，即使内容只有 `minimal_timestamp_only`；sort `train`。未知值必须使用注册枚举 `unknown/not_available`，不能用空字符串。上下文缺失不必阻断纯 Gate B 几何产品，但必须由独立 Gate C eligibility 产品记录原因；Gate B 的 `reference_standard_status` 只表示参考数据是否存在，不承载性能资格。

`source_context_row_hash` 必须逐字等于 `normalized_input_manifest.acquisition_context_row_hash`，后者对本行排除 `schema_version/detection_root_id/source_context_row_hash` 的 root-free allowlist payload计算；它不能从已有 product metadata复制。patient/session/unit字段必须是项目受控HMAC pseudonym，不能是可逆文件名或临床ID。

`scientific_context_epochs`：

```text
schema_version:chr!, detection_root_id:chr!, context_epoch_id:chr!,
train:chr!, start_isi:int!, end_isi:int!, epoch_domain:chr!,
epoch_status_registry_id:chr!, hard_boundary:lgl!, source_evidence_id:chr!
```

`epoch_domain=condition|nonstationarity|sorting_qc|acquisition`；PK `context_epoch_id`；unique `(train,epoch_domain,start_isi,end_isi)`；sort `(train,start_isi,end_isi,epoch_domain)`。Pause/State local reference window 不得跨 hard epoch boundary。

### 6.20 独立参考标注表（不属于 AUTO/FINAL 输入）

`reference_annotations`：

```text
reference_schema:chr!, annotation_project_id:chr!, annotation_id:chr!,
annotator_pseudonym:chr!, rater_role:chr!, annotation_round:int!,
annotation_pass:int!, repeat_of_annotation_id:chr?, washout_days:int?,
train:chr!, annotation_domain:chr!, edge_left_annotation_id:chr?,
edge_right_annotation_id:chr?, start_isi:int?, end_isi:int?,
reference_class_registry_id:chr!, decision_status:chr!, confidence_code:chr!,
uncertainty_reason_registry_id:chr?, protocol_version:chr!,
display_protocol_id:chr!, blinded_to_auto:lgl!, blinded_to_final:lgl!,
blinded_to_thresholds:lgl!, blinded_to_other_raters:lgl!,
blinded_to_clinical_group:lgl!, waveform_qc_visible:lgl!,
source_input_hash:chr!, supersedes_annotation_id:chr?, created_utc:chr!
```

`decision_status=accepted|rejected|uncertain|unscorable`；`annotation_domain=qc_interval|state_direct_support|reference_pause|burst_event|connector_edge|pause_interrupted_compound_link`。interval domain 必须有 start/end 且 edge IDs 为 NA；edge/link domain 必须有左右 annotation FK且 geometry从 parents重建。Pass 0 只标 valid/artifact/unit-loss/uncertain，Pass 1 独立标 State direct support/reference Pause/Burst Event，Pass 2 才标 connector edge与 compound link。原始标注 append-only，修订使用 `supersedes_annotation_id`。validation performance reference 的五个 blind flags必须全部满足预注册要求；否则只能用于 workflow agreement。

PK `annotation_id`；unique `(annotation_project_id,annotator_pseudonym,annotation_round,annotation_pass,train,annotation_domain,start_isi,end_isi,edge_left_annotation_id,edge_right_annotation_id)`；sort `(annotation_project_id,annotator_pseudonym,annotation_round,annotation_pass,train,annotation_domain,start_isi,end_isi,annotation_id)`。

`reference_display_protocols`：

```text
reference_schema:chr!, display_protocol_id:chr!, protocol_version:chr!,
visible_channels_json:json!, hidden_channels_json:json!,
context_window_pre_sec:dbl!, context_window_post_sec:dbl!,
time_scale_policy:chr!, waveform_display_policy:chr!,
threshold_overlay_visible:lgl!, algorithm_overlay_visible:lgl!,
protocol_payload_sha256:chr!
```

validation protocol 必须令最后两个 visibility 为 FALSE；visible/hidden channels 使用唯一、排序、注册 code array。

`PK=display_protocol_id`；`protocol_version`全表唯一；sort `(protocol_version,display_protocol_id)`。令`protocol_pre_id_payload=canonical row excluding display_protocol_id/protocol_payload_sha256`，则`protocol_payload_sha256=H("stpd-reference-display-payload-v1\0"||protocol_pre_id_payload)`，`display_protocol_id="rd_"+H("stpd-reference-display-id-v1\0"||protocol_pre_id_payload)`。同一`protocol_version`出现不同payload为fatal contract conflict。

`reference_adjudications`：

```text
reference_schema:chr!, adjudication_id:chr!, annotation_project_id:chr!,
adjudicator_pseudonym:chr!, source_annotation_ids_json:json!,
train:chr!, annotation_domain:chr!, start_isi:int?, end_isi:int?,
adjudicated_class_registry_id:chr!, adjudication_status:chr!,
reason_registry_id:chr!, protocol_version:chr!, created_utc:chr!
```

PK `adjudication_id`；sort `(annotation_project_id,train,annotation_domain,start_isi,end_isi,adjudication_id)`。`source_annotation_ids_json` 是 byte-order sorted unique ID array，至少两位不同 annotator、同 domain/train/geometry且独立阶段完成并冻结后才能裁决；validator逐项 FK。原始分歧、uncertain/unscorable 和 confidence 必须保留。AUTO 构建权限边界禁止读取这些表、display protocol与任何派生缓存。reference label使用独立 `reference_pause`，不得用 detector threshold定义“canonical Pause”。

Reference ID 使用独立完整 SHA-256 域：`annotation_id="ra_"+H("stpd-reference-annotation-v1\0"||canonical row excluding annotation_id/created_utc)`；display protocol按上一段唯一pre-ID payload与两个独立domain生成payload hash/ID；`adjudication_id="rj_"+H("stpd-reference-adjudication-v1\0"||canonical row excluding id/created_utc)`。时间只属于审计 envelope，不改变同一冻结标注的科学 ID。validation reference 行必须 `blinded_to_auto/final/thresholds/other_raters/clinical_group=TRUE`、绑定已批准 display protocol且 `algorithm_overlay_visible=threshold_overlay_visible=FALSE`；不满足者只能标记为 algorithm-assisted workflow agreement，不能进入 Gate C performance reference。`annotation_pass=0` 只能用 QC domain，pass 1只能用 State/Pause/Burst，pass 2只能用 connector/link；非法组合 fail closed。

`reference_project_manifest` 冻结 annotation project 本身：

```text
reference_schema:chr!, annotation_project_id:chr!, project_protocol_version:chr!,
source_input_hash:chr!, partition_id:chr!, reference_ontology_hash:chr!,
display_protocol_manifest_hash:chr!, rater_roster_hash:chr!,
preregistration_hash:chr!, project_payload_sha256:chr!
```

`annotation_project_id="rp_"+H("stpd-reference-project-v1\0"||canonical row excluding annotation_project_id/project_payload_sha256)`；`project_payload_sha256` 对同一排除列 payload 使用独立 `stpd-reference-project-payload-v1` domain。`PK=annotation_project_id`，sort `annotation_project_id`；一个 `reference_manifest_hash` 对应的 reference product 必须恰有一行 project row，且 manifest四行和全部 annotation/adjudication 行的 `annotation_project_id` 都必须逐字等于该唯一 PK。rater roster 只含排序后的 pseudonym/role/round 资格，不含直接身份；partition 必须逐成员 group-disjoint。

`reference_manifest` 是 reference 产品的唯一内容清单：

```text
reference_schema:chr!, annotation_project_id:chr!, table_name:chr!,
table_schema:chr!, row_count:int!, canonical_table_sha256:chr!
```

必含 `reference_project_manifest/reference_display_protocols/reference_annotations/reference_adjudications` 四行（空表也列出），PK `table_name`，按 table-name byte order。`reference_manifest_hash=H("stpd-reference-manifest-v1\0"||canonical reference_manifest)`。transition 与 Gate C eligibility 中的 `reference_manifest_hash` 必须逐表验证，而不是信任一个孤立字符串；project、display、annotation、adjudication 任一 byte 变化都会产生新 manifest hash。

## 7. ID、lineage 与碰撞合同

### 7.1 ID 格式

通式：`prefix + SHA256("stpd-entity-id-v1:" + domain + NUL + canonical_payload)`，使用完整 64 hex，不截断。payload object 只含下表字段，字段名按 JCS 排序；任何未列字段不得进入 ID。

| domain / prefix | exact payload fields |
|---|---|
| state_candidate / `sc_` | detection_root_id, train, start_isi, end_isi, candidate_generation_rule_registry_id, source_candidate_key_hash |
| state_axis_evidence / `sx_` | detection_root_id, state_candidate_id, start_isi, end_isi, evidence_payload_hash |
| state_episode / `se_` | detection_root_id, train, state_class, start_isi, end_isi, classification_evidence_id |
| state_segment / `ss_` | detection_root_id, state_episode_id, segment_index, segment_role, start_isi, end_isi, source_state_candidate_id, state_axis_evidence_id, connector_decision_id |
| event_candidate / `ec_` | detection_root_id, train, start_isi, end_isi, candidate_generation_rule_registry_id, source_candidate_key_hash |
| event / `ev_` | detection_root_id, train, event_class, start_isi, end_isi, base_event_evidence_id |
| event_modifier_evidence / `me_` | detection_root_id, event_id, modifier_domain, evidence_id |
| gap_candidate / `gc_` | detection_root_id, train, start_isi, end_isi, gap_policy_registry_id |
| gap / `gp_` | detection_root_id, gap_candidate_id |
| boundary / `bd_` | detection_root_id, train, boundary_domain, boundary_geometry, edge_side, start_isi, end_isi, reason_registry_id, evidence_id |
| state_abstention / `sa_` | detection_root_id, state_candidate_id, start_isi, end_isi, state_axis_evidence_id |
| gap_abstention / `ga_` | detection_root_id, gap_candidate_id, evidence_id |
| connector_decision / `cd_` | detection_root_id, pre_direct_candidate_id, gap_start_isi, gap_end_isi, post_direct_candidate_id, evidence_id |
| review_candidate / `rv_` | detection_root_id, candidate_domain, suggested_class_registry_id, train, start_isi, end_isi, evidence_id |
| event_state_relationship / `er_` | detection_root_id, event_id, state_episode_id |
| interrupted_state_relationship / `ir_` | detection_root_id, event_id, parent_state_candidate_id, lineage_closure_hash |
| episode_link / `lk_` | detection_root_id, pre_state_episode_id, gap_id, post_state_episode_id, link_policy_registry_id, link_evidence_id |
| scientific_context_epoch / `ce_` | detection_root_id, train, epoch_domain, start_isi, end_isi, source_evidence_id |
| evidence / `ed_` | detection_root_id, evidence_type_registry_id, evidence_schema_registry_id, source_bytes_sha256, evidence_json_hash |
| lineage / `ln_` | detection_root_id, child_domain_id, child_id, parent_edge_set_hash |
| entity_domain / `dm_` | entity_domain, target_table, primary_key_columns_json, train_column, product_scope |
| partition / `pt_` | partition_version, sorted canonical partition_membership rows excluding partition_id |
| multiple_testing_family / `mf_` | detection_root_id, train, gap_policy_registry_id, scientific_context_epoch_id (typed nullable), sorted complete gap_candidate IDs |
| threshold_policy / `tp_` | detection_root_id, path, threshold_source_class, adaptation_unit, label_access, policy_parameter_hash, partition_id |
| threshold_instance / `ti_` | detection_root_id, threshold_policy_id, path, application_scope, application_scope_key_hash, effective_value_hash |
| threshold_binding / `tb_` | detection_root_id, consumer_domain_id, consumer_id, path, threshold_instance_id, binding_role_registry_id |
| diagnostic / `dg_` | detection_root_id, diagnostic_code_registry_id, entity_domain_id, entity_id, train, start_isi, end_isi, detail_schema_registry_id, detail_json_hash |
| migration / `mg_` | source_product_hash, target_detection_root_id, mapping_status, source_entity_domain_id, source_entity_id, target_entity_domain_id, target_entity_id |
| transition / `tx_` | detection_root_id, parent_auto_product_hash, sequence_no, previous_transition_hash, action_type, target_domain_id, target_id, payload_schema_registry_id, payload_json_hash |

其他辅助表的 row key 使用其已定义复合 PK，不另生实体 ID。`registry_entry_id` 按 6.16 公式生成；reference IDs 按 6.20 公式生成。相同 payload 必须产生相同 ID；同 ID 不同 payload 是 fatal collision，产品 `failed_closed`。所有表中以 `_id` 结尾且不是明确外部 pseudonym/hash 的字段，必须在本节、6.16、6.20、14.3 或其 FK target中找到唯一生成规则；schema-bundle lint 对未解析 ID 必须失败。唯一允许的外部审计标识是已在对应签名/环境 manifest 中定义格式与信任域的 `execution_id/native_build_id/toolchain_id/key_id/stable_test_id/contract_id/fixture_id`；它们不得充当科学实体 FK，也不得进入 scientific entity ID。

### 7.2 Lineage

- split 后残段必须新建 ID 和 lineage edge；不得继承父 accepted ID；
- merge 只允许由显式 source edges 构成新 ID；
- AUTO ID 不受行排序/UI/语言影响；
- FINAL 未修改科学对象保留 AUTO entity ID；几何、class、source evidence或lineage发生变化的对象创建 FINAL entity ID，并通过 transition/evidence edge 指回 AUTO；
- `review_candidates.candidate_status` 与 `state_episode_links.link_status` 是已存在candidate/relationship的status-only adjudication，显式保留原ID；变化由transition chain、canonical row hash和FINAL product hash证明，不得被解释为新的科学实体；
- 跨 detection root 不允许仅凭几何复用 ID；AUTO 与其 FINAL 共享 detection root，因此未修改实体保留 ID，修改实体依新 evidence/lineage 产生新 ID。

## 8. Canonical serialization 与无环 hash DAG

### 8.1 Typed-table canonical bytes

规范 hash 使用 `stpd_canonical_table_v1`，不是 R `serialize()`、CSV 字节或当前 locale：

1. 顶层为 UTF-8 JSON：`{schema_id, columns, rows}`；
2. object key 按 RFC 8785/JCS 排序；rows 依本文 sort key；
3. columns 明示 name/type/nullability/enum；rows 使用定长数组；
4. strings NFC；integers 十进制；double 使用最短 round-trip 十进制；`-0` 规范化为 `0`；
5. nullable 使用 JSON `null`；禁止 NaN/Inf；
6. 不包含 R attributes、row.names、factor、timezone/locale 或生成时间。

`canonical_table_sha256` 与 `csv_sha256/rds_sha256` 必须分列。后两者只证明 artifact 字节，不替代语义 hash。

### 8.2 Hash DAG

所有 hash 使用 raw 32-byte SHA-256，文本表示为 lowercase 64 hex。domain separator 使用 ASCII 字节并以 NUL 终止；连接对象均为上一节 canonical JSON bytes，不使用模糊字符串拼接。

```text
normalized_input_manifest_hash =
  H("stpd-normalized-input-manifest-v1\0" || canonical_input_manifest)

requested_params_hash =
  H("stpd-requested-params-v1\0" || canonical_requested_params_manifest)

partition_membership_manifest_hash =
  H("stpd-partition-membership-v1\0" || canonical_partition_membership_source_manifest)

threshold_policy_source_manifest_hash =
  H("stpd-threshold-policy-source-v1\0" || canonical_threshold_policy_source_manifest)

threshold_instance_source_manifest_hash =
  H("stpd-threshold-instance-source-v1\0" || canonical_threshold_instance_source_manifest)

qc_input_manifest_hash =
  H("stpd-qc-input-v1\0" || canonical_qc_input_manifest)

source_manifest_hash =
  H("stpd-source-manifest-v1\0" || canonical_source_manifest)

native_manifest_hash =
  H("stpd-native-manifest-v1\0" || canonical_native_manifest)

toolchain_manifest_hash =
  H("stpd-toolchain-manifest-v1\0" || canonical_toolchain_manifest)

detector_policy_hash =
  H("stpd-detector-policy-manifest-v1\0" || canonical_detector_policy_manifest)

label_blind_contract_hash =
  H("stpd-label-blind-contract-manifest-v1\0" || canonical_label_blind_contract_manifest)

code_identity_hash =
  H("stpd-code-identity-v1\0" || canonical{source_manifest_hash,
    native_manifest_hash, package_version, toolchain_manifest_hash})

source_context_hash =
  H("stpd-detection-root-v1\0" || canonical{
    normalized_input_manifest_hash, requested_params_hash,
    threshold_policy_source_manifest_hash,
    threshold_instance_source_manifest_hash,
    partition_membership_manifest_hash, qc_input_manifest_hash,
    code_identity_hash, detector_policy_hash, label_blind_contract_hash})

detection_root_id = "dr_" + source_context_hash

canonical_manifest_core_hash =
  H("stpd-canonical-manifest-core-v1\0" || canonical_manifest_bytes)

product_context_hash =
  H("stpd-product-context-v1\0" || canonical{
    product_kind, detection_root_id, parent_auto_product_hash,
    history_head_hash, canonical_manifest_core_hash,
    contract_sha256, schema_contract_sha256})

product_hash = H("stpd-product-v3\0" || canonical{
  identity_schema, product_kind, detection_root_id, source_context_hash,
  product_context_hash, parent_auto_product_hash, history_head_hash,
  canonical_manifest_core_hash, contract_sha256, schema_contract_sha256})
```

`execution_id` 为 UUIDv4，只进入 status/audit envelope，不进入上述 hash。AUTO 与 FINAL 共享 immutable `detection_root_id/source_context_hash`；FINAL 的 `product_context_hash/product_hash` 另绑定 parent AUTO 和 history。因此未被 review 修改的 entity ID 与 scientific payload 可保持不变，产品本身仍因 history 产生新 hash。所谓“非目标字节不变”特指 canonical scientific payload（排除 status/audit envelope）及其 entity ID 不变。

上式 leaf manifest 的 exact rows：

```text
normalized_input_manifest:
  train:chr!, n_spikes:int!, timestamp_unit:chr!,
  normalized_timestamp_bytes_sha256:chr!, acquisition_context_row_hash:chr!,
  patient_group_id_hash:chr?, session_group_id_hash:chr?,
  dataset_batch_id_hash:chr?
  PK=train; sort=train

requested_params_manifest:
  path:chr!, value_type:chr!, canonical_value_hash:chr!
  PK=path; sort=path

partition_membership_source_manifest:
  partition_id:chr!, partition_version:chr!, group_id_hash:chr!, group_role:chr!,
  patient_group_id_hash:chr?, session_group_id_hash:chr?,
  source_input_hash:chr!, membership_row_hash:chr!
  PK=(partition_id,group_id_hash); sort=PK

threshold_policy_source_manifest:
  path:chr!, units:chr!, threshold_source_class:chr!, adaptation_unit:chr!,
  label_access:chr!, threshold_algorithm_semantic_sha256:chr!,
  policy_parameter_hash:chr!, requested_value_hash:chr!,
  frozen_before_validation:lgl!, partition_id:chr?, policy_source_row_hash:chr!
  PK=(path,threshold_source_class,adaptation_unit,policy_parameter_hash,partition_id);
  sort=PK

threshold_instance_source_manifest:
  policy_source_row_hash:chr!, path:chr!, application_scope:chr!,
  source_scope_key_hash:chr!, train:chr?, session_group_id_hash:chr?,
  dataset_batch_id_hash:chr?, derivation_input_manifest_hash:chr!,
  application_input_manifest_hash:chr!,
  derivation_statistics_schema_registry_id:chr?, derivation_statistics_json:json?,
  application_statistics_schema_registry_id:chr?, application_statistics_json:json?,
  value_type:chr!, value_num:dbl?, value_chr:chr?, units:chr!,
  effective_value_hash:chr!, instance_evidence_source_hash:chr!
  PK=(policy_source_row_hash,path,application_scope,source_scope_key_hash); sort=PK

source_manifest:
  repository_relative_path:chr!, file_type:chr!, size_bytes:dbl!,
  content_sha256:chr!
  PK=repository_relative_path; sort=PK

native_manifest:
  repository_relative_path:chr!, file_type:chr!, size_bytes:dbl!,
  content_sha256:chr!, native_build_id:chr!
  PK=repository_relative_path; sort=PK

toolchain_manifest:
  toolchain_id:chr!, os:chr!, arch:chr!, R_version:chr!,
  compiler_id:chr!, compiler_version:chr!, BLAS_id:chr!, LAPACK_id:chr!,
  native_abi:chr!, numeric_equivalence_class:chr!
  PK=toolchain_id; exactly one row; sort=PK

detector_policy_manifest:
  policy_path:chr!, value_type:chr!, canonical_value_hash:chr!,
  governing_registry_entry_id:chr!
  PK=policy_path; sort=PK

label_blind_contract_manifest:
  contract_rule_registry_id:chr!, allowlist_schema_registry_id:chr!,
  source_domain_registry_id:chr!, action:chr!,
  semantic_definition_sha256:chr!
  PK=contract_rule_registry_id; sort=PK

qc_input_manifest:
  train:chr!, qc_input_schema:chr!, canonical_qc_input_hash:chr!
  PK=train; sort=train
```

normalized timestamp bytes是 little-endian IEEE-754 binary64有限 seconds，先按原 spike order验证严格递增；禁止包含标签、row name、UI selection或R attrs。

`toolchain_id=H("stpd-toolchain-id-v1\0"||canonical toolchain row excluding toolchain_id)`；其余字段均为NFC UTF-8非空字符串，`numeric_equivalence_class=numeric_bitwise_equivalent|toolchain_bound`。`detector_policy_manifest.value_type` 与 typed-value/hash规则逐字复用下段 requested-parameter合同；`governing_registry_entry_id` 必须 FK 到 active operational/classification policy entry。`label_blind_contract_manifest.action=allow|forbid|strip`；三个ID分别 FK 到 `label_blind_rule/input_allowlist_schema/input_source_domain` registry domain，且 `semantic_definition_sha256` 与 rule registry逐字一致。三个 manifest 均为 root-free；空 detector-policy或label-blind-contract manifest非法。实现不得用 package metadata中的自由文本、运行时环境对象或自报布尔值代替这些 canonical rows。

`requested_params_manifest.value_type=double|integer|logical|string|null`。令`typed_value_json=canonical{value_type,value}`：double必须是有限binary64且按8.1编码，integer为32-bit十进制，logical为JSON boolean，string为NFC UTF-8，null的value只能是JSON null；其余类型或表示一律拒绝。`canonical_value_hash=H("stpd-requested-param-value-v1\0"||typed_value_json)`。

`threshold_instances.value_type=double|string`，且double时只有`value_num`非NA、string时只有`value_chr`非NA；units必须引用相同policy的单位。`policy_source_row_hash=H("stpd-threshold-policy-source-row-v1\0"||canonical row excluding policy_source_row_hash)`。`requested_value_hash` 必须逐字等于 `requested_params_manifest` 同path行的 `canonical_value_hash`；`policy_parameter_hash=H("stpd-threshold-policy-parameters-v1\0"||canonical sorted {path,value_type,canonical_value_hash} rows actually read by the threshold algorithm)`，其path集合必须逐字等于对应algorithm registry entry的parameter-path allowlist。`effective_value_hash=H("stpd-effective-threshold-value-v1\0"||canonical{value_type,value_num,value_chr,units})`。

`application_input_manifest_hash=H("stpd-threshold-application-input-v1\0"||canonical authorized normalized-input rows)`：global取当前产品全部rows，train取exact train row，session取`session_group_id_hash`相等的全部rows，dataset_batch取`dataset_batch_id_hash`相等的全部rows；空集合非法。`derivation_input_manifest_hash`按source class唯一生成：`fixed_preregistered`哈希domain-separated canonical空manifest；`development_trained`哈希exact partition中role为development/calibration的sorted `{partition_id,group_id_hash,group_role,source_input_hash}` rows，且validation/excluded绝不进入；`per_train_unsupervised`哈希该train的normalized-input row；`batch_transductive`哈希该batch的authorized normalized-input rows。公式统一为`H("stpd-threshold-derivation-input-v1\0"||canonical{threshold_source_class,authorized_derivation_rows})`；不允许把application rows代替development derivation rows。

`instance_evidence_source_hash=H("stpd-threshold-instance-evidence-source-v1\0"||canonical{threshold_algorithm_semantic_sha256,policy_source_row_hash,path,application_scope,source_scope_key_hash,derivation_input_manifest_hash,application_input_manifest_hash,derivation_statistics_schema_registry_id,derivation_statistics_json,application_statistics_schema_registry_id,application_statistics_json,effective_value_hash})`。两组 schema/JSON 必须各自成对同为null或同为非null；非null schema必须是active `evidence_schema` registry entry。derivation statistics 只能读取允许矩阵中的 derivation rows；application statistics只能读取矩阵中的无标签 application rows，禁止任何 MANUAL/reference/review/final派生值。active threshold-algorithm registry entry必须冻结两组统计各自的 required|optional|prohibited 状态及exact JSON schema；`development_trained/session` 如需 session adaptation，必须把development/calibration训练统计放前一组、当前无标签session统计放后一组，不能混为一个不可审计JSON。材料化`instance_evidence_id`所指evidence必须包含同一完整payload并可删除detection-root字段后重算该source hash。

上述三个 threshold/partition **source manifest 均不含 `detection_root_id` 或由它派生的 entity ID**；否则 source context 会形成环。`partition_id="pt_"+H("stpd-partition-v1\0"||canonical membership rows excluding partition_id)`，因此可安全出现在 root-free membership manifest。`source_scope_key_hash` 只使用该scope的root-free `application_input_manifest_hash`与global/train/session/batch identity；derivation权限由独立`derivation_input_manifest_hash`和partition closure证明，两者都不使用 detection root。source context确定后，6.13 的 materialized rows机械加入 detection root，再从 `{detection_root_id,application_scope,source_scope_key_hash}` 生成 `application_scope_key_hash`，并按第7节生成 policy/instance/binding IDs；validator逐字段证明材料化表与root-free source manifests等价。`acquisition_context_row_hash` 同样来自不含 detection root 的 acquisition/QC allowlist投影，而不是下游 `scientific_context` 表 hash。detector policy和label-blind contract是签名 registry/schema bundle的canonical hash。

无环顺序：

```text
raw input manifest + code/native manifest + policy + requested params
  -> QC/effective-threshold canonical hashes
  -> source_context_hash
  -> entity IDs and canonical tables
  -> canonical table hashes
  -> canonical_manifest_core_hash
  -> product_hash
  -> metadata envelope
  -> embedded release-attestation core/signature
  -> built tarball hash
  -> detached distribution signature
```

`product_hash`、signature、artifact filenames、approval status 和 `generated_utc` 不得处于其自身祖先。`materialized_product_identity` 是上述公式的展示 envelope，不作为 canonical manifest member。

### 8.3 `canonical_manifest`

```text
manifest_schema:chr!, product_kind:chr!, table_name:chr!,
table_schema:chr!, required:lgl!, row_count:int!,
canonical_table_sha256:chr!, sort_contract_registry_id:chr!
```

PK `table_name`，按 table name byte order。`sort_contract_registry_id` FK to `operational_policy` registry且 semantic JSON 必须逐字列出该表在第6节规定的 sort key与 nullable排序。第 8.4 inventory 中 required 的所有规范表，包括 0 行表，都必须列出。status envelope、materialized identity、artifact manifest 与签名不进入该表。

### 8.3a `artifact_manifest`

导出字节使用独立、非规范科学内容的 manifest：

```text
artifact_manifest_schema:chr!, product_kind:chr!, artifact_path:chr!,
artifact_role:chr!, media_type:chr!, canonical_table_name:chr?,
required:lgl!, size_bytes:dbl!, artifact_sha256:chr!
```

`artifact_role=canonical_json|interchange_csv|typed_rds|identity|manifest|diagnostic_log`；PK `artifact_path`，路径为 generation-relative POSIX path，禁止 `..`、绝对路径和 symlink。`artifact_manifest_hash=H("stpd-artifact-manifest-v1\0"||canonical artifact manifest)`；它不进入 `product_hash`（避免导出格式改变科学产品），但必须进入 product-anchor signed payload与 reader验证。canonical JSON重新解析后的 typed-table hash必须等于 canonical manifest；CSV/RDS只按artifact hash验证，不成为科学真相来源。

为避免自引用，`artifact-manifest.json` 本身、`product-status.json`、`release-attestation.json`、`product-attestation.json` 及任何 signature envelope **不得**成为 `artifact_manifest` 行。它们使用固定路径并分别通过以下方式验证：manifest bytes重算 `artifact_manifest_hash`；release attestation bytes重算 status/product-anchor 中的 `release_attestation_hash` 并验证签名；product-status 先排除 `product_attestation_hash` 计算 `status_core_hash=H("stpd-product-status-core-v1\0"||canonical status core)`；product attestation作为 detached signature同时验证 exact artifact-manifest hash与status-core hash。`product_attestation_hash` 是最终 product-attestation signature envelope bytes 的 SHA-256，只作为回指，不进入 status core。reader 先验证这四者，再依据 manifest验证其余文件。任何实现把签名文件、status回指或manifest自身加入其被签祖先都必须由 schema lint判为 hash cycle。

### 8.4 Canonical product inventory

| table | AUTO | FINAL | 0-row allowed |
|---|---:|---:|---:|
| `state_candidates` | required | required | 是 |
| `state_episodes` | required | required | 是 |
| `state_segments` | required | required | 是 |
| `state_axis_evidence` | required | required | 是 |
| `per_isi` | required | required | 仅 0-ISI input |
| `event_candidates` | required | required | 是 |
| `events` | required | required | 是 |
| `event_modifier_evidence` | required | required | 是 |
| `gap_candidates` | required | required | 是 |
| `gaps` | required | required | 是 |
| `boundary_evidence` | required | required | 是 |
| `state_abstentions` | required | required | 是 |
| `gap_abstentions` | required | required | 是 |
| `state_connector_decisions` | required | required | 是 |
| `review_candidates` | required | required | 是 |
| `event_state_relationships` | required | required | 是 |
| `event_interrupted_state_relationships` | required | required | 是 |
| `state_episode_links` | required | required | 是 |
| `entity_evidence_edges` | required | required | 是 |
| `evidence_records` | required | required | 是 |
| `lineage_records` | required | required | 是 |
| `threshold_policies` | required | required, byte-identical to AUTO | 否 |
| `threshold_instances` | required | required, byte-identical to AUTO | 否 |
| `threshold_instance_bindings` | required | required | 是 |
| `partition_memberships` | required | required, byte-identical to AUTO | 仅当不存在 `development_trained` policy |
| `diagnostic_records` | required | required | 是 |
| `scientific_context` | required | required, byte-identical to AUTO | 仅 0-train input |
| `scientific_context_epochs` | required | required, byte-identical to AUTO | 是 |
| `contract_registries` | required | required, byte-identical to AUTO | 否 |
| `entity_domain_registry` | required | required, byte-identical to AUTO | 否 |
| `adjudication_transitions` | forbidden | required | 是 |
| `migration_records` | required | required | 是 |

candidate 与 authoritative 使用同一 inventory；差别只在 status/attestation envelope。`not_present/materializing/failed_closed/requires_redetection` 没有 canonical manifest。

FINAL 中以下 AUTO source-of-evidence tables 必须逐字节相同：`state_candidates`、`state_axis_evidence`、`event_candidates`、`gap_candidates`、`state_connector_decisions`、`threshold_policies`、`threshold_instances`、`partition_memberships`、`scientific_context`、`scientific_context_epochs`、`contract_registries`、`entity_domain_registry`。人工 transition 不改写 detector候选/证据；它只增加 transitions/review evidence/lineage，并重建受影响的 accepted projections、relationships、bindings、diagnostics与manifest。任何 action试图修改上述 immutable表即 stale/tamper failure。

### 8.5 三种完整性保证

```text
integrity_level = none | detached_internal_integrity |
                  parent_bound_recomputed |
                  externally_anchored_product
```

- `detached_internal_integrity` 只证明 schema/FK/geometry/hash 自洽，不能证明该产品确由 detector 产生，永远不能单独授予 official authority；
- `parent_bound_recomputed` 必须取得 exact normalized raw/QC/params/threshold/code identity，重建 source context并确定性重算规范产品；live official accessor 最低要求此级别；
- `externally_anchored_product` 由与源码仓库隔离的 product-attestation service 对第 12.2 节规定的 exact `product_anchor` 八字段 payload签名，或由不可变公开归档对同一 payload锚定；detached official export 最低要求此级别。

因此“协调重封必须被 detached validator 发现”不再作为不可能的承诺；协调重封攻击由 parent recomputation 或 external product anchor 检出。detached-only bundle 必须醒目标为 internally consistent candidate。

## 9. Label-blind 权限边界

AUTO v3 必须从 allowlist 重新构造：train ID、timestamp/ISI、不可变 acquisition identifiers、预先定义的 QC inputs。禁止复制 parent dataset 后再删除字段。以下任一数据或 attribute/cached derivative 都禁止读取：

```text
pattern_manual, pattern_manual_negative, pattern_final,
review/adjudication columns, learned/manual train ranges,
manual-derived parameters, prior AUTO/FINAL/multitrack products,
candidate ledgers produced under manual-aware mode,
threshold/cache attrs that observed held-out labels
```

Metamorphic requirement：向同一 raw/QC input 注入、改变或删除上述任意禁止字段/属性，AUTO 的所有 canonical table bytes 与 source-context-eligible fields 必须不变。

阈值来源权限以 `threshold_policies/threshold_instances/partition_memberships/threshold_instance_bindings` 记录。默认 new-patient Gate C estimand 只允许 `fixed_preregistered`、只在 development groups 学得且 validation overlap 为 0 的 `development_trained`，或预注册的 per-train unsupervised policy。`batch_transductive` 必须单列 estimand，不能混入默认性能结果。

## 10. FINAL replay 状态机

1. 读取 exact authoritative/pending AUTO parent（候选复核可基于 pending，但不能变 official）；
2. 验证 history chain、CAS parent、action payload、actor/evidence ID；
3. 依 sequence 回放；
4. 每次 action 后重建 affected entity IDs、lineage、per-ISI、relationship closure、counts；
5. 回放结束后运行同强度但 FINAL-aware 的 validator：immutable AUTO candidate/evidence tables与 exact parent逐字一致；accepted projections/relationships/per-ISI 必须等于“parent AUTO + ordered history”确定性回放结果，而不是再次强迫等于未复核的 AUTO accepted set；
6. 生成 FINAL canonical tables、manifest 与 product hash。

非法 action、stale parent、缺 transition、链分叉、未知 payload schema、跨 train target 或 geometry 越界全部 `failed_closed`。FINAL 永远是 reviewed prediction record，不是 biological truth。reference annotations 存在独立产品/数据集，不写回 AUTO，也不自动成为下一次 calibration 输入。

## 11. 产品存在矩阵与 fail-closed

| status | execution/root/product hash | Gate B / authority / authoritative | integrity | required reason | canonical tables | accessor |
|---|---|---|---|---|---:|---|
| `not_present` | all NA | pending / none_candidate / FALSE | none | all NA | 否 | typed absent |
| `materializing` | execution required；root/hash may NA | pending / none_candidate / FALSE | none | all NA | staging only | all public access拒绝 |
| `candidate_pending` | execution/root/product required | pending / none_candidate / FALSE | detached或parent-bound | all NA | inventory全部 | candidate only |
| `gate_b_authoritative` live | execution/root/product required | attested / product-kind scope / TRUE | parent_bound_recomputed | all NA；release attestation required | inventory全部 | live official allowed |
| `gate_b_authoritative` export | execution/root/product required | attested / product-kind scope / TRUE | externally_anchored_product | all NA；release+product attestation required | inventory全部 | detached official allowed |
| `failed_closed` | execution optional；root optional；product hash NA | failed / none_candidate / FALSE | none | failure_stage+failure_code_registry_id required | 否；staging quarantine不公开 | typed failure |
| `requires_redetection` | execution optional；root/hash NA | pending / none_candidate / FALSE | none | requires_redetection_reason_registry_id required | 否 | typed migration status |

所有行固定 `gate_c_status=pending, detector_performance_eligible=FALSE, biological_ground_truth=FALSE`。`reference_standard_status` 只表示独立参考是否另行存在，不改变任何合法组合。未列组合全部 illegal。status envelope 自身通过 exact prototype验证，不需要伪造 counts/source hash。

legacy v1/v2 检测可在 v3 failure 时成功，但 legacy accessor 与 v3 accessor 必须完全分离；不得 silent fallback。正式 v3 bundle 若 stale cleanup 或 commit 失败，整个 v3 export 失败，不得声称 omitted/success。

## 12. 发布证明与信任根

### 12.1 Embedded attestation

隔离 CI 使用仓库外私钥签名 attestation core；core 是以下 exact prototype，不允许增删或改名：

```text
release_attestation_schema:chr!, release_version:chr!,
source_manifest_sha256:chr!, native_manifest_sha256:chr!,
toolchain_manifest_sha256:chr!,
contract_sha256:chr!, schema_contract_sha256:chr!,
fixture_manifest_sha256:chr!, test_manifest_sha256:chr!,
test_result_bundle_sha256:chr!, gate_b_evidence_table_sha256:chr!,
required_contract_id_set_sha256:chr!, environment_manifest_sha256:chr!,
R_version:chr!, toolchain_id:chr!, package_build_manifest_sha256:chr!,
installed_manifest_sha256:chr!, gate_b_approval_manifest_sha256:chr!,
all_required_contracts_passed:lgl!
```

`release_attestation_schema=stpd_gate_b_v3_release_attestation_1`；`all_required_contracts_passed` 必须为 TRUE。`required_contract_id_set_sha256=H("stpd-required-contract-id-set-v1\0"||canonical sorted unique required contract IDs)`；`gate_b_evidence_table_sha256=H("stpd-gate-b-evidence-table-v1\0"||canonical typed evidence-record table)`；`toolchain_manifest_sha256` 必须逐字等于第8.2节的 `toolchain_manifest_hash`。所有 manifest/hash必须由其 exact bytes重算并逐项闭合，且core的 `R_version/toolchain_id` 必须逐字等于该toolchain manifest唯一row。embedded-release domain 的 `payload_sha256` 必须等于 `H("stpd-embedded-release-attestation-core-v1\0"||canonical exact core)`。签名算法固定 Ed25519；公钥 fingerprint 由 release channel/文档独立发布。私钥、可改公钥和测试结果不得来自同一可写工作区。运行时只接受经受信 fingerprint 验证的 embedded attestation；本地开发 build 永远 pending。

`gate_b_approval_manifest` 在两份 candidate 规范完成独立复审后单独生成，规范正文状态不再改写：

```text
approval_schema:chr!, plan_sha256:chr!, normative_contract_sha256:chr!,
schema_contract_sha256:chr!, contract_fixture_spec_sha256:chr!,
science_review_sha256:chr!, science_verdict:chr!,
engineering_review_sha256:chr!, engineering_verdict:chr!,
review_resolution_sha256:chr!, approved_stage:chr!,
approver_pseudonym:chr!, approved_utc:chr!, approval_payload_sha256:chr!
```

两个 verdict 必须均为 `approve_for_implementation`（最终发布时为 `approve_for_gate_b_release`）。`approval_payload_sha256=H("stpd-gate-b-approval-payload-v1\0"||canonical approval row excluding approval_payload_sha256)`；approval signature 位于外部 signature envelope，不写回 payload。规范、复审或 resolution 任一 byte 改变即生成新 candidate version和新 approval，避免“改成 approved 后 SHA 改变”的循环。

每项合同的证据行固定为：

```text
evidence_record_schema:chr!, evidence_record_id:chr!, contract_id:chr!, fixture_id:chr!,
repository_relative_test_path:chr!, test_name:chr!, expected_result_hash:chr!,
actual_result_hash:chr!, status:chr!, source_manifest_sha256:chr!,
test_manifest_sha256:chr!, contract_sha256:chr!, schema_contract_sha256:chr!,
fixture_manifest_sha256:chr!, environment_manifest_sha256:chr!
```

`evidence_record_schema=stpd_gate_b_v3_evidence_record_1`，`status=pass|fail|error|skip`。`evidence_record_id=H("stpd-gate-b-evidence-record-v1\0"||canonical row excluding evidence_record_id)`；PK `evidence_record_id`，unique `(contract_id,fixture_id,repository_relative_test_path,test_name)`，按该四元组排序。所有 `*_hash/*_sha256` 均为 lowercase 64 hex并按字段名所指exact manifest重算；required contract的唯一通过状态是`pass`，任何`fail|error|skip`均使其失败。attestation core 必须包含证据表 canonical hash、required contract ID set hash 和 `all_required_contracts_passed=TRUE`；缺行、重复、额外未知 required ID 或任一 non-pass 均拒绝。

签名 `fixture_manifest` 必须至少包含 exact `fixture_expected_results` typed table：

```text
contract_id:chr!, fixture_id:chr!, repository_relative_test_path:chr!, test_name:chr!,
expected_result_schema:chr!, expected_result_json:json!, expected_result_hash:chr!
```

`PK=(contract_id,fixture_id,repository_relative_test_path,test_name)`；sort=PK；`expected_result_hash=H("stpd-fixture-expected-result-v1\0"||canonical{expected_result_schema,expected_result_json})`。evidence row 的 `expected_result_hash` 必须逐字 FK 到同四元组 fixture row；fixture manifest hash必须覆盖该完整 typed table。

`test_result_bundle` 的 exact prototype 为：

```text
test_bundle_schema:chr!, stable_test_id:chr!, contract_id:chr!, fixture_id:chr!,
repository_relative_test_path:chr!, test_name:chr!, command_argv_json:json!,
exit_code:int!, discovered_n:int!, passed_n:int!, failed_n:int!, error_n:int!,
skipped_required_n:int!, skipped_optional_n:int!, report_sha256:chr!,
stdout_sha256:chr!, stderr_sha256:chr!, environment_manifest_sha256:chr!
```

`test_bundle_schema=stpd_gate_b_v3_test_bundle_1`；`stable_test_id=H("stpd-stable-test-id-v1\0"||canonical{contract_id,fixture_id,repository_relative_test_path,test_name,command_argv_json})`；PK `stable_test_id`，unique `(contract_id,fixture_id,repository_relative_test_path,test_name)`，sort `(contract_id,fixture_id,repository_relative_test_path,test_name,stable_test_id)`。所有count必须非负，且`passed_n+failed_n+error_n+skipped_required_n+skipped_optional_n=discovered_n`。路径为仓库相对 POSIX path，禁止绝对用户路径。令`actual_result_hash=H("stpd-test-result-row-v1\0"||canonical complete test-result row)`；每个 evidence row 必须按同一 contract/fixture/path/name 一对一 FK 到该 row，且其 `actual_result_hash` 逐字相等。`test_result_bundle_sha256=H("stpd-test-result-bundle-v1\0"||canonical typed table sorted as above)`。required contract 必须 `exit_code=0, failed_n=0, error_n=0, skipped_required_n=0` 且对应 evidence status为pass；仅进程退出 0 不足以通过。日志进入签名 hash前必须去除用户名、home path、host/IP和直接身份。

### 12.2 Distribution signature

包构建完成后，令 `distribution_payload_sha256=H("stpd-distribution-tarball-payload-v1\0"||canonical{tarball_sha256,embedded_attestation_sha256,release_version})` 并生成 detached signature。签名不放回被签 tarball，避免循环。安装/发布步骤必须先验证 detached signature；运行时验证 embedded signature与当前安装 manifest。

两个 signature envelope 统一为 canonical JSON：

```text
signature_schema:chr!, signature_domain:chr!, payload_sha256:chr!,
algorithm:chr!, key_id:chr!, public_key_fingerprint_sha256:chr!,
signature_base64:chr!, release_sequence:chr!,
not_before_utc:chr!, not_after_utc:chr!
```

`signature_schema=stpd_gate_b_v3_signature_1`，`algorithm=Ed25519`，`signature_domain=gate_b_approval|embedded_release|distribution_tarball|product_anchor`。Base64 使用 RFC 4648 standard alphabet、含 padding；key ID/fingerprint 必须出现在独立 trust store。`release_sequence`是canonical unsigned-64十进制字符串，regex `0|[1-9][0-9]{0,19}` 且数值不超过`18446744073709551615`；比较按无前导零十进制整数进行，必须不小于 trust store 的 anti-rollback floor。key rotation使用由旧受信 key签名的新 key statement，revocation list由独立 release channel签名。未知/过期/撤销 key、sequence rollback或payload mismatch均 typed拒绝。

实际 Ed25519 message 固定为 `"stpd-gate-b-signature-envelope-v1\0" || canonical{signature_schema,signature_domain,payload_sha256,algorithm,key_id,public_key_fingerprint_sha256,release_sequence,not_before_utc,not_after_utc}`；即 exact envelope core 排除 `signature_base64`，其余字段全部被签。四个domain的 `payload_sha256` 唯一映射为：`gate_b_approval -> approval_payload_sha256`；`embedded_release -> H("stpd-embedded-release-attestation-core-v1\0"||canonical exact core)`；`distribution_tarball -> distribution_payload_sha256`；`product_anchor -> H("stpd-product-anchor-payload-v1\0"||canonical exact product-anchor payload)`。`not_before_utc/not_after_utc` 使用第2.2节格式且前者严格早于后者。verification 必须同时验证签名、domain、payload、key/fingerprint、有效期与anti-rollback sequence，禁止只对 `payload_sha256` 的64 hex文本签名或忽略envelope字段。

`product_anchor` 的被签 payload 恰为 `{product_hash,detection_root_id,product_kind,release_attestation_hash,normalized_input_manifest_hash,artifact_manifest_hash,status_core_hash,release_sequence}`；payload中的`release_sequence`必须逐字等于signature envelope同名canonical unsigned-64十进制字符串，并通过同一trust-store anti-rollback floor；不得另设未登记的anchor序号，也不得只签自报 status。parent-bound live验证receipt使用同一payload另加 `verification_mode=deterministic_recompute` 与 verifier build identity。

任一 source/test/contract/schema/fixture/native byte 变化都会使旧 attestation 失效，必须重新跑最终测试与双人复审。

`installed_manifest` 固定记录仓库相对 package path、file type、mode、size、SHA-256 与 native binary build ID，按 path byte order；不得含 mtime、绝对路径或 host。为避免 embedded-attestation 自引用，它和 `package_build_manifest` 均明确排除 embedded attestation/signature envelope自身；这些固定路径由 attestation payload hash与签名独立验证。运行时只对 release allowlist内其余文件验证，任何缺失、额外 executable/native 文件或 hash不符均使 embedded attestation无效。schema lint必须证明 package-build、installed、artifact、approval、release、product-anchor与distribution-signature DAG 无环。

### 12.3 支持平台边界

每个 release attestation 通过已签 `toolchain_manifest_sha256` 唯一解析 `{os,arch,R_version,compiler_id,compiler_version,BLAS_id,LAPACK_id,native_abi,numeric_equivalence_class}`，并通过已签 `native_manifest_sha256` 逐文件解析全部 `native_build_id`；不得在 attestation core 外另信任自报平台字符串。canonical encoding golden bytes 必须跨全部支持 tuple一致；detector数值输出只在 `numeric_equivalence_class=numeric_bitwise_equivalent` 的 tuple间承诺逐字节一致，否则 product authority绑定具体 toolchain tuple并要求各自通过语义fixture。未列平台只能运行candidate，不得宣称bitwise authoritative。

## 13. 原子导出与 reader 合同

1. 获取 dataset/product scoped exclusive lock；
2. 在目标目录同一文件系统创建不可变 generation staging directory；
3. 写全部 canonical/artifact 文件，逐文件 flush/fsync；
4. 拒绝 symlink、hardlink escape 与非普通目标；
5. 重新读取并验证 artifact hashes、canonical manifest、product status；
6. fsync generation directory；
7. atomic rename staging -> immutable generation name；
8. 写临时 `CURRENT`，fsync 后 atomic rename 更新指针；
9. reader 只解析一次 `CURRENT`，并始终在同一 generation 读取；
10. ZIP 在临时路径构建、验证、fsync 后 atomic rename。

崩溃测试必须覆盖每个步骤；reader 只能看到旧完整 generation 或新完整 generation，不能看到混合。复用 out_dir 时 stale v3 files、dangling symlink 或不可删除目标必须被识别；清理 postcondition 失败阻断正式 bundle。

上述十步原子 staging/commit 对 official 与 candidate 都适用，但两者使用互斥目录、名称、指针和验证合同。official generation 名为 `gen-<product_hash>-<release_sequence>`；official `CURRENT` 是恰一行 ASCII `generation=<name>\n`，不得是 symlink。official reader在解析 CURRENT 后打开 generation directory、拒绝路径跳转/符号链接，验证 status、identity、canonical/artifact manifest及全部 required signature后才暴露文件。

candidate 只允许位于独立 `multitrack-v3-candidate/` 根，generation 名为 `cand-<product_hash>-<execution_id>`，指针名 `CANDIDATE_CURRENT` 且内容同样为恰一行 `generation=<name>\n`；`execution_id` 必须为该 candidate 的 status-envelope UUIDv4。candidate generation 必须有 `candidate_pending` status、identity、完整 canonical/artifact manifests并通过 detached internal validator，但**不得**包含或伪造 `release-attestation.json/product-attestation.json/release_sequence`，也不得由 official accessor读取。candidate reader验证 product hash、detached integrity和status后只返回 `access=candidate, integrity_level=detached_internal_integrity`；任何 official/attested/authoritative字段为真都使其损坏。candidate ZIP同样从单一candidate generation的显式allowlist在临时路径构建并原子rename，且根metadata固定标记 non-authoritative。candidate不需要 release signature，也永远不能更新 official `CURRENT`。

official 与 candidate 分别使用 `.stpd-v3-export.lock` 和 `.stpd-v3-candidate-export.lock`；lock 内容含 normalized target hash、execution ID和RFC3339 UTC。timeout、stale-lock recovery与force-break原因必须进入审计。

旧 immutable generation 只有在没有 reader lease、对应 CURRENT/CANDIDATE_CURRENT 不再指向且 retention policy允许时回收；loose legacy/stale v3文件从不参与generation allowlist。v1/v2 dual-write不与v3共享事务；formal ZIP按显式version目录/allowlist组装，v1/v2部分成功不能把v3状态改成成功。支持平台必须证明 directory rename与file/dir fsync语义；不满足的平台禁用official writer。candidate writer也必须满足本节原子性；否则只允许返回内存对象，不得把目录/ZIP称为成功导出。

### 13.1 资源、取消与隐私边界

- 规范材料化目标复杂度为 `O(n_isi+n_entities+n_edges)`；任何更高阶步骤必须在 registry 中声明、设置独立预算并有压力 fixture。
- 不得在多表重复保存 timestamp/ISI 原始大向量；`evidence_json` 只保存统计摘要与 source hash。每个 JSON、单表行数、总规范字节、总 artifact 字节和峰值内存上限由签名 operational-policy registry冻结。
- 超预算不得截断 candidate、relationship、lineage 或 per-ISI 表；必须在发布任何 canonical table 前进入 `failed_closed/resource_budget`，code=`resource_budget_exceeded`。
- 取消只在 train/materialization/commit 的可恢复检查点生效；取消后删除 staging、保留旧 CURRENT，并返回 `failed_closed/cancelled`，不得形成部分 candidate。
- progress 只报告阶段、已完成/总 train 和受控计数，不得把患者、操作者、绝对路径、主机/IP或任意 evidence payload写入通知/日志。
- 可移植 bundle 的 diagnostic/evidence/test log 都按签名 redaction schema验证；未知 key、超长字段或直接身份为导出错误。原始技术日志若业务需要，只能留在权限受控的非规范本地位置，且不得被 artifact manifest/ZIP自动收集。

## 14. Consumer、UI 与 Gate C guard

- API 必须要求 `schema_version` 和 capability negotiation；
- `official_*` accessor 只返回 signed `gate_b_authoritative` v3；
- `candidate_*` accessor 明确返回 pending/failed typed state；
- UI 分别显示 episode envelope、direct support、connector、predicted Pause、abstention 和 link candidate；connector 不得着色为 active State；
- Event–State 关系只计一次 episode pair，并分别显示 envelope/direct/connector overlap；
- Gate C 前，所有公开路径（R API、Shiny、CSV/ZIP、reports、batch、validation helper）禁止 estimand `detector_performance`；只可输出 `exploratory_technical_agreement`，并强制 `detector_performance_eligible=FALSE`；
- FINAL 与 reference 的比较命名 `adjudicated_agreement`，不叫 accuracy。

### 14.1 可测试 consumer 接口

固定 R API：

```text
stpd_multitrack_v3_capabilities(x)
stpd_get_multitrack_auto_v3(x, access = "official" | "candidate")
stpd_get_multitrack_final_v3(x, access = "official" | "candidate")
stpd_validate_multitrack_v3(x, integrity = "detached" | "parent_bound")
stpd_export_multitrack_v3(x, out_dir, access = "official" | "candidate")
```

统一 response exact prototype为：

```text
response_schema:chr!, status_code:chr!, product_kind:chr?,
schema_version:chr?, product_hash:chr?, integrity_level:chr!,
capabilities:lst!, data:lst?
```

`response_schema=stpd_multitrack_v3_response_1`；所有字段均为length-one scalar，`capabilities`是固定、sorted-name的feature/version list，`data`非NA时必须是已经按请求access mode完成验证的单一product named list，不能含环境、external pointer或未验证legacy对象。status code 固定：`ok|not_present|pending_not_official|failed_closed|requires_redetection|unsupported_transition_for_schema|unknown_schema|integrity_insufficient|attestation_invalid|product_anchor_missing|version_conflict|corrupt_product`。official request绝不 fallback到candidate/v2/v1；candidate request也不把legacy伪装成v3。

多个版本并存时，只能按调用者明确 version 读取；generic UI selector默认显示最高可理解版本，但必须显示 source/version/status，unknown future schema返回 `unknown_schema`，不得降级读取旧版本而隐藏损坏的新版本。workspace只保存 `{dataset_input_hash,requested_schema,product_hash,access_mode}`，恢复时逐项验证，不保存未经验证的对象指针。

official generation 内固定路径：`multitrack-v3/auto/canonical/<table>.json`、`multitrack-v3/final/canonical/<table>.json`、对应 `artifacts/<table>.csv`、`product-status.json`、`product-identity.json`、`canonical-manifest.json`、`artifact-manifest.json`、`release-attestation.json`、`product-attestation.json`。candidate generation 使用独立顶层 `multitrack-v3-candidate/` 下的对应 `auto|final/canonical`、`artifacts`、status、identity与两个manifest，但禁止出现任何 attestation/signature 文件；不得与 official 文件名同目录或共享 CURRENT。

以上 consumer API 与 Gate-C guard 是 Gate B `MUST`。

### 14.2 Gate C 统计设计 handoff（非 Gate-B 阻断）

以下统计 estimand条目是未来 Gate C 的 candidate handoff，不进入 Gate B promotion条件。未来 Gate C 合同必须版本化采纳或替代它们，不能由 Gate B builder自行实现为“已通过”。Gate C 应分别预注册 episode、direct support、connector/link、Pause、Event–State relationship、modifier、abstention estimands；本 handoff 不填入未经验证的数值门槛：

1. interval/event matching 在同 patient/session、train、domain/class 内做一对一最大基数匹配，次目标最大总预注册 overlap score，再用稳定 ID 打破完全平局；不得 greedy 或 many-to-one；
2. 同时报 ISI-index IoU、time IoU、起止边界误差；匹配阈值由 Gate C 在看 held-out 结果前预注册；
3. State 分别计算 episode-envelope 和 direct-support 的 time/ISI confusion、macro-F1/MCC、IoU；connector 单独计算 false merge/false split 与 connector precision/recall；
4. Pause 报 event P/R/F1、边界/时长误差、false Pause per minute 与 per 1000 ISI；episode link 报 link P/R/F1；
5. Event–State 关系和 modifiers 作为独立 estimand，不从 Event/State 单项性能推断；
6. `uncertain/unscorable/algorithm_abstention` 不得静默删去：必须报告 coverage、conditional performance，并给出将弃权按错判处理的保守界；
7. 独立标注者原始结果、inter/intra-rater agreement 与 adjudicated reference 分开报告；
8. sampling unit 和 bootstrap cluster 固定为 patient/session（若 patient 不可用则 session，且需声明降级），禁止把 ISI 当独立样本；
9. calibration/development/validation 必须 group-disjoint；任一侧独立 group 不足时结构化 `not_estimable`，禁止 fallback 用 validation 学阈值；
10. prevalence sample 与 challenge sample 分开估计，禁止用 challenge set prevalence 代表总体。

### 14.3 独立 Gate C estimand registry 接口（非 Gate-B 阻断 handoff）

本节是未来 Gate C 合同的 candidate interface，不属于 Gate B schema bundle、required contract-ID set或approval blocker。未来 Gate C 使用独立签名产品；建议最低 schema 为：

```text
gate_c_schema:chr!, estimand_id:chr!, index_product_kind:chr!,
index_track:chr!, index_domain:chr!, index_class:chr!,
reference_domain:chr!, reference_class:chr!, target_population_id:chr!,
sampling_unit:chr!, eligible_edge_rule_registry_id:chr!,
matching_objective:chr!, matching_threshold_registry_id:chr!,
primary_metric:chr!, secondary_metrics_json:json!, denominator_domain:chr!,
aggregation_rule:chr!, uncertain_policy:chr!, unscorable_policy:chr!,
algorithm_abstention_policy:chr!, cluster_level:chr!, ci_method:chr!,
split_error_rule:chr!, merge_error_rule:chr!, sample_stratum:chr!,
preregistered_before_validation:lgl!, estimand_payload_sha256:chr!
```

至少有独立 estimands：`burst_event_base`、每个 State episode、每个 State direct support、connector edge、predicted Pause、pause-interrupted compound link、Event–State relation、extent modifier、frequency modifier和abstention coverage。每个 registry 明确唯一 primary endpoint；多个 IoU/容差只作预注册 secondary，不得择优报告。patient group可用时 `cluster_level=patient`，不得降级 session；独立 cluster不足返回 `not_estimable`。

Gate C 还必须独立材料化 reference eligibility，而不是改写 Gate B `reference_standard_status`：

```text
gate_c_schema:chr!, eligibility_id:chr!, annotation_project_id:chr!,
estimand_id:chr!, patient_group_id_hash:chr?, session_group_id_hash:chr!,
index_detection_root_id:chr!, index_product_hash:chr!,
reference_manifest_hash:chr!, display_protocol_id:chr!,
context_completeness:chr!, group_disjoint_verified:lgl!,
blinding_verified:lgl!, waveform_qc_requirement:chr!,
eligibility_status:chr!, reason_registry_ids_json:json!,
eligibility_payload_sha256:chr!
```

`eligibility_status=eligible|not_estimable|workflow_agreement_only`；reason array sorted unique且逐项 FK。eligibility 产品与 estimand registry/reference manifest 一起由独立 Gate C attestation签名；Gate B builder、metadata和attestation不得读写或伪造该状态。

Gate C 接口 ID 也必须是可重建内容地址，而不是调用方自由字符串：

```text
target_population_id = "pop_" + H("stpd-target-population-v1\0" ||
  canonical signed target-population registry row)
estimand_id = "ge_" + H("stpd-gate-c-estimand-v1\0" ||
  canonical estimand row excluding estimand_id/estimand_payload_sha256)
eligibility_id = "el_" + H("stpd-gate-c-eligibility-v1\0" ||
  canonical eligibility row excluding eligibility_id/eligibility_payload_sha256)
```

两个 payload hash 分别使用独立 `stpd-gate-c-*-payload-v1` domain 对相同排除列 payload 计算。target-population registry 至少固定物种、脑区、纳入/排除、patient/session sampling frame、时间范围与 context-completeness 下限；其 byte 与 Gate C approval/attestation 一并冻结。

## 15. Migration

| 来源 | v3 AUTO | v3 FINAL | 处理 |
|---|---|---|---|
| raw + exact params/code/threshold/QC | 可重检 | 可在新 AUTO 上复核 | 正常路径 |
| v2 product only | 不可 | 不可 | `requires_redetection` |
| v2 + raw，但代码/阈值不一致 | 仅新 run 重检 | 旧 history archive | 不迁移身份 |
| v1/v2 review transition | 仅当 source hash、train、geometry、target/evidence 一一等价 | 可 import transition | 否则 `requires_readjudication` |
| 已篡改/缺 parent | 不可 | 不可 | `failed_closed` |

禁止根据 v2 envelope 事后猜 connector，禁止把旧 ID/hash “升级”成 v3。

`migration_records` 精确表：

```text
migration_schema:chr!, migration_id:chr!, source_schema:chr!,
source_product_hash:chr!, target_detection_root_id:chr!,
source_entity_domain_id:chr?, source_entity_id:chr?,
target_entity_domain_id:chr?, target_entity_id:chr?, mapping_status:chr!,
reason_registry_id:chr!, evidence_id:chr!,
redetected_from_v2_product_hash:chr?
```

`mapping_status=redetected_new_identity|transition_imported_exact|archive_only|requires_readjudication|requires_redetection|rejected_tampered`；PK `migration_id`；unique `(source_product_hash,source_entity_domain_id,source_entity_id,target_detection_root_id,target_entity_domain_id,target_entity_id,mapping_status)`；sort by该 key。nullable domain/id 必须成对同为 NA 或同为非 NA；非 NA domain ID逐项解析对应 entity。任何 imported transition 必须有 source→target rows及跨产品 lineage；无 exact proof 时不得出现 target entity ID。

## 16. 稳定合同 ID 与最低证据

| contract_id | 内容 | 最低证据 |
|---|---|---|
| `GB3-SCI-001` | 完整 3×3 State 映射与弃权 | 9 cells + nonfinite/gray fixtures |
| `GB3-SCI-002` | Pause 独立且 hard boundary | below/equal/above connector threshold fixtures |
| `GB3-SCI-003` | connector 非递归、两侧先独立过门 | chain/adversarial fixtures |
| `GB3-SCI-004` | Burst–HF-irregular 非破坏共存 | overlap + modifier no-double-count fixture |
| `GB3-GEO-001` | ISI/time 公式与 parent recompute | endpoint/nondefault rowname/timestamp fixtures |
| `GB3-GEO-002` | episode–segment partition | overlap/gap/nonmaximal/orphan attacks |
| `GB3-GEO-003` | per-ISI episode 与 active support 分离 | connector active fields null fixture |
| `GB3-REL-001` | Event–State expected-set closure | delete/add/reseal attacks |
| `GB3-REL-002` | episode link adjacency/no transitivity | graph closure attacks |
| `GB3-ID-001` | deterministic full-length IDs/collision fail | reorder/locale/collision fixtures |
| `GB3-HASH-001` | canonical serialization/hash DAG | cross-process/platform golden bytes |
| `GB3-BLIND-001` | AUTO label/cache blindness | forbidden-column/attribute metamorphic suite |
| `GB3-THR-001` | threshold source/group overlap provenance | held-out/transductive fixtures |
| `GB3-FINAL-001` | append-only CAS replay/compensation与schema-version action guard | stale/fork/revoke/tamper、review Event×HFT/tonic及interrupted-Event reject fixtures |
| `GB3-STATUS-001` | product presence/fail-closed matrix | every status × accessor/export fixture |
| `GB3-ATT-001` | external signed attestation | missing/wrong key/changed byte/replay attacks |
| `GB3-EXP-001` | generation atomic commit | crash point, lock, symlink, stale-file suite |
| `GB3-MIG-001` | v2 requires redetection | all migration matrix rows |
| `GB3-GUARD-001` | Gate C estimand guard | API/UI/export/batch/report scans + runtime tests |
| `GB3-CAND-001` | State/Event/Gap 完整 candidate universe | negative/abstain row delete-add-reseal attacks |
| `GB3-REG-001` | registry/domain/FK 唯一解析 | unknown domain、wrong-domain ID、semantic hash attacks |
| `GB3-CTX-001` | scientific context 与 hard epoch | missing/minimal/epoch-crossing fixtures |
| `GB3-REF-001` | 独立 reference、盲法与 reference manifest | blind-flag/display/repeat/adjudication/manifest attacks |
| `GB3-EVT-001` | Event maximal/disjoint 与正交 modifiers | overlap/nesting/one-axis-unresolved fixtures |
| `GB3-RES-001` | 线性材料化与资源 fail-closed | large/all-hit/edge-growth/cancel fixtures |
| `GB3-PRIV-001` | 可移植审计脱敏 | path/user/host/patient/oversize payload attacks |
| `GB3-CONS-001` | version-negotiated consumer/no fallback | version×status×access matrix |

每个 ID 必须在签名 evidence bundle 中绑定精确 fixture、test name、source/test/contract/schema hashes 与 actual result。布尔常量、函数存在性或字符串存在性不能作为通过证据。

## 17. 冻结条件

本合同正文永久保持 `candidate_contract_v1`，审后不得为改状态而修改其 byte。只有在以下条件同时满足后，外部 `gate_b_approval_manifest` 才可授予 `approved_stage=implementation`：

1. 神经生理/统计审查 P0/P1 为 0；
2. 软件架构/可复现性审查 P0/P1 为 0；
3. 两位审查者基于同一最终 SHA，且互不读取对方复审；
4. 主责人发布逐条 review resolution；
5. schema 原型、canonical serializer 试验和状态矩阵可执行，无未定义 enum/null；
6. 阶段 0 安全止血可以独立实施，不会误升格现有 v2。

Gate B 最终发布另需新的 approval manifest：两位审查者在最终源码/测试/合同/fixture bytes 上给出 `approve_for_gate_b_release`，主责 resolution 无未关闭 P0/P1，release attestation 与 product anchor 均验证通过。任何被绑定 byte 改变都使 approval 失效，但不回写本合同正文。
