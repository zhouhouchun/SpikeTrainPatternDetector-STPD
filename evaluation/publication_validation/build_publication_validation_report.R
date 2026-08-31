#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Unable to resolve the report script path.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(
  file.path(dirname(script_path), "..", ".."), mustWork = TRUE
)
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
source(file.path(
  repo, "evaluation", "publication_validation",
  "publication_validation_helpers.R"
), local = FALSE)
root <- path.expand(Sys.getenv(
  "STPD_VALIDATION_OUTPUT_ROOT",
  unset = file.path(repo, "test-results", "publication_validation")
))
out_dir <- file.path(root, "report")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

resolve_simulation_result <- function(env_var) {
  path <- stpd_pub_resolve_frozen_result_dir(
    env_var = env_var,
    required_files = c(
      "pooled_observed_metrics.csv", "publication_validation_result.rds"
    ),
    configured_path = Sys.getenv(env_var, unset = "")
  )
  if (!any(file.exists(file.path(
      path, c("cluster_bootstrap_95ci.csv", "group_cluster_bootstrap_95ci.csv")
  )))) {
    stop(
      "Frozen simulation result from ", env_var,
      " lacks a cluster-bootstrap 95% CI table.", call. = FALSE
    )
  }
  path
}

simulation_short_dir <- resolve_simulation_result(
  "STPD_SIM_SHORT_VALIDATION_DIR"
)
simulation_medium_dir <- resolve_simulation_result(
  "STPD_SIM_MEDIUM_VALIDATION_DIR"
)
simulation_long_dir <- resolve_simulation_result(
  "STPD_SIM_LONG_VALIDATION_DIR"
)
real_bundle <- stpd_pub_validate_real_result_bundle(
  configured_path = Sys.getenv("STPD_REAL_VALIDATION_DIR", unset = "")
)
real_result_dir <- real_bundle$path
real_patient_n <- as.integer(real_bundle$quality_value("patient_n", TRUE))
real_hemisphere_n <- as.integer(real_bundle$quality_value("hemisphere_n", TRUE))
real_group_n <- as.integer(real_bundle$quality_value("recording_group_n", TRUE))
real_train_n <- as.integer(real_bundle$quality_value("train_n", TRUE))
real_isi_n <- as.integer(real_bundle$quality_value("isi_n", TRUE))
real_manual_labeled_n <- as.integer(
  real_bundle$quality_value("manual_labeled_n", TRUE)
)
real_blank_as_other_n <- as.integer(
  real_bundle$quality_value("blank_as_other_n", TRUE)
)
real_multi_axis_n <- as.integer(
  real_bundle$quality_value("multi_axis_interval_n", TRUE)
)
real_state_eligible_n <- as.integer(
  real_bundle$quality_value("state_reference_eligible_train_n", TRUE)
)
real_bootstrap_n <- as.integer(real_bundle$quality_value("bootstrap_n", TRUE))
real_reference_train_n <- nrow(real_bundle$reference_eligibility)
real_excluded_train_n <- sum(
  !as.logical(real_bundle$reference_eligibility$reference_eligible), na.rm = TRUE
)
tonic_summary <- real_bundle$tonic_summary[1L, , drop = FALSE]
broad_hfs_event_n <- unique(real_bundle$confirmatory_event$truth_n[
  real_bundle$confirmatory_event$pattern == "broad_hfs"
])
if (length(broad_hfs_event_n) != 1L || !is.finite(broad_hfs_event_n)) {
  stop("Frozen real Broad-HFS event support is inconsistent.", call. = FALSE)
}

read_result <- function(path, cohort, source_id, cluster_n, design,
                        include_tonic = FALSE) {
  observed <- read.csv(file.path(path, "pooled_observed_metrics.csv"), stringsAsFactors = FALSE)
  ci_file <- if (file.exists(file.path(path, "cluster_bootstrap_95ci.csv")))
    "cluster_bootstrap_95ci.csv" else "group_cluster_bootstrap_95ci.csv"
  ci <- read.csv(file.path(path, ci_file), stringsAsFactors = FALSE)
  targets <- rbind(
    data.frame(axis = "event", label = c("burst", "pause")),
    data.frame(axis = "state_hf_family", label = c("high_frequency_spiking", "tonic")))
  if (!include_tonic) {
    targets <- targets[targets$label != "tonic", , drop = FALSE]
  }
  rows <- lapply(seq_len(nrow(targets)), function(ii) {
    a <- targets$axis[ii]; l <- targets$label[ii]
    o <- observed[observed$level == "event" & observed$axis == a & observed$label == l, ]
    q <- ci[ci$level == "event" & ci$axis == a & ci$label == l & ci$metric == "F1", ]
    if (!nrow(o)) return(NULL)
    data.frame(cohort = cohort, pattern = l, axis = a, level = "event",
      f1 = o$F1[1], ci_low = q$ci_low[1], ci_high = q$ci_high[1],
      precision = o$precision[1], recall = o$recall[1], support = o$support[1],
      tp = o$tp[1], fp = o$fp[1], fn = o$fn[1], cluster_n = cluster_n,
      validation_design = design, source_id = source_id, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

performance <- rbind(
  read_result(simulation_short_dir, "模拟：短尺度", "simulation_short", 20,
              "20次10/10 train重复留出；确认性Tonic待eligible_primary重算"),
  read_result(simulation_medium_dir, "模拟：中尺度", "simulation_medium", 20,
              "20次10/10 train重复留出；确认性Tonic待eligible_primary重算"),
  read_result(simulation_long_dir, "模拟：长尺度", "simulation_long", 20,
              "20次10/10 train重复留出；确认性Tonic待eligible_primary重算"),
  read_result(real_result_dir,
              "真实数据", "real_validation", real_group_n,
              paste0(
                "单患者双侧STN；", real_excluded_train_n,
                "条非参考合格train排除；", real_group_n,
                "个记录组leave-one-group-out；真实Tonic仅作tonic-like描述性复核"
              ),
              include_tonic = FALSE))
performance$pattern_label <- c(burst = "Burst", pause = "Pause",
  high_frequency_spiking = "Broad HFS (HFI ∪ HFT)",
  tonic = "Tonic")[performance$pattern]
performance$cohort_pattern <- paste(performance$cohort, performance$pattern_label, sep = " · ")

real_rows <- performance[performance$cohort == "真实数据", ]
headline <- as.list(stats::setNames(real_rows$f1, paste0(real_rows$pattern, "_f1")))
headline$patient_n <- real_patient_n
headline$hemisphere_n <- real_hemisphere_n
headline$recording_group_n <- real_group_n
headline$train_n <- real_train_n
headline$isi_n <- real_isi_n
headline <- as.data.frame(headline, stringsAsFactors = FALSE)

quality <- data.frame(
  analysis = c("模拟短尺度", "模拟中尺度", "模拟长尺度", "真实数据"),
  resampling_cluster_n = c(20, 20, 20, real_group_n),
  biological_subject_n = c(NA, NA, NA, real_patient_n),
  hemisphere_n = c(NA, NA, NA, real_hemisphere_n),
  repeated_split_n = c(20, 20, 20, real_group_n),
  calibration_validation_overlap_n = c(0, 0, 0, 0),
  examples_per_pattern = c(
    "10/次", "10/次", "10/次",
    "10/折 × Burst/Pause/BroadHFS/Other；真实Tonic=0"
  ),
  bootstrap_n = c(1000, 1000, 1000, real_bootstrap_n),
  label_blind = c(TRUE, TRUE, TRUE, TRUE),
  reference_status = c("冻结模拟真值", "冻结模拟真值", "冻结模拟真值",
                       "人工草稿，identity-bound，未封存"),
  stringsAsFactors = FALSE)

sensitivity <- read.csv(file.path(real_result_dir,
  "explicit_manual_positive_recall_sensitivity.csv"), stringsAsFactors = FALSE)
sensitivity <- aggregate(cbind(explicit_positive_n, detected_n) ~ axis + label,
                         sensitivity, sum)
sensitivity$recall <- sensitivity$detected_n / sensitivity$explicit_positive_n
sensitivity <- sensitivity[sensitivity$label %in% c("burst", "pause",
  "high_frequency_spiking", "high_frequency_spiking+burst", "high_frequency_spiking+pause"), ]

# Ontology-aligned real primary estimand.  The detector predictions and outer
# folds remain frozen; only the prespecified reporting family is recomputed.
real_draws <- readRDS(file.path(real_result_dir,
  "group_cluster_bootstrap_draws.rds"))
primary_hit <- real_draws$level == "event" & (
  (real_draws$axis == "event" & real_draws$label %in% c("burst", "pause")) |
  (real_draws$axis == "state_hf_family" &
     real_draws$label == "high_frequency_spiking"))
primary_draws <- real_draws[primary_hit,
  c("bootstrap_replicate", "axis", "label", "F1"), drop = FALSE]
primary_wide <- reshape(primary_draws, idvar = "bootstrap_replicate",
                        timevar = "label", direction = "wide")
primary_f1_columns <- grep("^F1\\.", names(primary_wide), value = TRUE)
primary_wide$macro_f1 <- rowMeans(primary_wide[primary_f1_columns], na.rm = FALSE)
primary_macro <- data.frame(
  estimand = "macro_F1_Burst_Pause_BroadHFS",
  estimate = mean(real_rows$f1),
  ci_low = as.numeric(stats::quantile(primary_wide$macro_f1, 0.025, na.rm = TRUE)),
  ci_high = as.numeric(stats::quantile(primary_wide$macro_f1, 0.975, na.rm = TRUE)),
  bootstrap_valid_n = sum(is.finite(primary_wide$macro_f1)),
  bootstrap_requested_n = real_bootstrap_n,
  note = paste0(
    "Real tonic-like fragments and ", real_excluded_train_n,
    " non-reference-eligible trains are excluded from the primary real-data estimand"
  ),
  stringsAsFactors = FALSE)

endpoint_scope <- data.frame(
  manual_label = c("burst", "pause", "high_frequency_spiking", "tonic"),
  reporting_role = c("primary", "primary", "primary_broad_family",
                     "descriptive_review_only_not_state_endpoint"),
  detector_comparator = c("burst", "pause",
    "high_frequency_irregular_state OR high_frequency_tonic",
    "not used as a confirmatory real-data endpoint"),
  rationale = c(
    "prespecified Event endpoint",
    "prespecified train-adaptive Gap endpoint",
    "manual HFS denotes broad high-frequency State",
    paste(
      "short tonic-like fragments lack a sustained Tonic-State phenotype",
      "and are retained only for descriptive review"
    )),
  stringsAsFactors = FALSE)

fmt <- function(x, digits = 3) ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "NA")
real_line <- function(label) {
  z <- real_rows[real_rows$pattern == label, ]
  sprintf("%s %.3f（95%%记录组重采样区间 %.3f–%.3f）",
          z$pattern_label, z$f1, z$ci_low, z$ci_high)
}

title <- "STPD重复划分、置信区间与单患者双侧STN内部验证"
summary_text <- paste0(
  "## 技术摘要\n\n",
  "**模拟验证支持参数学习方案，但真实验证只支持部分模式。** 三个时间尺度分别完成20次独立10/10 train重复留出；",
  "真实数据来自", real_patient_n, "位患者的", real_hemisphere_n,
  "侧STN；", real_excluded_train_n,
  "条非参考合格train不进入参数学习和评分，剩余", real_train_n,
  "条train按", real_group_n,
  "个记录位点/深度组完成leave-one-recording-group-out。所有验证检测均在移除held-out人工标签后运行。\n\n",
  "- 模拟中尺度的Burst/Pause/Broad HFS事件F1分别为 ",
  paste(fmt(performance$f1[performance$cohort == "模拟：中尺度"]), collapse = ", "), "。\n",
  "- 确认性Tonic仅允许使用Stage-C `phenotype / eligible_primary`估计量；该冻结结果尚未重算，因此本报告不展示旧strict-mechanism Tonic F1。\n",
  "- 真实数据主要事件/State终点为：", real_line("burst"), "；", real_line("pause"), "；",
  real_line("high_frequency_spiking"), "。Tonic不进入主要真实终点。\n",
  "- 三个主要真实终点的macro-F1为 ", fmt(primary_macro$estimate),
  "（记录组cluster-bootstrap描述性区间 ", fmt(primary_macro$ci_low), "–",
  fmt(primary_macro$ci_high), "；不能解释为患者总体95%置信区间）。\n",
  "- 因此目前可支持“STPD是可校准的候选事件生成器”以及“在该患者内跨记录组具有一定迁移能力”的定位；不能支持跨患者泛化或真实数据已全面验证的表述。"
)

findings_text <- paste0(
  "## 模拟改善没有完全转化为真实State泛化\n\n",
  "下图比较同一套人工示例导入规则在三个模拟时间尺度和单患者内部留出记录组上的事件级F1。",
  "模拟数据中Pause与Broad HFS总体稳定；short-scale Burst较弱，说明单次划分曾高估其泛化。",
  "真实数据中的短tonic-like片段因缺乏持续Tonic-State表型而不属于正式评价对象。Broad HFS仍需按direct support与parent episode分别报告，",
  "这一区别要求论文把模拟验证与真实验证分开陈述。"
)

definitions_text <- paste0(
  "## 评价对象、分母与匹配规则\n\n",
  "- **事件级F1**：在同一train和同一模式内进行一对一最大基数匹配，再最大化总IoU；IoU阈值为0.25。\n",
  "- **State与Event正交**：Burst/Pause在Event/Gap轴，Broad HFS在State轴；",
  real_multi_axis_n, "个真实ISI同时具有State与Event标签，未压平成单标签。\n",
  "- **Broad HFS定义**：人工HFS是广义高频状态；主要比较对象是检测器的HF-irregular与HFT并集。亚型没有独立人工真值，因此不分别计算确认性F1。\n",
  "- **参考资格规则**：共", real_reference_train_n, "条train接受逐轴资格检查；",
  real_excluded_train_n, "条非参考合格train完全排除，避免结果导向筛选。\n",
  "- **空白规则**：在", real_train_n, "条参考合格train内，",
  format(real_blank_as_other_n, big.mark = ",", scientific = FALSE),
  "个空白ISI按预注册决定作为others。由于空白不等同于独立阴性复核，敏感性分析只能可靠报告明确阳性的召回，不能据此声称特异度。\n",
  "- **不确定性区间**：模拟按train进行percentile cluster bootstrap；真实数据按",
  real_group_n, "个可评分记录位点/深度组重采样。由于只有", real_patient_n,
  "位患者和", real_hemisphere_n,
  "个半球，真实数据区间仅描述该患者内部记录组变异，不能作为患者总体置信区间。"
)

methods_text <- paste0(
  "## 重复划分与单患者内部验证设计\n\n",
  "### 模拟数据\n",
  "短、中、长三个尺度完全分开运行。每个尺度20条train，每次随机选10条校准、10条验证，共20次。",
  "校准侧按train均衡抽取Burst、Pause、Tonic、HFS和Other各10个连续片段；验证侧人工标签完全清空。\n\n",
  "### 真实数据\n",
  "数据来自", real_patient_n, "位患者的", real_hemisphere_n,
  "侧STN。原始", real_reference_train_n, "条train中，",
  real_excluded_train_n, "条非参考合格train被排除；主要分析含", real_train_n,
  "条train、", real_group_n,
  "个记录位点/深度组，每折完整留出一个记录组。真实tonic-like片段不进入校准抽样。",
  "原始CSV与工作簿在纳入范围内的", format(real_isi_n, big.mark = ",", scientific = FALSE),
  "个ISI左右时间戳逐点完全一致。HFS bridge被限制在独立学习的Pause下界，",
  "从而保证canonical Pause切断连续State support。\n\n",
  "### 不确定性\n",
  "模拟每个bootstrap replicate先按train有放回抽样，再为每条train随机选择一次其held-out预测，",
  "同时传播train异质性和校准划分不确定性。真实数据按", real_group_n,
  "个可评分记录组有放回重采样，共", format(real_bootstrap_n, big.mark = ","),
  "次；该区间不包含患者间变异。"
)

limitations_text <- paste0(
  "## 结论受人工草稿与State稀疏性限制\n\n",
  "1. 真实参考的authority状态是 `identity_bound_manual_draft_unsealed`，属于独立人工参考草稿，不是biological ground truth。\n",
  "2. 全部真实记录来自同", real_patient_n, "位患者；左右STN也只有",
  real_hemisphere_n, "个半球，不能估计患者间变异或声称外部/跨患者验证。\n",
  "3. 真实参考中的tonic标签共", tonic_summary$interval_n,
  "个ISI、", tonic_summary$episode_n, "个短片段（中位",
  fmt(as.numeric(tonic_summary$median_episode_isi_n), 0), "个ISI、",
  fmt(as.numeric(tonic_summary$median_episode_duration_sec), 4),
  " s），不能代表典型持续Tonic State；因此仅作为tonic-like描述性复核，不计算确认性F1，也不参与参数校准。\n",
  "4. Broad HFS参考事件数为", as.integer(broad_hfs_event_n),
  "；其事件级不确定性必须按记录组报告。\n",
  "5. 空白=others是用户预设规则，不等于这些ISI均经过阴性复核；主分析的精度/特异度可能偏乐观或偏悲观，方向取决于漏标结构。\n",
  "6. 多轨输出当前仍标记Gate-B pending、`authoritative=FALSE`。本报告将其作为正交候选产品进行技术验证，不将其称为正式生物学分类。\n",
  "7. 这是单患者内部预测/诊断验证，不是跨患者验证或因果实验。"
)

next_text <- paste0(
  "## 下一步应优先修复真实State，而不是继续调高模拟分数\n\n",
  "- 由两名独立专家封存Broad HFS/Pause/Burst参考，并显式增加reviewed-other阴性样本。\n",
  "- 对Broad HFS实施按组校准的frequency×regularity双轴规则；HF-irregular/HFT只作内部亚型输出。\n",
  "- 保持Burst结构对比度预检测和Pause train-adaptive规则；开发期仍可使用相同",
  real_group_n, "个参考合格组的外层验证且禁止在held-out组上调参。\n",
  "- 发表级外部验证必须新增多位患者，并按患者整体留出；同一患者左右STN和所有记录组不得跨校准/验证集合。"
)

questions_text <- paste0(
  "## 仍需回答的问题\n\n",
  "- 参考合格train中的3,253个空白ISI有多少是真正others，有多少只是尚未复核？\n",
  "- 若未来评价Tonic/HFT亚型，是否能补充独立、按亚型标注的参考集？\n",
  "- HFS+Pause只有2个共存ISI，是否足以作为模型评价目标，还是仅作为challenge case？"
)

report_md <- paste(
  paste0("# ", title), summary_text, findings_text, definitions_text, methods_text,
  limitations_text, next_text, questions_text, sep = "\n\n")
writeLines(report_md, file.path(out_dir, "publication_validation_report.md"), useBytes = TRUE)
write.csv(performance, file.path(out_dir, "publication_performance_table.csv"), row.names = FALSE)
write.csv(quality, file.path(out_dir, "publication_protocol_table.csv"), row.names = FALSE)
write.csv(sensitivity, file.path(out_dir, "real_explicit_positive_sensitivity.csv"), row.names = FALSE)
write.csv(primary_macro, file.path(out_dir, "real_primary_macro_f1.csv"), row.names = FALSE)
write.csv(endpoint_scope, file.path(out_dir, "real_endpoint_scope.csv"), row.names = FALSE)

generated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
sources <- list(
  list(id = "validation_report", label = "Combined publication validation table",
       path = normalizePath(file.path(
         out_dir, "publication_performance_table.csv"
       ), mustWork = TRUE)),
  list(id = "simulation_short", label = "Short-scale repeated simulation validation",
       path = file.path(simulation_short_dir, if (file.exists(file.path(
         simulation_short_dir, "cluster_bootstrap_95ci.csv"
       ))) "cluster_bootstrap_95ci.csv" else "group_cluster_bootstrap_95ci.csv")),
  list(id = "simulation_medium", label = "Medium-scale repeated simulation validation",
       path = file.path(simulation_medium_dir, if (file.exists(file.path(
         simulation_medium_dir, "cluster_bootstrap_95ci.csv"
       ))) "cluster_bootstrap_95ci.csv" else "group_cluster_bootstrap_95ci.csv")),
  list(id = "simulation_long", label = "Long-scale repeated simulation validation",
       path = file.path(simulation_long_dir, if (file.exists(file.path(
         simulation_long_dir, "cluster_bootstrap_95ci.csv"
       ))) "cluster_bootstrap_95ci.csv" else "group_cluster_bootstrap_95ci.csv")),
  list(id = "real_validation", label = "Grechishnikova 2017 within-patient leave-one-recording-group-out validation",
       path = file.path(real_result_dir, "group_cluster_bootstrap_95ci.csv")),
  list(id = "real_reference", label = "Grechishnikova 2017 manual ISI reference draft",
       path = normalizePath(file.path(
         repo, "PD_STN",
         "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx"
       ), mustWork = TRUE))
)
top_sources <- lapply(sources, function(s) {
  query <- list(engine = "duckdb", language = "SQL",
    description = paste("Reproducible validation output from", s$label),
    tables_used = list(s$path), executed_at = generated)
  if (grepl("\\.csv$", s$path, ignore.case = TRUE)) {
    escaped_path <- gsub("'", "''", s$path, fixed = TRUE)
    query$sql <- paste0("SELECT * FROM read_csv_auto('", escaped_path, "')")
  }
  list(id = s$id, query = query)
})

real_source <- "real_validation"
cards <- lapply(seq_len(nrow(real_rows)), function(ii) {
  z <- real_rows[ii, ]
  list(id = paste0("real_", z$pattern), dataset = "real_headline", sourceId = real_source,
    description = paste(
      z$pattern_label, "event-level F1 in one-patient,",
      paste0(real_group_n, "-recording-group internal validation.")
    ),
    metrics = list(list(label = paste(z$pattern_label, "F1"),
      field = paste0(z$pattern, "_f1"), format = "percent")))
})

artifact <- list(
  surface = "report",
  manifest = list(version = 1, surface = "report", title = title,
    description = "Repeated simulation splits and within-patient bilateral-STN internal validation.",
    generatedAt = generated, filters = list(), cards = cards,
    charts = list(list(id = "event_f1_chart", title = "事件级F1：模拟三尺度与单患者内部验证",
      subtitle = "点估计；重采样区间见下表（真实数据区间不代表患者总体）", type = "bar",
      dataset = "performance", sourceId = "validation_report", valueFormat = "percent",
      encodings = list(
        x = list(field = "cohort", type = "nominal", label = "验证数据"),
        y = list(field = "f1", type = "quantitative", label = "事件级F1", format = "percent"),
        color = list(field = "pattern_label", type = "nominal", label = "模式"),
        tooltip = list(
          list(field = "precision", type = "quantitative", label = "Precision", format = "percent"),
          list(field = "recall", type = "quantitative", label = "Recall", format = "percent"),
          list(field = "ci_low", type = "quantitative", label = "95% resampling lower", format = "percent"),
          list(field = "ci_high", type = "quantitative", label = "95% resampling upper", format = "percent"),
          list(field = "support", type = "quantitative", label = "Truth events", format = "number"))),
      palette = list(kind = "categorical"), legend = list(show = TRUE))),
    tables = list(
      list(id = "performance_table", title = "事件级性能与95%重采样区间",
        subtitle = "真实数据按记录组bootstrap；仅描述单患者内部变异", dataset = "performance",
        sourceId = "validation_report", density = "dense", columns = list(
          list(field = "cohort", label = "数据", type = "text"),
          list(field = "pattern_label", label = "模式", type = "text"),
          list(field = "f1", label = "F1", format = "percent"),
          list(field = "ci_low", label = "95%区间下界", format = "percent"),
          list(field = "ci_high", label = "95%区间上界", format = "percent"),
          list(field = "precision", label = "Precision", format = "percent"),
          list(field = "recall", label = "Recall", format = "percent"),
          list(field = "support", label = "Truth events", format = "number"),
          list(field = "cluster_n", label = "重采样组", format = "number"))),
      list(id = "quality_table", title = "验证协议与质量门禁",
        subtitle = "划分、标签盲化、bootstrap和参考状态", dataset = "quality",
        sourceId = "validation_report", density = "dense", columns = list(
          list(field = "analysis", label = "分析", type = "text"),
          list(field = "resampling_cluster_n", label = "重采样组", format = "number"),
          list(field = "biological_subject_n", label = "患者数", format = "number"),
          list(field = "hemisphere_n", label = "STN半球数", format = "number"),
          list(field = "repeated_split_n", label = "划分/折数", format = "number"),
          list(field = "calibration_validation_overlap_n", label = "组重叠", format = "number"),
          list(field = "examples_per_pattern", label = "每类示例", type = "text"),
          list(field = "bootstrap_n", label = "Bootstrap", format = "number"),
          list(field = "label_blind", label = "Label-blind", type = "text"),
          list(field = "reference_status", label = "参考状态", type = "text")))),
    sources = sources,
    blocks = list(
      list(id = "title", type = "markdown", body = paste0("# ", title)),
      list(id = "summary", type = "markdown", body = summary_text),
      list(id = "real_metrics", type = "metric-strip", cardIds = vapply(cards, `[[`, "", "id")),
      list(id = "findings", type = "markdown", body = findings_text),
      list(id = "performance_chart", type = "chart", chartId = "event_f1_chart", layout = "full"),
      list(id = "performance_evidence", type = "table", tableId = "performance_table", layout = "full"),
      list(id = "definitions", type = "markdown", body = definitions_text),
      list(id = "methods", type = "markdown", body = methods_text),
      list(id = "quality_evidence", type = "table", tableId = "quality_table", layout = "full"),
      list(id = "limitations", type = "markdown", body = limitations_text),
      list(id = "next", type = "markdown", body = next_text),
      list(id = "questions", type = "markdown", body = questions_text))),
  snapshot = list(version = 1, generatedAt = generated, status = "ready",
    datasets = list(
      performance = lapply(seq_len(nrow(performance)), function(i) as.list(performance[i, ])),
      real_headline = lapply(seq_len(nrow(headline)), function(i) as.list(headline[i, ])),
      quality = lapply(seq_len(nrow(quality)), function(i) as.list(quality[i, ]))),
    accessIssues = list()),
  sources = top_sources,
  package_info = list(originUrl = "artifact://stpd-publication-validation",
                      controls = list(edit = FALSE, refresh = FALSE)))

jsonlite::write_json(artifact, file.path(out_dir, "artifact.json"), auto_unbox = TRUE,
                     pretty = TRUE, na = "null", null = "null", digits = 16)
cat("Wrote publication report inputs to", out_dir, "\n")
