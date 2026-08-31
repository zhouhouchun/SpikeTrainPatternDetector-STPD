relation_validator <- function() {
  getFromNamespace("stpd_validate_param_relations", "SpikeTrainPatternDetector")
}

relation_codes <- function(params, severity = NULL) {
  issues <- relation_validator()(params)
  if (!is.null(severity)) issues <- issues[issues$severity %in% severity, , drop = FALSE]
  as.character(issues$issue_code)
}

test_that("canonical defaults satisfy all cross-parameter relations", {
  issues <- relation_validator()(default_params_sec())

  expect_s3_class(issues, "data.frame")
  expect_true(all(c("severity", "path", "issue", "issue_code", "args") %in% names(issues)))
  expect_equal(nrow(issues), 0L)
  public_issues <- stpd_validate_params(default_params_sec())
  expect_identical(names(public_issues), c("severity", "path", "issue"))
  expect_false(any(public_issues$severity == "error"))
})

test_that("canonical burst, tonic, pause, and HFS chains are checked table-wise", {
  cases <- list(
    list(
      code = "burst_seed_lower_not_below_upper",
      mutate = function(p) { p$spiketrainpattern$burst$seed_lower_sec <- p$spiketrainpattern$burst$seed_upper_sec; p }
    ),
    list(
      code = "burst_seed_upper_above_bridge",
      mutate = function(p) { p$spiketrainpattern$burst$bridge_upper_sec <- 0.009; p }
    ),
    list(
      code = "possible_burst_contrast_above_canonical",
      mutate = function(p) { p$spiketrainpattern$burst$possible_contrast_min <- p$spiketrainpattern$burst$contrast_min + 0.1; p }
    ),
    list(
      code = "burst_classic_min_above_max",
      mutate = function(p) { p$spiketrainpattern$burst$classic_min_spikes <- p$spiketrainpattern$burst$classic_max_spikes + 1L; p }
    ),
    list(
      code = "burst_classic_max_not_below_long_min",
      mutate = function(p) { p$spiketrainpattern$burst$long_min_spikes <- p$spiketrainpattern$burst$classic_max_spikes; p }
    ),
    list(
      code = "burst_long_min_above_max",
      mutate = function(p) { p$spiketrainpattern$burst$long_max_spikes <- p$spiketrainpattern$burst$long_min_spikes - 1L; p }
    ),
    list(
      code = "burst_long_max_not_below_prolonged_min",
      mutate = function(p) { p$spiketrainpattern$burst$long_max_spikes <- p$spiketrainpattern$burst$prolonged_min_spikes; p }
    ),
    list(
      code = "burst_prolonged_min_above_max",
      mutate = function(p) { p$spiketrainpattern$burst$prolonged_min_spikes <- p$spiketrainpattern$burst$prolonged_max_spikes + 1L; p }
    ),
    list(
      code = "tonic_min_not_below_max",
      mutate = function(p) { p$spiketrainpattern$tonic$min_isi_sec <- p$spiketrainpattern$tonic$max_isi_sec; p }
    ),
    list(
      code = "tonic_max_above_bridge",
      mutate = function(p) { p$spiketrainpattern$tonic$bridge_upper_sec <- p$spiketrainpattern$tonic$max_isi_sec - 0.001; p }
    ),
    list(
      code = "pause_min_above_max",
      mutate = function(p) { p$spiketrainpattern$pause$min_isi_sec <- p$spiketrainpattern$pause$max_isi_sec + 0.001; p }
    ),
    list(
      code = "pause_max_above_bridge",
      mutate = function(p) { p$spiketrainpattern$pause$bridge_upper_sec <- p$spiketrainpattern$pause$max_isi_sec - 0.001; p }
    ),
    list(
      code = "hfs_short_above_q90",
      mutate = function(p) { p$spiketrainpattern$high_frequency_spiking$short_isi_upper_sec <- p$spiketrainpattern$high_frequency_spiking$q90_isi_max_sec + 0.001; p }
    ),
    list(
      code = "hfs_q90_above_bridge",
      mutate = function(p) { p$spiketrainpattern$high_frequency_spiking$q90_isi_max_sec <- p$spiketrainpattern$high_frequency_spiking$epoch_bridge_isi_sec + 0.001; p }
    ),
    list(
      code = "hfs_bridge_above_tolerated_gap",
      mutate = function(p) { p$spiketrainpattern$high_frequency_spiking$epoch_bridge_isi_sec <- p$spiketrainpattern$high_frequency_spiking$tolerated_gap_isi_sec + 0.001; p }
    ),
    list(
      code = "pause_below_hfs_bridge",
      mutate = function(p) { p$spiketrainpattern$pause$min_isi_sec <- p$spiketrainpattern$high_frequency_spiking$epoch_bridge_isi_sec - 0.001; p }
    )
  )

  for (case in cases) {
    params <- case$mutate(default_params_sec())
    expected_severity <- if (identical(case$code, "pause_below_hfs_bridge")) {
      "warning"
    } else "error"
    expect_true(
      case$code %in% relation_codes(params, expected_severity),
      info = paste("missing relation code", case$code)
    )
  }
})

test_that("long_max zero remains an explicit warning-only compatibility sentinel", {
  params <- default_params_sec()
  params$spiketrainpattern$burst$long_max_spikes <- 0
  issues <- relation_validator()(params)

  sentinel <- issues[issues$issue_code == "burst_long_max_unbounded_sentinel", , drop = FALSE]
  expect_equal(nrow(sentinel), 1L)
  expect_identical(sentinel$severity, "warning")
  expect_false(any(issues$severity == "error"))

  params$spiketrainpattern$burst$long_max_spikes <- -1
  expect_false("burst_long_max_unbounded_sentinel" %in% relation_codes(params))
})

test_that("active HFS Max_ISI is a direct-support cap below the connector bridge", {
  disabled <- default_params_sec()
  disabled$detector$pattern_isi_limits$high_frequency_spiking$max_sec <- 0
  expect_false(any(grepl("^active_hfs_cap", relation_codes(disabled))))

  compatible <- default_params_sec()
  bridge <- compatible$spiketrainpattern$high_frequency_spiking$epoch_bridge_isi_sec
  compatible$detector$pattern_isi_limits$high_frequency_spiking$max_sec <- bridge * 0.8
  expect_false(any(grepl("^active_hfs_cap", relation_codes(compatible))))

  hard_conflict <- default_params_sec()
  hard_conflict$detector$pattern_isi_limits$high_frequency_spiking$max_sec <- bridge * 1.1
  expect_true("active_hfs_cap_above_bridge" %in%
    relation_codes(hard_conflict, "error"))
})

test_that("every enabled event-grammar user band and burst contrast is validated", {
  valid_band <- function(pattern) {
    band <- list(
      enable = TRUE,
      seed_lower_sec = 0.010,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.030
    )
    if (identical(pattern, "burst")) {
      band$contrast_S <- 2.5
      band$one_sided_contrast_S <- 3.0
    }
    band
  }

  patterns <- c("burst", "high_frequency_spiking", "high_frequency_tonic", "tonic", "pause")
  for (pattern in patterns) {
    params <- default_params_sec()
    params$event_grammar$user[[pattern]] <- valid_band(pattern)
    params$event_grammar$user[[pattern]]$seed_upper_sec <- 0.031
    expect_true(
      "event_grammar_user_seed_upper_above_bridge" %in% relation_codes(params, "error"),
      info = paste("enabled user band not checked for", pattern)
    )
  }

  invalid_values <- default_params_sec()
  invalid_values$event_grammar$user$tonic <- valid_band("tonic")
  invalid_values$event_grammar$user$tonic$seed_lower_sec <- NA_real_
  expect_true("event_grammar_user_band_value_invalid" %in% relation_codes(invalid_values, "error"))

  invalid_contrast <- default_params_sec()
  invalid_contrast$event_grammar$user$burst <- valid_band("burst")
  invalid_contrast$event_grammar$user$burst$contrast_S <- 0.9
  expect_true("event_grammar_user_burst_contrast_invalid" %in% relation_codes(invalid_contrast, "error"))

  below_possible <- default_params_sec()
  below_possible$event_grammar$user$burst <- valid_band("burst")
  below_possible$event_grammar$user$burst$contrast_S <- 1.5
  expect_true(
    "event_grammar_user_burst_contrast_below_possible" %in%
      relation_codes(below_possible, "error")
  )

  inactive_one_sided <- default_params_sec()
  inactive_one_sided$event_grammar$user$burst <- valid_band("burst")
  inactive_one_sided$event_grammar$user$burst$one_sided_contrast_S <- 0.5
  expect_false(any(relation_validator()(inactive_one_sided)$severity == "error"))
})

test_that("canonical product values are authoritative over conflicting legacy aliases", {
  legacy_only_conflict <- default_params_sec()
  legacy_only_conflict$event_core$seed_band_lower_sec <- 0.020
  legacy_only_conflict$event_core$seed_band_upper_sec <- 0.010
  expect_false("burst_seed_lower_not_below_upper" %in% relation_codes(legacy_only_conflict))

  canonical_conflict <- default_params_sec()
  canonical_conflict$spiketrainpattern$burst$seed_lower_sec <- 0.020
  canonical_conflict$spiketrainpattern$burst$seed_upper_sec <- 0.010
  canonical_conflict$event_core$seed_band_lower_sec <- 0.001
  canonical_conflict$event_core$seed_band_upper_sec <- 0.010
  expect_true("burst_seed_lower_not_below_upper" %in% relation_codes(canonical_conflict, "error"))
})

test_that("strict validation and the engine fail closed only for relational errors", {
  params <- default_params_sec()
  params$spiketrainpattern$burst$seed_lower_sec <- params$spiketrainpattern$burst$seed_upper_sec

  expect_error(
    stpd_validate_params(params, strict = TRUE),
    "burst seed lower bound must be strictly less"
  )

  prepare <- getFromNamespace("stpd_engine_prepare_params", "SpikeTrainPatternDetector")
  expect_error(
    prepare(params, strict = FALSE),
    "Cross-parameter validation failed:.*burst_seed_lower_not_below_upper"
  )

  ordinary_contract_error <- default_params_sec()
  ordinary_contract_error$pause$global_median_guard <- "TRUE"
  expect_no_error(prepare(ordinary_contract_error, strict = FALSE))
  expect_error(prepare(ordinary_contract_error, strict = TRUE), "type mismatch")
})

test_that("new relational issue prose localizes without changing English", {
  params <- default_params_sec()
  params$event_grammar$user$burst <- list(
    enable = TRUE,
    seed_lower_sec = 0.001,
    seed_upper_sec = 0.010,
    bridge_upper_sec = 0.015,
    contrast_S = 1.5
  )
  issues <- relation_validator()(params)
  issue_rows <- issues[, c("severity", "path", "issue"), drop = FALSE]

  zh <- stpd_ui_localize_parameter_issues(issue_rows, lang = "zh")
  en <- stpd_ui_localize_parameter_issues(issue_rows, lang = "en")
  expect_identical(en, issue_rows)
  expect_false(identical(zh$issue, issue_rows$issue))
  expect_true(all(grepl("[\u3400-\u9FFF]", zh$issue, perl = TRUE)))
  expect_identical(zh$path, issue_rows$path)
})

test_that("new parameter provenance and MANUAL-HF diagnostic copy has exact English", {
  source <- c(
    "旧版/诊断：经典 burst bridge 阈值",
    "旧版/诊断：经典 burst seed 阈值",
    "旧版/诊断：HF spiking 最大容忍 ISI",
    "旧版/诊断：HF 状态短 ISI 上限",
    "旧版/诊断：强 pause ISI 阈值",
    "旧版/诊断兼容参数。当前活动 AUTO event grammar 不消费此值；仅供旧版/备用检测和诊断记录使用。",
    "旧版/near-miss 诊断兼容参数。当前活动 AUTO event grammar 的 HF-spiking short band 来自解析后的生效阈值，不消费此值；仅供兼容路径和诊断记录使用。",
    "旧版/near-miss 诊断兼容参数。当前活动 AUTO event grammar 不消费此值；仅供旧版/near-miss 诊断和兼容记录使用。",
    "选择 user、manual、histogram 和 default 阈值之间的优先级。auto 按 user → manual → histogram → default 解析；检测实际使用的值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "burst 内部可桥接的中等 ISI 上限。调高会让事件跨过更长的小间隙并合并为同一候选。在 auto 来源策略下，本输入仅作为 default 回退值；检测实际读取解析后的生效值，当前值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "burst 核心相对 pre/post 背景的最小对比度。调高更保守，要求事件更突显；调低会增加召回。在 auto 来源策略下，本输入仅作为 default 回退值；检测实际读取解析后的生效值，当前值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "burst 紧凑核心允许的最短 ISI。通常贴近有效 ISI 下限；调低可能纳入伪迹，调高会忽略最短的核心间隔。在 auto 来源策略下，本输入仅作为 default 回退值；检测实际读取解析后的生效值，当前值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "burst 紧凑核心的短 ISI 上限。调高会把较慢的 spike 簇纳入 burst seed，提高召回但可能增加假阳性。在 auto 来源策略下，本输入仅作为 default 回退值；检测实际读取解析后的生效值，当前值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "规则强直发放允许的最长 ISI。调高会纳入更慢的稳定放电，调低会更严格。在 auto 来源策略下，本输入仅作为 default 回退值；检测实际读取解析后的生效值，当前值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "规则强直发放允许的最短 ISI。调高会排除过快片段，帮助区分 HF tonic / HF spiking。在 auto 来源策略下，本输入仅作为 default 回退值；检测实际读取解析后的生效值，当前值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "进入 pause 候选的长 ISI 起点阈值。调高会只保留更长暂停，调低会增加短暂停候选。在 auto 来源策略下，本输入仅作为 default 回退值；检测实际读取解析后的生效值，当前值与来源请查看“阈值来源 / 实际检测阈值”表。",
    "从 MANUAL HF 学习旧版/near-miss 诊断锚点",
    "tonic/pause 锚点可用于当前相应活动层的尺度定位；HF 锚点仅供旧版/near-miss 诊断，当前活动 AUTO event grammar 不消费 HF 锚点。最终 AUTO 仍由局部结构、连续性和对比决定。",
    "旧版/near-miss 诊断高频锚点"
  )
  dictionary <- getFromNamespace(
    "stpd_i18n_exact_dictionary", "SpikeTrainPatternDetector"
  )()
  translate <- getFromNamespace(
    "stpd_i18n_translate_text", "SpikeTrainPatternDetector"
  )

  expect_true(all(source %in% names(dictionary)))
  english <- vapply(source, translate, character(1), lang = "en")
  expect_false(any(grepl("[\u3400-\u9FFF]", english, perl = TRUE)))
  expect_false(any(english == source))
  expect_true(all(nzchar(english)))
})
