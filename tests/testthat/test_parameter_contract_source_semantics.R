test_that("Basic threshold-source selector has a complete, non-empty contract", {
  contract_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(
    ui_level = "basic"
  )
  source_row <- contract_schema[
    as.character(contract_schema$path) == "event_grammar.threshold_source_mode",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(source_row), 1L)
  expect_identical(as.character(source_row$type), "choice")
  expect_identical(as.character(source_row$control_type), "select")
  expect_identical(
    getFromNamespace("stpd_schema_ui_choices", "SpikeTrainPatternDetector")(source_row$choices),
    c("auto", "user", "manual", "histogram", "default")
  )
  expect_identical(as.character(source_row$default), "auto")

  rendered <- getFromNamespace("stpd_schema_ui_control", "SpikeTrainPatternDetector")(
    source_row,
    prefix = "contract_test_",
    show_notes = TRUE,
    lang = "zh"
  )
  rendered_text <- paste(as.character(rendered), collapse = "")
  expect_match(rendered_text, "contract_test_event_grammar__threshold_source_mode", fixed = TRUE)
  for (choice in c("auto", "user", "manual", "histogram", "default")) {
    expect_match(rendered_text, paste0("value=\"", choice, "\""), fixed = TRUE)
  }
})

test_that("resolver fallback semantics and legacy diagnostics are explicit", {
  contract_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(
    ui_level = "all"
  )
  basic_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(
    ui_level = "basic"
  )
  advanced_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(
    ui_level = "advanced"
  )

  fallback_paths <- c(
    "event_core.seed_band_lower_sec",
    "event_core.seed_band_upper_sec",
    "event_core.bridge_band_upper_sec",
    "event_core.burst_contrast_min",
    "tonic.T_min",
    "tonic.T_max",
    "pause.T_seed"
  )
  fallback_rows <- contract_schema[match(fallback_paths, contract_schema$path), , drop = FALSE]
  expect_false(any(is.na(fallback_rows$path)))
  expect_true(all(grepl("auto 来源策略", fallback_rows$help_text, fixed = TRUE)))
  expect_true(all(grepl("default 回退值", fallback_rows$help_text, fixed = TRUE)))
  expect_true(all(grepl("生效值", fallback_rows$help_text, fixed = TRUE)))
  expect_true(all(grepl("阈值来源 / 实际检测阈值", fallback_rows$help_text, fixed = TRUE)))

  legacy_paths <- getFromNamespace(
    "stpd_contract_ui_legacy_diagnostic_paths",
    "SpikeTrainPatternDetector"
  )()
  expect_false(any(legacy_paths %in% basic_schema$path))
  expect_true(all(legacy_paths %in% advanced_schema$path))

  legacy_rows <- contract_schema[match(legacy_paths, contract_schema$path), , drop = FALSE]
  expect_false(any(is.na(legacy_rows$path)))
  expect_true(all(grepl("旧版/", legacy_rows$label, fixed = TRUE)))
  expect_true(all(grepl("当前活动 AUTO event grammar", legacy_rows$help_text, fixed = TRUE)))
  expect_true(all(grepl("不消费", legacy_rows$help_text, fixed = TRUE)))
  expect_true(all(grepl(
    "near-miss",
    legacy_rows$help_text[grepl("^highfreq\\.|^pause\\.T_strong$", legacy_rows$path)],
    fixed = TRUE
  )))
})

test_that("Basic sensitivity omits compatibility-only controls", {
  legacy_paths <- getFromNamespace(
    "stpd_contract_ui_legacy_diagnostic_paths",
    "SpikeTrainPatternDetector"
  )()
  sensitivity_paths <- getFromNamespace(
    "stpd_basic_sensitivity_paths",
    "SpikeTrainPatternDetector"
  )(max_params = 1000L)

  expect_false(any(legacy_paths %in% sensitivity_paths))
  expect_true(all(c(
    "event_core.seed_band_lower_sec",
    "event_core.seed_band_upper_sec",
    "event_core.bridge_band_upper_sec",
    "event_core.burst_contrast_min",
    "event_core.possible_burst_contrast_min",
    "tonic.T_min",
    "tonic.T_max",
    "pause.T_seed"
  ) %in% sensitivity_paths))

  default_paths <- getFromNamespace(
    "stpd_basic_sensitivity_paths",
    "SpikeTrainPatternDetector"
  )()
  expect_identical(default_paths, c(
    "event_core.boundary_floor_sec",
    "event_core.min_seed_isi_count",
    "event_core.max_bridge_isi_count",
    "event_core.max_bridge_isi_fraction",
    "event_core.max_expansion_isi_each_side",
    "event_core.possible_burst_contrast_min",
    "burst.G_min",
    "tonic.G_min"
  ))
  expect_false(any(c(
    "event_core.seed_band_lower_sec",
    "event_core.seed_band_upper_sec",
    "event_core.bridge_band_upper_sec",
    "event_core.burst_contrast_min",
    "tonic.T_min", "tonic.T_max", "pause.T_seed"
  ) %in% default_paths))
})

test_that("sensitivity variants update canonical effective parameters", {
  params <- default_params_sec()
  paths <- getFromNamespace(
    "stpd_basic_sensitivity_paths",
    "SpikeTrainPatternDetector"
  )()
  variants <- getFromNamespace(
    "stpd_parameter_variant_grid",
    "SpikeTrainPatternDetector"
  )(
    params,
    paths = paths,
    max_params = length(paths),
    relative_step = 0.25
  )
  baseline <- effective_params_for_detector(params)

  for (path in paths) {
    one <- Filter(function(x) identical(x$parameter_path, path), variants)
    expect_true(length(one) > 0L, info = path)
    expect_true(
      any(vapply(one, function(x) {
        !identical(effective_params_for_detector(x$params), baseline)
      }, logical(1))),
      info = paste("canonical sensitivity variant remained a no-op:", path)
    )
  }
})

test_that("MANUAL HF calibration is labelled as legacy near-miss diagnostics", {
  app_ui_text <- paste(
    as.character(getFromNamespace("ui", "SpikeTrainPatternDetector")),
    collapse = ""
  )
  expect_match(app_ui_text, "从 MANUAL HF 学习旧版/near-miss 诊断锚点", fixed = TRUE)
  expect_match(app_ui_text, "当前活动 AUTO event grammar 不消费 HF 锚点", fixed = TRUE)
  expect_match(app_ui_text, "旧版/near-miss 诊断高频锚点", fixed = TRUE)
})
