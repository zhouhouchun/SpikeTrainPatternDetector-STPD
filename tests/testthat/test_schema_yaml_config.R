test_that("parameter YAML materializes runtime, product, and key schema defaults", {
  cfg <- getFromNamespace("stpd_parameter_config", "SpikeTrainPatternDetector")(reload = TRUE)
  cfg_cache <- getFromNamespace("stpd_parameter_config_cache", "SpikeTrainPatternDetector")
  expect_equal(cfg$schema_version, "spiketrainpattern-parameters-1")
  expect_true(file.exists(getFromNamespace("stpd_parameter_config_path", "SpikeTrainPatternDetector")()))
  expect_match(cfg_cache$path, "^package-default:")
  expect_false(grepl("00LOCK|00new|[/\\\\]", cfg_cache$path))

  runtime_defaults <- getFromNamespace("stpd_runtime_default_params", "SpikeTrainPatternDetector")()
  default_patterns <- getFromNamespace("stpd_default_patterns_to_run", "SpikeTrainPatternDetector")()
  expect_equal(runtime_defaults$burst$T_seed, cfg$runtime_defaults$burst$T_seed)
  expect_equal(runtime_defaults$detector$patterns_to_run, cfg$runtime_defaults$detector$patterns_to_run)
  expect_equal(default_patterns, cfg$runtime_defaults$detector$patterns_to_run)

  p <- default_params_sec()
  expect_equal(p$burst$T_seed, cfg$runtime_defaults$burst$T_seed)
  expect_equal(p$spiketrainpattern$burst$seed_upper_sec, cfg$product_defaults$burst$seed_upper_sec)
  expect_equal(stpd_product_schema_defaults()$high_frequency_spiking$min_spikes, cfg$product_defaults$high_frequency_spiking$min_spikes)

  schema_paths <- stpd_parameter_schema()$path
  expect_true(all(vapply(cfg$key_schema, `[[`, character(1), "path") %in% schema_paths))
  expect_true(all(vapply(cfg$eventness_schema, `[[`, character(1), "path") %in% schema_paths))
})

test_that("UI pattern selection defaults back to the full core detector set", {
  resolver <- getFromNamespace("stpd_resolve_patterns_to_run", "SpikeTrainPatternDetector")

  non_strict <- resolver("burst", strict_subset = FALSE)
  expect_true(all(c("burst", "tonic", "pause", "high_frequency_tonic", "high_frequency_spiking") %in% non_strict))

  strict <- resolver("burst", strict_subset = TRUE)
  expect_equal(strict, "burst")
})

test_that("YAML parameter contract covers all materialized defaults", {
  p <- default_params_sec()
  flat <- getFromNamespace("stpd_flatten_params", "SpikeTrainPatternDetector")(p)
  contract <- stpd_parameter_contract()

  expect_equal(nrow(contract), nrow(stpd_parameter_schema(scope = "all")))
  expect_true(all(c("path", "type", "default", "unit", "schema_scope", "required",
                    "ui_level", "ui_order", "section", "section_order", "advanced",
                    "expert_only", "visible_if", "help_text", "control_type") %in% names(contract)))
  expect_false(any(is.na(contract$type) | !nzchar(contract$type)))
  expect_false(any(is.na(contract$default) | !nzchar(contract$default)))
  expect_false(any(is.na(contract$ui_level) | !nzchar(contract$ui_level)))
  expect_true(all(c("basic", "advanced", "expert") %in% contract$ui_level))
  expect_equal(setdiff(flat$path, contract$path), character())
  expect_true(all(stpd_parameter_schema()$path %in% contract$path))
})

test_that("key schema defaults stay synchronized with YAML-backed default params", {
  p <- default_params_sec()
  schema <- stpd_parameter_schema()
  get_param <- getFromNamespace("stpd_get_param", "SpikeTrainPatternDetector")
  schema_value <- getFromNamespace("stpd_schema_value", "SpikeTrainPatternDetector")

  mismatches <- vapply(seq_len(nrow(schema)), function(i) {
    actual <- get_param(p, schema$path[i], NULL)
    expected <- schema_value(schema[i, , drop = FALSE])
    !isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  }, logical(1))

  expect_false(any(mismatches), info = paste(schema$path[mismatches], collapse = ", "))
})

test_that("contract validator checks type, ranges, and choices", {
  p <- default_params_sec()
  expect_false(any(stpd_validate_params(p)$severity == "error"))

  bad_range <- p
  bad_range$detector$min_valid_isi_sec <- -0.001
  range_issues <- stpd_validate_params(bad_range)
  expect_true(any(range_issues$severity == "error" & range_issues$path == "detector.min_valid_isi_sec"))

  bad_choice <- p
  bad_choice$burst$local_compression_candidate_class <- "not_a_candidate_class"
  choice_issues <- stpd_validate_params(bad_choice)
  expect_true(any(choice_issues$severity == "error" & choice_issues$path == "burst.local_compression_candidate_class"))

  bad_type <- p
  bad_type$pause$global_median_guard <- "TRUE"
  type_issues <- stpd_validate_params(bad_type)
  expect_true(any(type_issues$severity == "error" & type_issues$path == "pause.global_median_guard"))

  missing_numeric <- p
  missing_numeric$detector$min_valid_isi_sec <- NA_real_
  missing_issues <- stpd_validate_params(missing_numeric)
  expect_true(any(missing_issues$severity == "error" & missing_issues$path == "detector.min_valid_isi_sec"))

  missing_logical <- p
  missing_logical$detector$fill_others_auto <- NA
  logical_issues <- stpd_validate_params(missing_logical)
  expect_true(any(logical_issues$severity == "error" & logical_issues$path == "detector.fill_others_auto"))
})

test_that("contract-driven UI exposes editable runtime parameters", {
  contract_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")()
  basic_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(ui_level = "basic")
  advanced_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(ui_level = "advanced")
  expert_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(ui_level = "expert")
  expect_gt(nrow(contract_schema), 300)
  expect_gt(nrow(basic_schema), 10)
  expect_lt(nrow(basic_schema), nrow(contract_schema) / 4)
  expect_gt(nrow(advanced_schema), nrow(basic_schema))
  expect_gt(nrow(expert_schema), nrow(basic_schema))
  expect_true(all(basic_schema$ui_level == "basic"))
  expect_true(all(advanced_schema$ui_level == "advanced"))
  expect_true(all(expert_schema$ui_level == "expert"))
  expect_false(any(contract_schema$schema_scope %in% c("key_ui", "eventness_audit")))
  expect_false(any(contract_schema$group %in% c("spiketrainpattern", "metadata")))
  expect_true("event_core.seed_band_upper_sec" %in% contract_schema$path)
  expect_true("burst.T_seed" %in% contract_schema$path)
  expect_true("event_core.seed_band_upper_sec" %in% basic_schema$path)
  expect_true("burst.T_seed" %in% basic_schema$path)

  ui <- getFromNamespace("stpd_contract_ui_controls", "SpikeTrainPatternDetector")()
  ui_text <- paste(as.character(ui), collapse = "")
  expect_true(grepl("contract_param_event_core__seed_band_upper_sec", ui_text, fixed = TRUE))
  expect_true(grepl("schema-contract-group", ui_text, fixed = TRUE))
  expect_true(grepl("schema-section-box", ui_text, fixed = TRUE))
  expect_true(grepl("Basic", ui_text, fixed = TRUE))

  app_ui_text <- paste(as.character(getFromNamespace("ui", "SpikeTrainPatternDetector")), collapse = "")
  expect_true(grepl("params_yaml_in", app_ui_text, fixed = TRUE))
	  expect_true(grepl("params_yaml_out", app_ui_text, fixed = TRUE))
	  expect_true(grepl("contract_ui_level", app_ui_text, fixed = TRUE))
	  expect_true(grepl("xrange_plot", app_ui_text, fixed = TRUE))
	  expect_true(grepl("auto-pattern-legend", app_ui_text, fixed = TRUE))
	  expect_true(grepl("pattern-isi-box", app_ui_text, fixed = TRUE))
	  expect_true(grepl("parameter_validation_table", app_ui_text, fixed = TRUE))
	  expect_true(grepl("parameter_change_preview_table", app_ui_text, fixed = TRUE))
	  expect_true(grepl("run_parameter_delta_preview", app_ui_text, fixed = TRUE))
		  expect_true(grepl("download_parameter_delta_preview_zip", app_ui_text, fixed = TRUE))
		  expect_true(grepl("show_parameter_delta_overlay", app_ui_text, fixed = TRUE))
		  expect_true(grepl("parameter_delta_preview_events_table", app_ui_text, fixed = TRUE))
		  expect_true(grepl("run_parameter_sensitivity_scan", app_ui_text, fixed = TRUE))
		  expect_true(grepl("download_parameter_sensitivity_zip", app_ui_text, fixed = TRUE))
		  expect_true(grepl("parameter_sensitivity_metric_plot", app_ui_text, fixed = TRUE))
		  expect_true(grepl("parameter_sensitivity_summary_table", app_ui_text, fixed = TRUE))
		})

test_that("basic contract metadata is biology-oriented and workflow ordered", {
  contract <- stpd_parameter_contract()
  path_row <- function(path) contract[match(path, contract$path), , drop = FALSE]

  seed <- path_row("event_core.seed_band_upper_sec")
  expect_equal(seed$ui_level, "basic")
  expect_match(seed$label, "Burst seed ISI")
  expect_match(seed$help_text, "\u7D27\u51D1|compact")
  expect_equal(seed$section, "\u7206\u53D1 seed / bridge / contrast")

  burst_order <- as.numeric(path_row("event_core.seed_band_upper_sec")$section_order)
  hf_order <- as.numeric(path_row("highfreq.T_high_max")$section_order)
  tonic_order <- as.numeric(path_row("tonic.T_min")$section_order)
  pause_order <- as.numeric(path_row("pause.T_seed")$section_order)
  arbitration_order <- as.numeric(path_row("arbitration.enabled")$section_order)
  expect_true(burst_order < hf_order)
  expect_true(hf_order < tonic_order)
  expect_true(tonic_order < pause_order)
  expect_true(pause_order < arbitration_order)

  basic_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")(ui_level = "basic")
  visible_order <- match(c("event_core.enabled", "event_core.seed_band_upper_sec", "highfreq.T_high_max", "tonic.T_min", "pause.T_seed", "arbitration.enabled"), basic_schema$path)
  expect_false(any(is.na(visible_order)))
  expect_true(all(diff(visible_order) > 0))
})

test_that("contract-driven inputs write back to nested params", {
  contract_schema <- getFromNamespace("stpd_contract_ui_schema", "SpikeTrainPatternDetector")()
  input <- list(
    contract_param_event_core__seed_band_upper_sec = 0.012,
    contract_param_burst__T_seed = 0.018
  )
  p <- schema_params_from_input(default_params_sec(), input, schema = contract_schema, prefix = "contract_param_", exclude_paths = character())
  expect_equal(p$event_core$seed_band_upper_sec, 0.012)
  expect_equal(p$burst$T_seed, 0.018)
})

test_that("parameter YAML import and export round-trip through contract", {
  p <- default_params_sec()
  p$event_core$seed_band_upper_sec <- 0.012
  p$burst$T_seed <- 0.018
  p$detector$preset_name <- "unit_test_yaml"

  tmp <- tempfile(fileext = ".yml")
  bundle <- stpd_write_params_yaml(p, tmp, source = "testthat")
  expect_true(file.exists(tmp))
  expect_equal(bundle$format, "spiketrainpattern-params-1")

  imported <- stpd_read_params_yaml(tmp)
  expect_equal(imported$params$event_core$seed_band_upper_sec, 0.012)
  expect_equal(imported$params$burst$T_seed, 0.018)
  expect_equal(imported$params$detector$preset_name, "unit_test_yaml")
  expect_false(any(imported$validation$severity == "error"))

  rt <- stpd_parameter_yaml_roundtrip_report(p, source = "testthat")
  expect_true(any(rt$check == "hash_preserved" & rt$status == "ok"))
})

test_that("partial parameter YAML overlays defaults and reports validation errors", {
  partial <- tempfile(fileext = ".yml")
  yaml::write_yaml(
    list(
      format = "spiketrainpattern-params-1",
      parameters = list(event_core = list(seed_band_upper_sec = 0.013))
    ),
    partial
  )
  imported <- stpd_read_params_yaml(partial)
  expect_equal(imported$params$event_core$seed_band_upper_sec, 0.013)
  expect_equal(imported$params$burst$T_seed, default_params_sec()$burst$T_seed)

  bad_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(
    list(
      format = "spiketrainpattern-params-1",
      parameters = list(detector = list(min_valid_isi_sec = -0.001))
    ),
    bad_file
  )
  bad_import <- stpd_read_params_yaml(bad_file)
  expect_true(any(bad_import$validation$severity == "error" & bad_import$validation$path == "detector.min_valid_isi_sec"))
})

test_that("parameter YAML rejects invalid numeric, integer, and logical values before silent coercion", {
  bad_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(
    list(
      format = "spiketrainpattern-params-1",
      parameters = list(
        event_core = list(
          seed_band_upper_sec = "abc",
          max_candidates_per_train = 2.9
        ),
        detector = list(fill_others_auto = "maybe")
      )
    ),
    bad_file
  )

  bad_import <- stpd_read_params_yaml(bad_file)
  expect_true(any(bad_import$validation$severity == "error" & bad_import$validation$path == "event_core.seed_band_upper_sec"))
  expect_true(any(bad_import$validation$severity == "error" & bad_import$validation$path == "event_core.max_candidates_per_train"))
  expect_true(any(bad_import$validation$severity == "error" & bad_import$validation$path == "detector.fill_others_auto"))
  expect_error(stpd_read_params_yaml(bad_file, strict = TRUE), "invalid YAML")
})

test_that("validation issues and parameter reports carry UI metadata", {
  p <- default_params_sec()
  p$detector$min_valid_isi_sec <- -0.001
  issues <- stpd_validate_params(p)
  issue_table <- getFromNamespace("stpd_parameter_issue_table", "SpikeTrainPatternDetector")(issues, ui_level = "basic")
  expect_true(any(issue_table$severity == "error" & issue_table$path == "detector.min_valid_isi_sec"))
  expect_true(all(c("ui_level", "section", "section_order", "ui_order") %in% names(issue_table)))

  p2 <- default_params_sec()
  p2$spiketrainpattern$burst$seed_upper_sec <- 0.012
  report <- parameter_report_table(p2)
  expect_true(all(c("ui_level", "section", "changed_from_default") %in% names(report)))
  expect_true(any(report$path == "event_core.seed_band_upper_sec" & report$changed_from_default))
	  expect_equal(report$ui_level[report$path == "event_core.seed_band_upper_sec"][1], "basic")
	})

test_that("parameter change preview lists UI-visible changes with impact metadata", {
  p <- default_params_sec()
  p$event_core$seed_band_upper_sec <- 0.012
  p <- stpd_productize_params(p, prefer = "legacy")

  preview <- getFromNamespace("stpd_parameter_change_preview", "SpikeTrainPatternDetector")(p)
  expect_true(all(c("path", "label", "ui_level", "section", "current_value", "default_value", "impact") %in% names(preview)))
  expect_true(any(preview$path == "event_core.seed_band_upper_sec"))
  seed <- preview[preview$path == "event_core.seed_band_upper_sec", , drop = FALSE]
  expect_equal(seed$ui_level[1], "basic")
  expect_match(seed$impact[1], "\u7D27\u51D1|compact")
  expect_false(any(grepl("^spiketrainpattern\\.", preview$path)))

  unchanged <- getFromNamespace("stpd_parameter_change_preview", "SpikeTrainPatternDetector")(default_params_sec())
  expect_true("message" %in% names(unchanged))
})
