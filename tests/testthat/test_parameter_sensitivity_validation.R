make_sensitivity_validation_train <- function() {
  isi <- c(0.10, 0.20, 0.008, 0.012, 0.008, 0.20, 0.10)
  t <- cumsum(c(0, isi))
  manual <- rep("", length(t))
  manual[4:6] <- "burst"
  data.frame(
    idx = seq_along(t),
    timestamp_sec = t,
    ISI_sec = c(NA_real_, diff(t)),
    pattern_manual = manual,
    pattern_auto = rep("", length(t)),
    stringsAsFactors = FALSE
  )
}

make_sensitivity_validation_params <- function() {
  p <- default_params()
  p$event_core$seed_band_upper_sec <- 0.013
  p$spiketrainpattern$burst$seed_upper_sec <- 0.013
  p$event_grammar$threshold_source_mode <- "default"
  p$spiketrainpattern$engine$threshold_source_mode <- "default"
  p
}

test_that("event-level match table separates label confusion from extras", {
  truth <- data.frame(event_id = 1L, train = "t1", pattern = "burst", start_isi = 2L, end_isi = 4L)
  pred <- data.frame(event_id = 1L, train = "t1", pattern = "tonic", start_isi = 2L, end_isi = 4L)
  matches <- stpd_event_level_match_table(pred, truth, iou_min = 0.25)
  expect_true(any(matches$error_type == "label_confusion"))
  expect_true(any(matches$match_status == "false_positive"))
  expect_true(any(matches$match_status == "false_negative"))
  expect_false(any(matches$match_status == "true_positive"))

  extra <- data.frame(event_id = 1L, train = "t1", pattern = "tonic", start_isi = 10L, end_isi = 11L)
  matches_extra <- stpd_event_level_match_table(extra, truth, iou_min = 0.25)
  expect_true(any(matches_extra$error_type == "extra_detector_event"))
  expect_false(any(matches_extra$error_type == "label_confusion"))
})

test_that("event-level validation report produces IoU metrics and exports", {
  ds <- make_dataset(
    "event_validation",
    "synthetic",
    list(train_1 = make_sensitivity_validation_train()),
    unit_in = "s"
  )
  params <- make_sensitivity_validation_params()
  report <- stpd_event_level_validation_report(ds, params, selected_trains = "train_1", iou_min = 0.25)

  expect_true(is.data.frame(report$metrics))
  expect_true(any(report$metrics$pattern == "burst" & report$metrics$true_positive_n >= 1))
  expect_true(any(report$matches$match_status == "true_positive"))
  expect_true(all(c("start_boundary_error_isi", "end_boundary_error_isi", "boundary_abs_error_isi") %in% names(report$matches)))

  out_dir <- tempfile("event_level_validation_export_")
  stpd_event_level_validation_export(report, out_dir)
  expect_true(file.exists(file.path(out_dir, "Event_level_validation_summary.csv")))
  expect_true(file.exists(file.path(out_dir, "Event_level_validation_metrics.csv")))
  expect_true(file.exists(file.path(out_dir, "Manual_detector_event_matches.csv")))
})

test_that("parameter sensitivity scan is dry-run and exports methods records", {
  ds <- make_dataset(
    "parameter_sensitivity",
    "synthetic",
    list(train_1 = make_sensitivity_validation_train(), train_2 = make_sensitivity_validation_train()),
    unit_in = "s"
  )
  ds_before <- ds
  params <- make_sensitivity_validation_params()
  scan <- stpd_parameter_sensitivity_scan(
    ds,
    params,
    selected_trains = c("train_1", "train_2"),
    paths = "event_core.seed_band_upper_sec",
    max_params = 1,
    max_trains = 2,
    relative_step = 0.5,
    iou_min = 0.25,
    permutation_n = 31
  )

  expect_true(is.data.frame(scan$summary))
  expect_true(any(scan$summary$variant_id == "baseline_current"))
  expect_true(any(scan$summary$parameter_path == "event_core.seed_band_upper_sec"))
  expect_true(all(c(
    "macro_precision", "macro_recall", "macro_F1", "changed_event_n",
    "current_tonic_state_episode_n", "delta_tonic_state_episode_n",
    "current_tonic_direct_support_isi_n", "delta_tonic_direct_support_isi_n",
    "current_broad_hfs_state_episode_n", "changed_overlap_resolution_n"
  ) %in% names(scan$summary)))
  expect_true(all(c("sensitivity_raw_p_value", "sensitivity_q_value", "robust_parameter_flag") %in% names(scan$summary)))
  expect_true(is.data.frame(scan$metrics))
  expect_true(is.data.frame(scan$matches))
  expect_true(is.data.frame(scan$train_metrics))
  expect_true(is.data.frame(scan$multiple_comparison_tests))
  expect_true(is.data.frame(scan$meta))
  expect_identical(scan$meta$preview_authority, "dry_run_non_authoritative")
  expect_false(scan$meta$preview_applies_parameters)
  expect_true(is.data.frame(scan$state_episode_differences))
  expect_true(is.data.frame(scan$state_direct_support_differences))
  expect_true(is.data.frame(scan$overlap_resolution_differences))
  expect_true(all(c(
    "scientific_direction", "preview_authority", "preview_applied",
    "effective_variant_value", "runtime_contract_status",
    "baseline_params_hash", "context_sha256"
  ) %in% names(scan$summary)))
  expect_identical(ds$trains, ds_before$trains)
  expect_identical(ds$results, ds_before$results)
  expect_identical(ds$params_last, ds_before$params_last)
  expect_identical(ds$params_est, ds_before$params_est)

  out_dir <- tempfile("parameter_sensitivity_export_")
  stpd_parameter_sensitivity_export(scan, out_dir)
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_summary.csv")))
  expect_true(file.exists(file.path(out_dir, "Event_level_validation_metrics.csv")))
  expect_true(file.exists(file.path(out_dir, "Manual_detector_event_matches.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_multiple_comparison_tests.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_meta.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_state_episode_differences.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_state_direct_support_differences.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_overlap_resolution_differences.csv")))
})

test_that("Tonic threshold direction is scientific rather than merely numeric", {
  expect_identical(
    stpd_parameter_sensitivity_scientific_direction(
      "tonic.tonic_mm_max", 1.25, 1.35, "increase"
    ),
    "relaxed"
  )
  expect_identical(
    stpd_parameter_sensitivity_scientific_direction(
      "tonic.tonic_mm_min", 0.85, 0.70, "decrease"
    ),
    "relaxed"
  )
  expect_identical(
    stpd_parameter_sensitivity_scientific_direction(
      "tonic.tonic_mm_min", 0.85, 1.00, "increase"
    ),
    "tightened"
  )
})

test_that("MM previews above 1.40 are not presented as applied without a frozen contract", {
  ds <- make_dataset(
    "parameter_sensitivity_mm_contract",
    "synthetic",
    list(train_1 = make_sensitivity_validation_train()),
    unit_in = "s"
  )
  scan <- stpd_parameter_sensitivity_scan(
    ds,
    default_params(),
    selected_trains = "train_1",
    paths = "tonic.tonic_mm_relaxed_max",
    path_scope = "tonic_regularization",
    max_params = 1L,
    max_trains = 1L,
    relative_step = 0.25,
    permutation_n = 15L
  )
  invalid <- scan$summary[
    scan$summary$parameter_path == "tonic.tonic_mm_relaxed_max" &
      suppressWarnings(as.numeric(scan$summary$variant_value)) > 1.40,
    , drop = FALSE
  ]
  expect_equal(nrow(invalid), 1L)
  expect_identical(invalid$variant_status, "invalid_contract")
  expect_false(invalid$preview_applied)
  expect_identical(invalid$effective_variant_value, "1.40")
  expect_match(
    invalid$runtime_contract_status,
    "detector_would_fallback_to_1.40",
    fixed = TRUE
  )
  expect_true(is.na(invalid$macro_F1))
})

test_that("sensitivity context hash invalidates changed data or parameters", {
  ds <- make_dataset(
    "parameter_sensitivity_hash",
    "synthetic",
    list(train_1 = make_sensitivity_validation_train()),
    unit_in = "s"
  )
  params <- default_params()
  h1 <- stpd_parameter_sensitivity_context_hash(ds, params, "train_1")
  changed_params <- stpd_set_param(params, "tonic.LV_core", 0.55)
  h2 <- stpd_parameter_sensitivity_context_hash(ds, changed_params, "train_1")
  changed_ds <- ds
  changed_ds$trains$train_1$pattern_manual[2L] <- "tonic"
  h3 <- stpd_parameter_sensitivity_context_hash(changed_ds, params, "train_1")
  expect_false(identical(h1, h2))
  expect_false(identical(h1, h3))
})

test_that("Tonic regularization sensitivity scope is an exact safe allowlist", {
  expected <- c(
    "tonic.LV_core",
    "tonic.tonic_mm_min",
    "tonic.tonic_mm_max",
    "tonic.tonic_mm_relax_lv_max",
    "tonic.tonic_mm_relax_cv_max",
    "tonic.tonic_mm_relaxed_max"
  )
  expect_identical(stpd_tonic_sensitivity_paths(max_params = 99L), expected)
  schema <- stpd_parameter_sensitivity_schema("tonic_regularization")
  expect_identical(as.character(schema$path), expected)
  expect_true(all(as.character(schema$ui_level) %in% c("advanced", "expert")))

  expect_error(
    stpd_parameter_variant_grid(
      default_params(), paths = "pause.T_seed",
      path_scope = "tonic_regularization"
    ),
    "not allowlisted"
  )
})

test_that("Tonic sensitivity variants change the effective canonical parameter", {
  params <- default_params()
  variants <- stpd_parameter_variant_grid(
    params,
    paths = "tonic.tonic_mm_max",
    max_params = 1L,
    relative_step = 0.05,
    path_scope = "tonic_regularization"
  )
  expect_gt(length(variants), 1L)
  baseline <- effective_params_for_detector(variants[[1L]]$params)$tonic$tonic_mm_max
  effective <- vapply(
    variants[-1L],
    function(v) effective_params_for_detector(v$params)$tonic$tonic_mm_max,
    numeric(1)
  )
  expect_true(all(is.finite(effective)))
  expect_true(all(effective != baseline))
})
