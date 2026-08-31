test_that("Mean-ISI support thresholds import into the Burst parameter store", {
  params <- default_params_sec()
  params$detector$pattern_isi_limits <- list(burst = c(0.001, 0.2))
  tonic_before <- params$tonic
  pause_before <- params$pause
  hfs_before <- params$highfreq
  support <- list(
    thresholds = data.frame(
      train = c("train_b", "train_a", "train_bad"),
      threshold_sec = c(0.020, 0.010, NA_real_),
      threshold_status = c("resolved", "resolved", "unresolved_too_few_valid_isi"),
      stringsAsFactors = FALSE
    ),
    support_report = data.frame(
      train = c("train_a", "train_b"),
      suggested_burst_max_ISI_sec = c(0.018, 0.024),
      burst_isi_q95_sec = c(0.020, 0.026),
      stringsAsFactors = FALSE
    )
  )
  support_bytes <- serialize(support, NULL, version = 3)

  out <- stpd_apply_support_threshold_to_params(params, support, "mean_isi")

  expect_equal(out$burst$T_MI, 0.015)
  expect_equal(out$burst$T_seed, 0.015)
  expect_equal(out$burst$T_bridge, 0.021)
  expect_equal(out$event_core$seed_band_upper_sec, 0.015)
  expect_equal(out$event_core$bridge_band_upper_sec, 0.021)
  expect_true(out$event_grammar$user$burst$enable)
  expect_equal(out$event_grammar$threshold_source_mode, "user")
  expect_equal(out$spiketrainpattern$engine$threshold_source_mode, "user")
  expect_equal(out$spiketrainpattern$burst$seed_upper_sec, 0.015)
  expect_equal(out$metadata$support_threshold_import$trains, c("train_a", "train_b"))
  expect_equal(out$metadata$support_threshold_import$n_resolved_trains, 2L)
  expect_identical(out$tonic, tonic_before)
  expect_identical(out$pause, pause_before)
  expect_identical(out$highfreq, hfs_before)
  expect_identical(out$detector$pattern_isi_limits, params$detector$pattern_isi_limits)
  expect_identical(serialize(support, NULL, version = 3), support_bytes)

  effective <- effective_params_for_detector(out)
  expect_equal(effective$event_core$seed_band_upper_sec, 0.015)
  expect_equal(effective$event_grammar$user$burst$seed_upper_sec, 0.015)
  expect_equal(effective$event_grammar$threshold_source_mode, "user")
})

test_that("LogISI import records method evidence and supports evidence-only mode", {
  params <- default_params_sec()
  old_seed <- params$burst$T_seed
  old_source <- params$event_grammar$threshold_source_mode
  support <- list(
    thresholds = data.frame(
      train = c("train_2", "train_1"),
      threshold_sec = c(0.040, 0.020),
      threshold_status = c("resolved", "resolved_above_reasonable_threshold"),
      stringsAsFactors = FALSE
    ),
    support_report = data.frame()
  )

  evidence_only <- stpd_apply_support_threshold_to_params(
    params, support, "logisi", activate = FALSE
  )
  expect_equal(evidence_only$burst$T_log, 0.030)
  expect_equal(evidence_only$burst$T_log_method, "pasquale_logisi")
  expect_equal(evidence_only$burst$T_log_resolved_n, 2L)
  expect_equal(evidence_only$burst$T_seed, old_seed)
  expect_equal(evidence_only$event_grammar$threshold_source_mode, old_source)
  expect_false(evidence_only$metadata$support_threshold_import$activated)

  active <- stpd_apply_support_threshold_to_params(params, support, "logisi")
  expect_equal(active$burst$T_log, 0.030)
  expect_equal(active$burst$T_seed, 0.030)
  expect_equal(active$event_grammar$threshold_source_mode, "user")
})

test_that("support import rejects unresolved or malformed threshold evidence", {
  params <- default_params_sec()
  unresolved <- list(thresholds = data.frame(
    train = "train_1", threshold_sec = NA_real_,
    threshold_status = "unresolved_no_peaks", stringsAsFactors = FALSE
  ))
  expect_error(
    stpd_apply_support_threshold_to_params(params, unresolved, "logisi"),
    "No resolved positive support threshold"
  )
  expect_error(
    stpd_apply_support_threshold_to_params(params, list(thresholds = data.frame()), "mean_isi"),
    "missing"
  )
})

test_that("support import controls are wired in both UI and server", {
  namespace <- asNamespace("SpikeTrainPatternDetector")
  ui_html <- htmltools::renderTags(get("ui", envir = namespace))$html
  server_src <- paste(
    deparse(body(get("server", envir = namespace))), collapse = "\n"
  )
  for (id in c("apply_misi_support_params", "apply_logisi_support_params")) {
    expect_match(ui_html, paste0('id="', id, '"'), fixed = TRUE)
    expect_match(server_src, paste0("input$", id), fixed = TRUE)
  }
})
