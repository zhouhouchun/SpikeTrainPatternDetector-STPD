test_that("parameter registry and validation work", {
  p <- default_params()
  reg <- stpd_parameter_registry(p)
  expect_true(nrow(reg) > 0)
  expect_true("detector.min_valid_isi_sec" %in% reg$path)
  issues <- stpd_validate_params(p)
  expect_true(is.data.frame(issues))
  rpt <- stpd_parameter_report(p)
  expect_true(all(c("section", "parameter", "value") %in% names(rpt)))
})

test_that("short ISI native prefilter wrapper returns runs", {
  isi <- c(NA, 0.01, 0.011, 0.09, 0.012, 0.013, 0.014, 0.2)
  pct <- compute_isi_percentiles(isi, min_isi_sec = 0.001)
  runs <- scan_short_isi_runs(isi, pct, max_abs_sec = 0.02, max_pct = 50, min_run_isi_n = 2, gate = "either")
  expect_true(is.data.frame(runs))
  expect_true(nrow(runs) >= 1)
})

test_that("detector engine produces a structured version-neutral result on golden data", {
  ds <- stpd_golden_test_dataset("middle_burst")
  p <- default_params()
  out <- stpd_detect(ds, p, selected_trains = "train_1")
  expect_true(is.list(out$results))
  expect_true("candidate_features" %in% names(out$results))
  expect_true("final_decisions" %in% names(out$results))
  expect_true("run_metadata_public" %in% names(out$results))
})
