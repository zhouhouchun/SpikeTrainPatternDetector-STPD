test_that("train LogISI suggestion delegates to Pasquale logISIH support", {
  set.seed(3)
  isi <- c(
    10^stats::rnorm(200, log10(0.005), 0.04),
    10^stats::rnorm(200, log10(0.050), 0.05)
  )
  detail <- SpikeTrainPatternDetector:::estimate_logisi_threshold_train_result(
    isi,
    min_isi_sec = 0.001,
    mcv_sec = 0.100
  )
  pasquale <- stpd_estimate_logisi_threshold_pasquale(
    isi_sec = isi,
    min_valid_isi_sec = 0.001,
    intraburst_peak_window_ms = 100,
    max_reasonable_threshold_sec = 0.100
  )

  expect_equal(detail$method, "pasquale_logisi")
  expect_true(isTRUE(detail$accepted))
  expect_equal(detail$threshold_sec, pasquale$threshold_sec, tolerance = 1e-12)
  expect_equal(
    SpikeTrainPatternDetector:::estimate_logisi_threshold_train(
      isi,
      min_isi_sec = 0.001,
      mcv_sec = 0.100
    ),
    detail$threshold_sec,
    tolerance = 1e-12
  )
})
