test_that("QC warning_message reports only actual issues", {
  params <- default_params()
  dat <- data.frame(
    timestamp_sec = c(0, 0.01, 0.02, 0.03, 0.04),
    ISI_sec = c(NA, 0.01, 0.01, 0.01, 0.01),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  q <- validate_train_quality(dat, train = "t1", min_isi_sec = 0.001, refractory_suspect_sec = 0.0015, display_unit = "ms")
  expect_equal(q$n_artifact_ISI, 0L)
  expect_equal(q$n_refractory_suspect_ISI, 0L)
  expect_false(grepl("n_artifact_ISI=0", q$warning_message, fixed = TRUE))
  expect_false(grepl("n_refractory_suspect_ISI=0", q$warning_message, fixed = TRUE))
  expect_false(grepl("percentile_status=reliable", q$warning_message, fixed = TRUE))
})

test_that("QC warning_message includes artifact values only when artifacts exist", {
  dat <- data.frame(
    timestamp_sec = c(0, 0.0005, 0.01),
    ISI_sec = c(NA, 0.0005, 0.0095),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  q <- validate_train_quality(dat, train = "t1", min_isi_sec = 0.001, refractory_suspect_sec = 0.0015, display_unit = "ms")
  expect_equal(q$n_artifact_ISI, 1L)
  expect_true(grepl("n_artifact_ISI=1", q$warning_message, fixed = TRUE))
  expect_true(grepl("artifact_ISI_ms", q$warning_message, fixed = TRUE))
})

test_that("QC treats explicit zero or negative ISI values as integrity errors", {
  dat <- data.frame(
    timestamp_sec = c(0, 0.01, 0.02, 0.03),
    ISI_sec = c(NA, 0.01, 0, 0.01),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  q <- validate_train_quality(dat, train = "t1", min_isi_sec = 0.001, refractory_suspect_sec = 0.0015, display_unit = "ms")
  expect_equal(q$n_zero_or_negative_ISI, 1L)
  expect_equal(q$n_zero_or_negative_timestamp_steps, 0L)
  expect_equal(q$warning_level, "error")
  expect_true(grepl("zero_or_negative_ISI=1", q$warning_message, fixed = TRUE))
})
