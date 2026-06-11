
test_that("ISI percentile C fallback-compatible function is sane", {
  x <- c(NA, 0.010, 0.020, 0.005, 0.100)
  pct <- compute_isi_percentiles_vector(x, min_isi_sec = 0.001)
  expect_equal(length(pct), length(x))
  expect_true(is.na(pct[1]))
  expect_true(all(pct[2:5] > 0))
})

test_that("default params and schema are available", {
  p <- default_params_sec()
  expect_true(is.list(p))
  expect_true("burst" %in% names(p))
  sch <- stpd_parameter_schema()
  expect_true(nrow(sch) > 0)
})
