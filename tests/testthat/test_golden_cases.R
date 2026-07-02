
test_that("golden synthetic datasets are constructible", {
  ds <- stpd_golden_test_dataset("middle_burst")
  expect_true(is.list(ds$trains))
  expect_true("train_1" %in% names(ds$trains))
})

test_that("golden reference detector runs without throwing", {
  ds <- stpd_golden_test_dataset("middle_burst")
  p <- default_params()
  out <- run_detector_dataset(ds, p, selected_trains="train_1")
  expect_true(is.list(out$results))
  expect_true("events" %in% names(out$results))
})
