
test_that("canonical public API is available", {
  expect_true(is.list(default_params()))
  expect_true(is.function(run_detector_dataset))
  expect_true(is.function(final_classify_candidate))
  expect_true(is.function(compute_candidate_feature_table))
})

test_that("schema can set and get parameters", {
  p <- default_params()
  p <- stpd_set_param(p, "burst.local_compression_local_ratio_min", 2.75)
  expect_equal(stpd_get_param(p, "burst.local_compression_local_ratio_min"), 2.75)
  expect_true(nrow(stpd_parameter_schema()) >= 10)
})
