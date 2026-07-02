test_that("bundled real-data subset can be read and QC'd", {
  path <- system.file("extdata", "STN_2017_subset.csv", package = "SpikeTrainPatternDetector")
  skip_if(!file.exists(path))
  ds <- build_spike_dataset(path, mode = "raw", unit_in = "s")
  expect_true(length(ds$trains) >= 1)
  qc <- run_qc(ds, default_params())
  expect_true(nrow(qc) >= 1)
})
