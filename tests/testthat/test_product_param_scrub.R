test_that("versioned top-level params do not delete stable burst params", {
  p <- default_params()
  p$v12_legacy <- TRUE
  p$v12 <- list(enabled = FALSE)
  p$burst$v12_old_runtime <- 1
  stable_burst_names <- setdiff(names(p$burst), "v12_old_runtime")

  out <- stpd_scrub_versioned_runtime_params(p)

  expect_false("v12" %in% names(out))
  expect_true("v12_legacy" %in% names(out))
  expect_false("v12_old_runtime" %in% names(out$burst))
  expect_true(all(stable_burst_names %in% names(out$burst)))
})
