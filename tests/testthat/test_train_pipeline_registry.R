test_that("train pipeline registry exposes stable semantic pipelines", {
  registry <- stpd_train_pipeline_registry()
  expected <- c(
    "near_miss_augmented",
    "arbitrated",
    "seed_bridge_classicity",
    "event_grammar_core",
    "threshold_resolved",
    "hf_protected"
  )

  expect_equal(names(registry), expected)
  expect_true(all(vapply(registry, is.function, logical(1))))
  expect_equal(stpd_train_pipeline_default(default_params()), "hf_protected")

  params <- default_params()
  params$detector$train_pipeline <- "threshold_resolved"
  expect_equal(stpd_train_pipeline_default(params), "threshold_resolved")

  params$detector$train_pipeline <- "not_a_pipeline"
  expect_equal(stpd_train_pipeline_default(params), "hf_protected")
})

test_that("train dispatcher preserves the default detector path", {
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  dat <- ds$trains$train_1
  min_isi <- params$detector$min_valid_isi_sec

  default_out <- run_detector_one_train(dat, params, min_isi_sec = min_isi, train = "train_1")
  explicit_out <- stpd_detect_train_dispatch(
    dat,
    params,
    min_isi_sec = min_isi,
    train = "train_1",
    pipeline = "hf_protected"
  )

  expect_identical(as.character(default_out$pattern_auto), as.character(explicit_out$pattern_auto))
  expect_equal(default_out$auto_score, explicit_out$auto_score)
  expect_equal(
    attr(default_out, "manual_lock_applied_to_auto"),
    attr(explicit_out, "manual_lock_applied_to_auto")
  )
})

test_that("train dispatcher honors explicit pipeline selection and rejects unknown names", {
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  dat <- ds$trains$train_1
  min_isi <- params$detector$min_valid_isi_sec

  params$detector$train_pipeline <- "hf_protected"
  from_params <- run_detector_one_train(dat, params, min_isi_sec = min_isi, train = "train_1")
  explicit <- stpd_detect_train_dispatch(dat, params, min_isi_sec = min_isi, train = "train_1", pipeline = "hf_protected")
  expect_identical(as.character(from_params$pattern_auto), as.character(explicit$pattern_auto))

  expect_error(
    stpd_detect_train_dispatch(dat, params, min_isi_sec = min_isi, train = "train_1", pipeline = "not_a_pipeline"),
    "Unknown train detector pipeline"
  )
})
