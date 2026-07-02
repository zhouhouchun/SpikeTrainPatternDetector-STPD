test_that("version-neutral internal API exposes stable result names", {
  ds <- stpd_golden_test_dataset("middle_burst")
  out <- stpd_detect(ds, default_params(), selected_trains = "train_1")

  expect_true("threshold_table" %in% names(out$results))
  expect_true("candidate_diagnostic_audit" %in% names(out$results))
  expect_true("candidate_features" %in% names(out$results))
  expect_true("final_decisions" %in% names(out$results))
  expect_true("run_metadata_public" %in% names(out$results))
  expect_true(is.data.frame(out$results$threshold_table))
  expect_true(is.data.frame(out$results$candidate_diagnostic_audit))
  expect_false(any(grepl("(^v[0-9]|_v[0-9])", names(out$results), ignore.case = TRUE)))
  expect_false(any(grepl("^v[0-9]+", names(attributes(out$trains$train_1)))))
  expect_false("detector" %in% names(out$params_last))
  expect_true("spiketrainpattern" %in% names(out$params_last))
})

test_that("version-neutral detector wrappers delegate to the active event grammar", {
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  dat <- ds$trains$train_1

  vp <- stpd_event_grammar_params(dat, params, min_isi_sec = params$detector$min_valid_isi_sec)
  expect_true(is.list(vp))
  expect_true("threshold_table" %in% names(vp))

  det <- stpd_detect_train_event_grammar(
    dat,
    params,
    min_isi_sec = params$detector$min_valid_isi_sec,
    train = "train_1",
    lock_manual = TRUE
  )
  expect_true("pattern_auto" %in% names(det))
})

test_that("version-neutral threshold resolver returns effective bands", {
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  resolved <- stpd_resolve_thresholds_for_dataset(
    ds$trains,
    params,
    min_isi_sec = params$detector$min_valid_isi_sec
  )

  expect_true("threshold_table" %in% names(resolved))
  expect_true("effective_bands" %in% names(resolved))
  expect_true(is.data.frame(resolved$threshold_table))
  expect_true(length(resolved$effective_bands) > 0)
})

test_that("detector progress callback reports stages without changing detection output", {
  ds <- stpd_golden_test_dataset("middle_burst")
  ds$trains$train_2 <- ds$trains$train_1
  params <- default_params()
  target <- c("train_1", "train_2")

  phases <- character()
  seen_trains <- character()
  out <- stpd_detect(
    ds,
    params,
    selected_trains = target,
    collect_diagnostics = FALSE,
    progress_callback = function(phase, train = NULL, ...) {
      phases <<- c(phases, phase)
      if (!is.null(train) && nzchar(as.character(train))) seen_trains <<- c(seen_trains, as.character(train))
    }
  )
  ref <- stpd_detect(ds, params, selected_trains = target, collect_diagnostics = FALSE)

  expect_true(all(c("train_start", "train_done", "complete", "public_complete") %in% phases))
  expect_equal(sort(unique(seen_trains)), target)
  expect_equal(out$trains$train_1$pattern_auto, ref$trains$train_1$pattern_auto)
  expect_equal(out$trains$train_2$pattern_auto, ref$trains$train_2$pattern_auto)
})
