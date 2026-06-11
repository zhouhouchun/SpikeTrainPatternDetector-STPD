test_that("consistency check returns a table", {
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  ds2 <- stpd_detect(ds, params)
  chk <- stpd_result_consistency_check(ds2)
  expect_true(is.data.frame(chk))
  expect_true(all(c("severity", "component", "issue", "detail") %in% names(chk)))
})

test_that("scientific validation handles no manual labels", {
  ds <- stpd_golden_test_dataset("middle_burst")
  params <- default_params()
  ds2 <- stpd_detect(ds, params)
  val <- stpd_event_level_validation(ds2, params, metric_mode = "strict_high_confidence")
  expect_true(is.data.frame(val))
  expect_true("note" %in% names(val))
})

test_that("metric mode keeps possible_burst strict and merges candidate family", {
  x <- c("burst", "long_burst", "possible_burst", "tonic")
  expect_equal(stpd_metric_mode_normalize(x, "strict_high_confidence"), c("burst", "long_burst", "possible_burst", "tonic"))
  expect_equal(stpd_metric_mode_normalize(x, "candidate_family"), c("burst_family", "burst_family", "burst_family", "tonic"))
})

test_that("lightweight validation uses AUTO predictions, not MANUAL-first final events", {
  ds <- stpd_golden_test_dataset("middle_burst")
  ds$trains$train_1$pattern_manual[4:6] <- "burst"

  out <- stpd_detect(
    ds,
    default_params(),
    selected_trains = "train_1",
    lock_manual = TRUE,
    collect_diagnostics = TRUE
  )

  truth <- stpd_manual_events(out, default_params(), selected_trains = "train_1")
  pred_auto <- stpd_predicted_events(out, default_params(), selected_trains = "train_1")
  pred_final <- stpd_predicted_events(out, default_params(), selected_trains = "train_1", prediction_source = "final")

  expect_true(any(truth$pattern == "burst" & truth$start_isi == 4L & truth$end_isi == 6L))
  expect_false(any(pred_auto$pattern == "burst" & pred_auto$start_isi == 4L & pred_auto$end_isi == 6L))
  expect_true(any(pred_final$pattern == "burst" & pred_final$start_isi == 4L & pred_final$end_isi == 6L))

  val <- stpd_event_level_validation(out, default_params(), selected_trains = "train_1",
                                     metric_mode = "strict_high_confidence")
  burst_row <- val[val$pattern == "burst", , drop = FALSE]
  expect_equal(nrow(burst_row), 1L)
  expect_equal(burst_row$truth_n, 1L)
  expect_equal(burst_row$predicted_n, 0L)
  expect_equal(burst_row$true_positive_n, 0L)
  expect_equal(burst_row$false_negative_n, 1L)
  expect_equal(burst_row$recall, 0)
  expect_true(grepl("auto-source", burst_row$note))
})
