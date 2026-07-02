make_slicetca_train <- function(times) {
  times <- sort(as.numeric(times))
  data.frame(
    idx = seq_along(times),
    timestamp_sec = times,
    ISI_sec = c(NA_real_, diff(times)),
    pattern_manual = rep("", length(times)),
    pattern_auto = rep("", length(times)),
    stringsAsFactors = FALSE
  )
}

test_that("sliceTCA tensor builder and optional backend wrapper are stable without Python", {
  if (requireNamespace("reticulate", quietly = TRUE)) {
    before <- reticulate::py_available(initialize = FALSE)
    status <- stpd_slicetca_backend_status()
    expect_s3_class(status, "data.frame")
    expect_equal(reticulate::py_available(initialize = FALSE), before)
  }

  set.seed(7)
  events <- data.frame(
    trial_id = paste0("t", 1:6),
    movement_onset_sec = seq(0.5, 3.0, by = 0.5),
    condition = rep(c("left", "right"), 3),
    stringsAsFactors = FALSE
  )
  trains <- list(
    n1 = make_slicetca_train(c(seq(0.05, 3.6, by = 0.10), events$movement_onset_sec + 0.03)),
    n2 = make_slicetca_train(c(seq(0.03, 3.5, by = 0.13), events$movement_onset_sec + 0.09)),
    n3 = make_slicetca_train(c(seq(0.02, 3.4, by = 0.17), events$movement_onset_sec + 0.15))
  )
  for (nm in names(trains)) {
    trains[[nm]]$pattern_auto[trains[[nm]]$timestamp_sec > 1.0 & trains[[nm]]$timestamp_sec < 2.1] <- "burst"
    trains[[nm]]$pattern_auto[trains[[nm]]$timestamp_sec > 2.2 & trains[[nm]]$timestamp_sec < 3.2] <- "pause"
  }
  tensor_res <- stpd_make_slicetca_trial_tensor(
    trains,
    selected_trains = names(trains),
    trial_events = events,
    event_time_col = "movement_onset_sec",
    trial_id_col = "trial_id",
    condition_col = "condition",
    pre_sec = 0.1,
    post_sec = 0.3,
    bin_sec = 0.05,
    time_origin = "raw",
    transform = "sqrt_count",
    scaling = "zscore",
    smoothing_sigma_bins = 0,
    label_source = "auto"
  )
  expect_identical(tensor_res$status, "ready")
  expect_equal(dim(tensor_res$tensor)[1:2], c(6L, 3L))
  expect_gt(dim(tensor_res$tensor)[3], 3)
  expect_true(all(c("trial_id", "condition", "event_state") %in% names(tensor_res$event_annotation)))

  res <- stpd_run_slicetca_backend(tensor_res, ranks = c(1L, 0L, 1L), run_python = FALSE)
  expect_identical(res$status, "not_run")
  expect_true(any(res$diagnostics$metric == "status"))
  expect_true(nrow(res$trial_embedding) > 0)
  expect_s3_class(stpd_slicetca_plot(res), "plotly")

  metrics <- stpd_slicetca_reconstruction_metrics(tensor_res)
  expect_true("metric" %in% names(metrics))
  expect_equal(stpd_slicetca_rank_parse("2,0,3"), c(2L, 0L, 3L))
})
