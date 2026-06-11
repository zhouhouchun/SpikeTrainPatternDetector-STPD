test_that("event-aligned activity builds raster, PSTH, heatmap and synchrony tables", {
  tmp <- tempfile(fileext = ".csv")
  pad <- function(x, n = 110L) c(x, rep(NA_real_, max(0L, n - length(x))))[seq_len(n)]
  dat <- data.frame(
    n1 = pad(seq(9.5, 12.5, by = 0.05)),
    n2 = pad(seq(9.48, 12.48, by = 0.08)),
    n3 = pad(seq(9.45, 12.45, by = 0.11)),
    `Event_Right hand fist` = pad(c(10, 11.2)),
    check.names = FALSE
  )
  utils::write.csv(dat, tmp, row.names = FALSE)
  ds <- build_spike_dataset(tmp, mode = "raw", unit_in = "s", header = TRUE)
  expect_equal(length(ds$trains), 3L)
  expect_equal(nrow(ds$task_events), 2L)

  res <- stpd_event_aligned_activity(
    ds$trains,
    ds$task_events,
    selected_trains = names(ds$trains),
    pre_sec = 0.5,
    post_sec = 0.8,
    bin_sec = 0.05,
    smoothing_sigma_bins = 1,
    baseline_start_sec = -0.5,
    baseline_end_sec = -0.1
  )
  expect_identical(res$status, "ok")
  expect_gt(nrow(res$raster), 0)
  expect_equal(nrow(res$population), nrow(res$bins))
  expect_equal(nrow(res$psth), length(ds$trains) * nrow(res$bins))
  expect_equal(nrow(res$heatmap), length(ds$trains) * nrow(res$bins))
  expect_equal(nrow(res$correlation), length(ds$trains)^2)
  expect_true(all(c("train_x", "train_y", "correlation") %in% names(res$correlation)))
  expect_gt(nrow(res$correlogram), 0)
  expect_true(any(res$summary$metric == "peak_population_rate_hz"))
  expect_s3_class(stpd_event_aligned_raster_plot(res), "plotly")
  expect_s3_class(stpd_event_aligned_population_plot(res), "plotly")
  expect_s3_class(stpd_event_aligned_heatmap_plot(res), "plotly")
})

test_that("event-aligned activity handles a single event without dropping array dimensions", {
  trains <- list(
    a = data.frame(idx = seq_len(20), timestamp_sec = seq(0, 1.9, by = 0.1), ISI_sec = c(NA, rep(0.1, 19)), stringsAsFactors = FALSE),
    b = data.frame(idx = seq_len(20), timestamp_sec = seq(0.03, 1.93, by = 0.1), ISI_sec = c(NA, rep(0.1, 19)), stringsAsFactors = FALSE)
  )
  events <- stpd_normalize_task_events(data.frame(
    event_name = "move",
    event_time_sec = 1,
    stringsAsFactors = FALSE
  ))
  res <- stpd_event_aligned_activity(
    trains,
    events,
    selected_trains = names(trains),
    pre_sec = 0.4,
    post_sec = 0.5,
    bin_sec = 0.1,
    smoothing_sigma_bins = 1
  )
  expect_identical(res$status, "ok")
  expect_equal(dim(res$counts)[1], 1L)
  expect_equal(dim(res$counts)[2], 2L)
  expect_equal(dim(res$counts)[3], nrow(res$bins))
  expect_equal(nrow(res$psth), 2L * nrow(res$bins))
  expect_equal(nrow(res$population), nrow(res$bins))
})
