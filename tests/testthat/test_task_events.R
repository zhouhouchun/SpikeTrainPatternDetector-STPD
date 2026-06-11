test_that("raw CSV Event columns are extracted as task events, not spike trains", {
  tmp <- tempfile(fileext = ".csv")
  unit_a <- seq(45, 47.9, by = 0.1)
  unit_b <- seq(45.05, 47.95, by = 0.1)
  dat <- data.frame(
    unit_a = unit_a,
    unit_b = unit_b,
    `Event_Right hand fist` = c(45.5, 46.25, 47.25, rep(NA_real_, 27)),
    check.names = FALSE
  )
  utils::write.csv(dat, tmp, row.names = FALSE)

  trains <- build_trains_from_raw(tmp, header = TRUE, unit_in = "s")
  expect_equal(names(trains), c("unit_a", "unit_b"))
  expect_false("Event_Right hand fist" %in% names(trains))

  events <- stpd_extract_task_events_from_raw(tmp, header = TRUE, unit_in = "s")
  expect_equal(nrow(events), 3L)
  expect_equal(unique(events$event_name), "Right hand fist")
  expect_true(all(is.finite(events$event_time_sec)))

  ds <- build_spike_dataset(tmp, mode = "raw", unit_in = "s", header = TRUE)
  expect_equal(length(ds$trains), 2L)
  expect_equal(nrow(ds$task_events), 3L)
})

test_that("task events annotate neural manifold bins and can feed sliceTCA tensor trials", {
  make_train <- function(times) {
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
  trains <- list(
    n1 = make_train(seq(45.0, 47.0, by = 0.08)),
    n2 = make_train(seq(45.03, 47.2, by = 0.11)),
    n3 = make_train(seq(45.01, 47.1, by = 0.14))
  )
  events <- stpd_normalize_task_events(data.frame(
    event_name = c("Right hand fist", "Right hand fist"),
    event_time_sec = c(45.6, 46.4),
    event_column = "Event_Right hand fist",
    stringsAsFactors = FALSE
  ))

  pop <- stpd_make_neural_population_matrix(
    trains,
    selected_trains = names(trains),
    bin_sec = 0.05,
    start_sec = 45,
    end_sec = 47,
    time_origin = "raw",
    task_events = events,
    task_event_pre_sec = 0.2,
    task_event_post_sec = 0.3
  )
  expect_true(any(pop$features$task_event_in_window))
  expect_true(any(pop$features$task_event_epoch %in% c("pre_event", "event_onset", "post_event")))

  pca <- stpd_run_neural_manifold_embedding(pop, method = "pca")
  pca$task_event_triggered <- stpd_neural_task_event_triggered_trajectory(pca)
  expect_true(is.data.frame(pca$task_event_triggered))
  expect_false("message" %in% names(pca$task_event_triggered))
  expect_true(any(pca$task_event_triggered$task_event_name == "Right hand fist"))

  trials <- stpd_task_events_for_slicetca(events)
  tensor <- stpd_make_slicetca_trial_tensor(
    trains,
    selected_trains = names(trains),
    trial_events = trials,
    event_time_col = "event_time_sec",
    trial_id_col = "trial_id",
    condition_col = "condition",
    pre_sec = 0.1,
    post_sec = 0.2,
    bin_sec = 0.05,
    time_origin = "raw"
  )
  expect_identical(tensor$status, "ready")
  expect_equal(dim(tensor$tensor)[1], 2L)
})
