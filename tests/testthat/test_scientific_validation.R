test_that("event metrics produce expected perfect match", {
  truth <- data.frame(train = "t1", pattern = "burst", start_isi = 2L, end_isi = 4L)
  pred <- data.frame(train = "t1", pattern = "burst", start_isi = 2L, end_isi = 4L)
  m <- stpd_event_level_metrics(pred, truth, iou_min = 0.25)
  expect_equal(m$true_positive_n[m$pattern == "burst"], 1)
  expect_equal(m$false_positive_n[m$pattern == "burst"], 0)
  expect_equal(m$false_negative_n[m$pattern == "burst"], 0)
})

test_that("metric mode keeps possible_burst strict and merges candidate-family", {
  x <- c("burst", "long_burst", "possible_burst", "tonic")
  expect_equal(stpd_metric_mode_normalize(x, "strict_high_confidence"), x)
  expect_equal(stpd_metric_mode_normalize(x, "candidate_family"), c("burst_family", "burst_family", "burst_family", "tonic"))
})

test_that("scientific split is reproducible", {
  ds <- list(trains = list(
    a = data.frame(idx = 1:4, timestamp_sec = c(0, .01, .02, .03), ISI_sec = c(NA, .01, .01, .01), pattern_manual = c("", "burst", "burst", ""), pattern_auto = ""),
    b = data.frame(idx = 1:4, timestamp_sec = c(0, .02, .04, .06), ISI_sec = c(NA, .02, .02, .02), pattern_manual = c("", "tonic", "tonic", ""), pattern_auto = "")
  ), meta = list(display_name = "toy"))
  split1 <- stpd_split_trains_by_manual_events(ds, default_params(), validation_fraction = 0.5, seed = 1)
  split2 <- stpd_split_trains_by_manual_events(ds, default_params(), validation_fraction = 0.5, seed = 1)
  expect_equal(split1$split, split2$split)
})
