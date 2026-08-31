test_that("Pause reviewer classifies and groups held-out Pause disagreements", {
  spikes <- list(T1 = c(0.001, 0.010, 0.020, 0.060, 0.100, 0.150))
  intervals <- data.frame(
    Train_ID = rep("T1", 5), Right_Spike_Index = 2:6,
    Truth = c("pause", "pause", "other", "burst", "pause"),
    Prediction = c("other", "burst", "pause", "pause", "pause"),
    stringsAsFactors = FALSE
  )
  bundle <- SpikeTrainPatternDetector:::stpd_pause_reviewer_bundle(
    intervals, spikes, id = "fixture", label = "fixture", provenance = "fixture"
  )
  expect_setequal(
    unique(bundle$intervals$error_type[!is.na(bundle$intervals$error_type)]),
    c("fn_other", "fn_burst", "fp_other", "fp_burst")
  )
  expect_equal(nrow(bundle$catalog), 4L)
  expect_true(all(bundle$catalog$context_start_sec <= bundle$catalog$start_time_sec))
  expect_true(all(bundle$catalog$context_end_sec >= bundle$catalog$end_time_sec))
})

test_that("Pause reviewer combines adjacent same-type errors into one review event", {
  spikes <- list(T1 = seq(0.01, 0.08, by = 0.01))
  intervals <- data.frame(
    Train_ID = rep("T1", 7), Right_Spike_Index = 2:8,
    Truth = c("other", "pause", "pause", "pause", "other", "other", "other"),
    Prediction = c("other", "other", "other", "pause", "other", "other", "other"),
    stringsAsFactors = FALSE
  )
  bundle <- SpikeTrainPatternDetector:::stpd_pause_reviewer_bundle(
    intervals, spikes, id = "fixture", label = "fixture", provenance = "fixture"
  )
  expect_equal(nrow(bundle$catalog), 1L)
  expect_equal(bundle$catalog$start_isi, 3L)
  expect_equal(bundle$catalog$end_isi, 4L)
  expect_equal(bundle$catalog$n_isi, 2L)
})
