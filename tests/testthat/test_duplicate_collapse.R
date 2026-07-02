
test_that("duplicate timestamp collapse keeps first occurrence and recomputes ISI", {
  dat <- data.frame(
    idx = 1:4,
    timestamp_sec = c(0.0, 0.1, 0.1, 0.3),
    ISI_sec = c(NA, 0.1, 0.0, 0.2),
    pattern_manual = c("", "burst", "others", ""),
    pattern_auto = c("", "burst", "burst", ""),
    stringsAsFactors = FALSE
  )
  res <- collapse_duplicate_timestamps_train(dat)
  expect_equal(res$dropped, 1L)
  expect_equal(nrow(res$data), 3L)
  expect_equal(res$data$timestamp_sec, c(0.0, 0.1, 0.3))
  expect_equal(res$data$ISI_sec, c(NA, 0.1, 0.2))
  expect_equal(res$data$pattern_manual[2], "burst")
  expect_true(all(res$data$duplicate_timestamp_policy == "collapse_manual"))
})

test_that("duplicate timestamp collapse reports duplicate timestamp groups, not duplicate rows", {
  dat <- data.frame(
    idx = 1:7,
    timestamp_sec = c(0.0, 0.1, 0.1, 0.1, 0.3, 0.4, 0.4),
    ISI_sec = c(NA, 0.1, 0.0, 0.0, 0.2, 0.1, 0.0),
    stringsAsFactors = FALSE
  )
  res <- collapse_duplicate_timestamps_train(dat)
  expect_equal(res$dropped, 3L)
  expect_equal(res$duplicate_groups, 2L)
  expect_equal(res$data$timestamp_sec, c(0.0, 0.1, 0.3, 0.4))
})
