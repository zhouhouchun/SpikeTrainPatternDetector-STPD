test_that("Mean-ISI threshold support resolves simple burst-like train", {
  x <- c(0, 0.010, 0.020, 0.030, 0.500, 0.510, 0.520, 0.530)
  th <- stpd_estimate_misi_threshold(x, min_valid_isi_sec = 0.001)
  expect_equal(th$threshold_status[1], "resolved")
  expect_true(is.finite(th$ML_sec[1]))
  res <- stpd_detect_misi_bursts_article(x, min_valid_isi_sec = 0.001, min_isi_count = 2L, min_spikes = 3L)
  expect_true(is.list(res))
  expect_true("bursts" %in% names(res))
})

test_that("Mean-ISI threshold support handles too few spikes", {
  th <- stpd_estimate_misi_threshold(c(0, 1), min_valid_isi_sec = 0.001)
  expect_match(th$threshold_status[1], "unresolved")
})
