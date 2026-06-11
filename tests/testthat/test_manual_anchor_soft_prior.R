test_that("manual burst anchors are soft scale priors rather than hard thresholds", {
  structure_candidate_row <- getFromNamespace("structure_candidate_row", "SpikeTrainPatternDetector")

  isi <- c(NA, 0.20, 0.18, 0.05, 0.05, 0.05, 0.24, 0.20, 0.18)
  dat <- data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(c(0, isi[-1])),
    ISI_sec = isi,
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )

  params <- SpikeTrainPatternDetector::default_params()
  p <- params$burst
  p$adaptive_use_train_ranges <- TRUE
  p$adaptive_train_ranges_hard <- TRUE
  p$adaptive_train_ranges <- list(
    train_1 = list(
      low_sec = 0.025,
      high_sec = 0.035,
      low_pct = 0,
      high_pct = 25,
      anchor_center_sec = 0.030,
      anchor_spread_log = 0.30,
      anchor_n = 6L,
      anchor_confidence = 0.50,
      source = "manual_burst",
      method = "manual burst anchor"
    )
  )

  row <- structure_candidate_row(dat, 4L, 6L, structure_id = 1L, train = "train_1",
                                 p = p, min_isi_sec = 0.001)

  expect_equal(as.character(row$structure_class), "structure_seed")
  expect_true(isTRUE(row$manual_anchor_active))
  expect_false(isTRUE(row$manual_anchor_soft_support))
  expect_false(isTRUE(row$train_range_hard_applied))
  expect_match(as.character(row$reject_reason), "manual_anchor_distant_soft")
})

test_that("explicit non-manual hard ranges remain hard constraints", {
  structure_candidate_row <- getFromNamespace("structure_candidate_row", "SpikeTrainPatternDetector")

  isi <- c(NA, 0.20, 0.18, 0.05, 0.05, 0.05, 0.24, 0.20, 0.18)
  dat <- data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(c(0, isi[-1])),
    ISI_sec = isi,
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )

  params <- SpikeTrainPatternDetector::default_params()
  p <- params$burst
  p$adaptive_use_train_ranges <- TRUE
  p$adaptive_train_ranges_hard <- TRUE
  p$adaptive_train_ranges <- list(
    train_1 = list(
      low_sec = 0.025,
      high_sec = 0.035,
      low_pct = 0,
      high_pct = 25,
      source = "ui_tab",
      method = "explicit user hard range"
    )
  )

  row <- structure_candidate_row(dat, 4L, 6L, structure_id = 1L, train = "train_1",
                                 p = p, min_isi_sec = 0.001)

  expect_equal(as.character(row$structure_class), "reject")
  expect_false(isTRUE(row$manual_anchor_active))
  expect_true(isTRUE(row$train_range_hard_applied))
  expect_match(as.character(row$reject_reason), "train_specific_range_failed")
})

test_that("manual anchor closeness gives bounded soft scoring support", {
  score_fun <- getFromNamespace("stpd_manual_anchor_score", "SpikeTrainPatternDetector")
  rr <- list(
    anchor_center_sec = 0.030,
    anchor_spread_log = 0.30,
    anchor_n = 8L,
    anchor_confidence = 0.60,
    source = "manual_burst",
    method = "manual burst anchor"
  )

  near <- score_fun(0.031, rr = rr)
  far <- score_fun(0.080, rr = rr)

  expect_true(isTRUE(near$soft_support))
  expect_false(isTRUE(far$soft_support))
  expect_gt(near$score, far$score)
  expect_lte(abs(near$score), 0.45)
  expect_lte(abs(far$score), 0.12)
})
