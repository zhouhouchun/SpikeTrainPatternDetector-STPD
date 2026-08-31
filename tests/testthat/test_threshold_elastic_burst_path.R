make_threshold_elastic_train <- function(isi_sec) {
  timestamp_sec <- c(0, cumsum(isi_sec))
  n <- length(timestamp_sec)
  data.frame(
    idx = seq_len(n),
    timestamp_sec = timestamp_sec,
    ISI_sec = c(NA_real_, isi_sec),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )
}

threshold_elastic_candidates <- function(seed_isi = 0.010,
                                         flank_isi = 0.025,
                                         bridge_high = 0.020,
                                         seed_n = 5L) {
  dat <- make_threshold_elastic_train(c(
    0.050, 0.050, flank_isi,
    rep(seed_isi, seed_n),
    flank_isi, 0.050, 0.050
  ))
  params <- default_params_sec()
  vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(
    dat, params, min_isi_sec = 0.0009
  )
  vp$seed_low <- 0.001
  vp$seed_high <- seed_isi + 0.001
  vp$bridge_high <- bridge_high
  vp$S <- 3
  vp$S_possible <- 1.5
  out <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, min_isi_sec = 0.0009, train = "elastic_train"
  )
  list(dat = dat, params = params, vp = vp, candidates = out)
}

test_that("threshold-elastic Burst does not require canonical flank contrast", {
  probe <- threshold_elastic_candidates(seed_isi = 0.010, flank_isi = 0.025)
  target <- probe$candidates[
    probe$candidates$start_isi == 5L & probe$candidates$end_isi == 9L,
    , drop = FALSE
  ]

  expect_equal(nrow(target), 1L)
  expect_identical(target$final_label, "burst")
  expect_identical(target$action, "accept")
  expect_identical(
    target$gate_status,
    "event_grammar_threshold_elastic_burst_event_pass"
  )
  expect_true(target$threshold_elastic_contract_pass)
  expect_true(target$threshold_elastic_support_pass)
  expect_equal(target$threshold_elastic_min_spikes_without_contrast, 6L)
  expect_true(target$threshold_elastic_acceptance)
  expect_false(target$strict_boundary_pass)
  expect_identical(target$contrast_evidence_status, "moderate_two_sided")
  expect_match(
    target$decision_path,
    "threshold_elastic_seed_bridge_contract_pass",
    fixed = TRUE
  )
})

test_that("weak contrast remains evidence rather than a threshold-path veto", {
  probe <- threshold_elastic_candidates(
    seed_isi = 0.018, flank_isi = 0.021, bridge_high = 0.020
  )
  target <- probe$candidates[
    probe$candidates$start_isi == 5L & probe$candidates$end_isi == 9L,
    , drop = FALSE
  ]

  expect_equal(nrow(target), 1L)
  expect_lt(target$pre_ratio_q90, probe$vp$S_possible)
  expect_lt(target$post_ratio_q90, probe$vp$S_possible)
  expect_identical(target$contrast_evidence_status, "weak_or_unavailable")
  expect_identical(target$final_label, "burst")
  expect_true(target$threshold_elastic_acceptance)
})

test_that("short weak candidates remain Review-only", {
  probe <- threshold_elastic_candidates(
    seed_isi = 0.010, flank_isi = 0.025, seed_n = 3L
  )
  target <- probe$candidates[
    probe$candidates$start_isi == 5L & probe$candidates$end_isi == 7L,
    , drop = FALSE
  ]

  expect_equal(nrow(target), 1L)
  expect_identical(target$final_label, "possible_burst")
  expect_true(target$threshold_elastic_contract_pass)
  expect_false(target$threshold_elastic_support_pass)
  expect_false(target$threshold_elastic_acceptance)
})

test_that("threshold-elastic acceptance keeps bridge and distribution guards", {
  probe <- threshold_elastic_candidates(
    seed_isi = 0.018, flank_isi = 0.021, bridge_high = 0.015,
    seed_n = 5L
  )

  expect_false(any(
    probe$candidates$final_label %in% c("burst", "long_burst") &
      probe$candidates$action == "accept"
  ))
  expect_false(any(probe$candidates$threshold_elastic_acceptance))
})

test_that("internal-gap guard rejects a gap-bearing envelope but keeps its Burst child", {
  dat <- make_threshold_elastic_train(c(
    0.080, 0.052,
    0.012, 0.010, 0.011, 0.009, 0.013, 0.010, 0.012, 0.011,
    0.080
  ))
  params <- default_params_sec()
  vp <- SpikeTrainPatternDetector:::stpd_event_core_params_impl(
    dat, params, min_isi_sec = 0.0009
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.018
  # Deliberately emulate the historically inflated bridge band.
  vp$bridge_high <- 0.053
  vp$S <- 1.2
  vp$S_possible <- 1.1

  candidates <- SpikeTrainPatternDetector:::stpd_event_grammar_detect_burst_events_final_impl(
      dat, params, vp, min_isi_sec = 0.0009, train = "gap_guard"
    )
  bad <- candidates[
    candidates$start_isi == 3L & candidates$end_isi == 11L,
    , drop = FALSE
  ]
  good <- candidates[
    candidates$start_isi == 4L & candidates$end_isi == 11L,
    , drop = FALSE
  ]

  expect_equal(nrow(bad), 1L)
  expect_identical(bad$final_label, "reject")
  expect_false(bad$internal_gap_guard_pass)
  expect_gt(bad$internal_gap_max_to_median_ratio, 3.5)
  expect_match(bad$decision_path, "internal_gap_exceeds_ratio_and_burst_left_band")
  expect_equal(nrow(good), 1L)
  expect_identical(good$final_label, "burst")
  expect_true(good$internal_gap_guard_pass)
})

test_that("internal-gap guard rescues an in-band maximum above the local ratio", {
  guard <- SpikeTrainPatternDetector:::stpd_event_grammar_burst_internal_gap_guard(
      c(0.002, 0.002, 0.002, 0.014),
      burst_left_upper = 0.018,
      ratio_max = 3.5
    )

  expect_true(guard$evaluated)
  expect_gt(guard$max_to_median_ratio, 3.5)
  expect_false(guard$ratio_pass)
  expect_true(guard$left_band_rescue)
  expect_true(guard$pass)
  expect_identical(guard$reason, "internal_gap_left_band_rescue")
})

test_that("internal-gap guard is scale invariant", {
  assess <- function(scale) {
    SpikeTrainPatternDetector:::stpd_event_grammar_burst_internal_gap_guard(
        c(0.010, 0.011, 0.009, 0.052) * scale,
        burst_left_upper = 0.018 * scale,
        ratio_max = 3.5
      )
  }
  guards <- lapply(c(1, 10, 100), assess)

  expect_identical(vapply(guards, `[[`, logical(1), "pass"),
                   rep(FALSE, 3))
  expect_equal(
    vapply(guards, `[[`, numeric(1), "max_to_median_ratio"),
    rep(5.2, 3), tolerance = 1e-12
  )
})
