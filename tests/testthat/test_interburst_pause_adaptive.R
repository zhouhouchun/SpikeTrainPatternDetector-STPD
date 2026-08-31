interburst_pause_train <- function(background_isi = 0.010, gap_isi = 0.060) {
  isi <- c(NA_real_, rep(background_isi, 3), rep(0.006, 3), gap_isi,
           rep(0.006, 3), rep(background_isi, 4))
  data.frame(
    timestamp_sec = cumsum(replace(isi, 1L, 0)),
    ISI_sec = isi,
    pattern_manual = "",
    pattern_manual_negative = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
}

interburst_pause_bursts <- function() {
  data.frame(
    candidate_id = c("left_burst", "right_burst"),
    final_label = c("burst", "burst"),
    action = c("accept", "accept"),
    start_isi = c(5L, 9L),
    end_isi = c(7L, 11L),
    selected_for_auto = TRUE,
    stringsAsFactors = FALSE
  )
}

interburst_pause_wide_fixture <- function(gap_n = 4L, background_isi = 0.010,
                                          largest_gap_isi = 0.060) {
  stopifnot(gap_n >= 1L)
  gap <- rep(0.012, gap_n)
  gap[ceiling(gap_n / 2)] <- largest_gap_isi
  isi <- c(
    NA_real_, rep(background_isi, 3), rep(0.006, 3), gap,
    rep(0.006, 3), rep(background_isi, 4)
  )
  dat <- data.frame(
    timestamp_sec = cumsum(replace(isi, 1L, 0)),
    ISI_sec = isi,
    pattern_manual = "",
    pattern_manual_negative = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  left_end <- 7L
  right_start <- left_end + gap_n + 1L
  bursts <- data.frame(
    candidate_id = c("left_burst", "right_burst"),
    final_label = c("burst", "burst"),
    decision_action = c("accept", "accept"),
    start_isi = c(5L, right_start),
    end_isi = c(7L, right_start + 2L),
    selected_for_auto = TRUE,
    stringsAsFactors = FALSE
  )
  list(
    dat = dat,
    bursts = bursts,
    largest_isi = left_end + ceiling(gap_n / 2)
  )
}

test_that("inter-burst Pause uses train-specific context below the global absolute seed", {
  p <- default_params_sec()
  dat <- interburst_pause_train(background_isi = 0.010, gap_isi = 0.060)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)

  out <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "fast_train",
    burst_candidates = interburst_pause_bursts()
  )

  hit <- out[out$start_isi == 8L & out$end_isi == 8L, , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$decision_path, "interburst_structural_non_bridge_gap")
  expect_identical(hit$pause_context_kind, "between_consecutive_burst_events")
  expect_lt(hit$pause_effective_threshold_sec, p$pause$T_seed)
})

test_that("a structural inter-burst Pause is retained in a slower train", {
  p <- default_params_sec()
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.060)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)

  out <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "slow_train",
    burst_candidates = interburst_pause_bursts()
  )

  hit <- out[out$start_isi == 8L & out$end_isi == 8L, , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$decision_path, "interburst_structural_non_bridge_gap")
})

test_that("a short absolute non-bridge gap is an inter-burst Pause", {
  p <- default_params_sec()
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.020)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)

  expect_lt(0.020, p$pause$T_seed)
  expect_gt(0.020, vp$bridge_high)
  out <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "slow_train",
    burst_candidates = interburst_pause_bursts()
  )

  hit <- out[out$start_isi == 8L & out$end_isi == 8L, , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_true(hit$pause_context_is_below_generic_pause_threshold)
  expect_identical(hit$pause_context_rule, "non_bridge_gap_with_flanking_burst_contrast")
})

test_that("a Burst-bridge gap is not relabelled as an inter-burst Pause", {
  p <- default_params_sec()
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.012)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)

  expect_lte(0.012, vp$bridge_high)
  out <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "slow_train",
    burst_candidates = interburst_pause_bursts()
  )

  hit <- out[out$start_isi == 8L & out$end_isi == 8L, , drop = FALSE]
  expect_equal(nrow(hit), 0L)
})

test_that("Burst bridge ownership is frozen before inter-burst Pause generation", {
  p <- default_params_sec()
  p$event_grammar$structural_burst_episode_bridge_factor <- 2
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.020)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)
  frozen <- stpd_event_core_freeze_burst_bridges(
    dat, p, vp, rbind(
      interburst_pause_bursts(),
      data.frame(
        candidate_id = "combined_burst_witness", final_label = "burst",
        action = "accept", start_isi = 5L, end_isi = 11L,
        selected_for_auto = FALSE, stringsAsFactors = FALSE
      )
    ), min_isi_sec = 0.001,
    train = "bridge_owned_train"
  )

  expect_equal(nrow(frozen), 1L)
  expect_equal(frozen$start_isi, 5L)
  expect_equal(frozen$end_isi, 11L)
  expect_true(any(frozen$start_isi == 5L & frozen$end_isi == 11L))
  out <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "bridge_owned_train",
    burst_candidates = frozen
  )
  expect_false(any(out$start_isi == 8L & out$end_isi == 8L))
})

test_that("A single bounded expanded ISI creates Burst support before Pause", {
  p <- default_params_sec()
  p$event_grammar$structural_burst_episode_bridge_factor <- 2
  p$event_grammar$single_expanded_bridge_enabled <- TRUE
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.020)
  vp <- stpd_event_grammar_params_impl(dat, p, min_isi_sec = 0.001)
  burst_candidates <- stpd_event_grammar_detect_burst_events(
    dat, p, vp, min_isi_sec = 0.001, train = "expanded_bridge_train"
  )
  bridge_hit <- burst_candidates[
    burst_candidates$candidate_class == "event_grammar_single_expanded_bridge_burst" &
      burst_candidates$start_isi == 5L & burst_candidates$end_isi == 11L,
    , drop = FALSE
  ]
  expect_equal(nrow(bridge_hit), 1L)
  expect_identical(bridge_hit$final_label, "burst")
  expect_identical(bridge_hit$action, "accept")
  frozen <- stpd_event_core_freeze_burst_bridges(
    dat, p, vp, burst_candidates, min_isi_sec = 0.001,
    train = "expanded_bridge_train"
  )
  out <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "expanded_bridge_train",
    burst_candidates = frozen
  )
  expect_false(any(out$start_isi == 8L & out$end_isi == 8L))
})

test_that("An ordinary in-band bridge does not fuse neighbouring Burst episodes", {
  p <- default_params_sec()
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.012)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)
  frozen <- stpd_event_core_freeze_burst_bridges(
    dat, p, vp, interburst_pause_bursts(), min_isi_sec = 0.001,
    train = "ordinary_bridge"
  )
  expect_equal(nrow(frozen), 2L)
})

test_that("a canonical Pause blocks Burst bridge ownership before materialization", {
  p <- default_params_sec()
  p$event_grammar$structural_burst_episode_bridge_factor <- 10
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.180)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)
  pauses <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "canonical_boundary",
    burst_candidates = NULL
  )
  boundaries <- SpikeTrainPatternDetector:::stpd_event_core_pause_hard_boundaries(
    pauses
  )
  expect_true(any(boundaries$start_isi == 8L))

  candidates <- rbind(
    interburst_pause_bursts(),
    data.frame(
      candidate_id = "crossing_witness", final_label = "burst",
      action = "accept", start_isi = 5L, end_isi = 11L,
      selected_for_auto = FALSE, stringsAsFactors = FALSE
    )
  )
  candidates <- SpikeTrainPatternDetector:::stpd_event_core_apply_hard_boundaries_to_burst_candidates(
    candidates, boundaries
  )
  expect_identical(
    candidates$action[candidates$candidate_id == "crossing_witness"],
    "reject"
  )
  frozen <- stpd_event_core_freeze_burst_bridges(
    dat, p, vp, candidates, min_isi_sec = 0.001,
    train = "canonical_boundary", hard_boundaries = boundaries
  )
  expect_equal(nrow(frozen), 2L)
  expect_false(any(frozen$start_isi <= 8L & frozen$end_isi >= 8L))
})

test_that("Pause evidence blocked by Burst ownership remains in the ledger", {
  p <- default_params_sec()
  dat <- interburst_pause_train(background_isi = 0.040, gap_isi = 0.180)
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)
  burst_support <- data.frame(
    candidate_id = "frozen_spanning_burst", final_label = "burst",
    action = "accept", start_isi = 5L, end_isi = 11L,
    selected_for_auto = TRUE, stringsAsFactors = FALSE
  )
  out <- stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "ledger_train",
    burst_candidates = burst_support
  )
  hit <- out[out$start_isi == 8L & out$end_isi == 8L, , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$gap_semantics, "ambiguous_gap")
  expect_identical(hit$pause_candidate_decision,
                   "blocked_by_frozen_burst_support")
  expect_identical(hit$action, "reject")
  expect_false(hit$hard_for_event)
  selected <- SpikeTrainPatternDetector:::stpd_event_core_weighted_select(
    hit, patterns = "pause"
  )
  expect_false(selected$selected_for_auto)
})

test_that("inter-burst context does not mutate frozen Burst evidence", {
  p <- default_params_sec()
  dat <- interburst_pause_train()
  vp <- stpd_event_core_params_impl(dat, p, min_isi_sec = 0.001)
  bursts <- interburst_pause_bursts()
  before <- serialize(bursts, NULL, version = 3)

  invisible(stpd_event_core_detect_pause(
    dat, p, vp, min_isi_sec = 0.001, train = "fast_train",
    burst_candidates = bursts
  ))

  expect_identical(serialize(bursts, NULL, version = 3), before)
})

test_that("a structural Pause searches a bounded four-ISI inter-burst gap", {
  p <- default_params_sec()
  fixture <- interburst_pause_wide_fixture(gap_n = 4L)
  vp <- stpd_event_core_params_impl(fixture$dat, p, min_isi_sec = 0.001)

  out <- stpd_event_core_detect_pause(
    fixture$dat, p, vp, min_isi_sec = 0.001, train = "fast_train",
    burst_candidates = fixture$bursts
  )

  hit <- out[
    out$start_isi == fixture$largest_isi &
      out$end_isi == fixture$largest_isi,
    , drop = FALSE
  ]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$decision_path, "interburst_structural_non_bridge_gap")
  expect_identical(hit$pause_context_gap_isi_n, 4L)
  expect_identical(hit$pause_context_gap_isi_cap, 4L)
})

test_that("train-adaptive Pause does not expand beyond the existing bridge-count safeguard", {
  p <- default_params_sec()
  fixture <- interburst_pause_wide_fixture(gap_n = 5L)
  vp <- stpd_event_core_params_impl(fixture$dat, p, min_isi_sec = 0.001)

  out <- stpd_event_core_detect_pause(
    fixture$dat, p, vp, min_isi_sec = 0.001, train = "fast_train",
    burst_candidates = fixture$bursts
  )

  expect_false(any(out$start_isi == fixture$largest_isi))
})

test_that("a wider inter-burst search remains structural rather than train-median gated", {
  p <- default_params_sec()
  fixture <- interburst_pause_wide_fixture(
    gap_n = 4L,
    background_isi = 0.040,
    largest_gap_isi = 0.060
  )
  vp <- stpd_event_core_params_impl(fixture$dat, p, min_isi_sec = 0.001)

  out <- stpd_event_core_detect_pause(
    fixture$dat, p, vp, min_isi_sec = 0.001, train = "slow_train",
    burst_candidates = fixture$bursts
  )

  hit <- out[
    out$start_isi == fixture$largest_isi &
      out$end_isi == fixture$largest_isi,
    , drop = FALSE
  ]
  expect_equal(nrow(hit), 1L)
})

test_that("final Event boundaries add contextual Pause only after ownership", {
  pause <- data.frame(
    start_isi = c(8L, 20L, 30L),
    end_isi = c(8L, 20L, 30L),
    gap_semantics = c(
      "contextual_interburst_pause", "canonical_pause", "ambiguous_gap"
    ),
    hard_for_event = c(TRUE, TRUE, FALSE),
    action = c("accept", "accept", "reject"),
    stringsAsFactors = FALSE
  )
  raw <- stpd_event_core_pause_hard_boundaries(pause)
  final <- stpd_event_core_final_event_boundaries(pause)

  expect_identical(raw$start_isi, 20L)
  expect_identical(
    final$start_isi, c(8L, 20L)
  )
  expect_identical(
    final$boundary_kind,
    c("contextual_interburst_pause", "canonical_pause")
  )
})
