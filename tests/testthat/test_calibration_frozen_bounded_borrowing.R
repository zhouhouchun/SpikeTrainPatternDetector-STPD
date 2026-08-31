make_bounded_borrowing_train <- function(
    isi, manual_burst = integer()) {
  timestamp <- c(0, cumsum(isi))
  manual <- rep("", length(timestamp))
  if (length(manual_burst)) manual[manual_burst] <- "burst"
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi), pattern_manual = manual,
    pattern_manual_negative = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
}

test_that("bounded borrowing contract reads calibration trains only", {
  calibration_a <- make_bounded_borrowing_train(
    rep(c(0.008, 0.010, 0.040, 0.080), 20), 2:6
  )
  calibration_b <- make_bounded_borrowing_train(
    rep(c(0.009, 0.011, 0.045, 0.090), 20), 2:6
  )
  validation <- make_bounded_borrowing_train(rep(0.030, 80), 2:20)
  ds <- make_dataset("frozen_borrowing", "synthetic", list(
    calibration_a = calibration_a,
    calibration_b = calibration_b,
    validation = validation
  ), unit_in = "s")
  frozen_1 <- stpd_freeze_thresholds_for_trains(
    ds, default_params_sec(), c("calibration_a", "calibration_b")
  )
  contract_1 <- frozen_1$event_grammar$bounded_borrowing_contract
  expect_true(contract_1$enabled)
  expect_identical(contract_1$calibration_trains,
                   c("calibration_a", "calibration_b"))
  expect_false("validation" %in% contract_1$calibration_trains)
  expect_true(contract_1$frozen_before_validation)
  expect_false(contract_1$validation_labels_read)

  ds$trains$validation$pattern_manual <- "pause"
  frozen_2 <- stpd_freeze_thresholds_for_trains(
    ds, default_params_sec(), c("calibration_a", "calibration_b")
  )
  contract_2 <- frozen_2$event_grammar$bounded_borrowing_contract
  expect_identical(contract_1$calibration_input_sha256,
                   contract_2$calibration_input_sha256)
  expect_identical(frozen_1$event_grammar$effective_bands,
                   frozen_2$event_grammar$effective_bands)

  ds$trains$calibration_a$pattern_manual[[10L]] <- "burst"
  frozen_3 <- stpd_freeze_thresholds_for_trains(
    ds, default_params_sec(), c("calibration_a", "calibration_b")
  )
  expect_false(identical(
    contract_1$calibration_input_sha256,
    frozen_3$event_grammar$bounded_borrowing_contract$calibration_input_sha256
  ))
})

test_that("borrowing is disabled without cross-train manual Burst consensus", {
  trains <- list(
    calibration_a = make_bounded_borrowing_train(rep(0.010, 40)),
    calibration_b = make_bounded_borrowing_train(rep(0.012, 40))
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(burst = list(
    seed_lower_sec = 0.001, seed_upper_sec = 0.010,
    bridge_upper_sec = 0.015, contrast_S = 2.5
  ))
  frozen <- stpd_freeze_bounded_borrowing_contract(params, trains)
  contract <- frozen$event_grammar$bounded_borrowing_contract

  expect_false(contract$enabled)
  expect_identical(contract$manual_burst_evidence_train_n, 0L)
  expect_identical(
    contract$status, "disabled_insufficient_calibration_evidence"
  )
})

test_that("borrowing creates a bounded candidate proposal without mutating thresholds", {
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    burst = list(
      seed_lower_sec = 0.001, seed_upper_sec = 0.010,
      bridge_upper_sec = 0.015, contrast_S = 2.5
    )
  )
  params$event_grammar$bounded_borrowing_contract <- list(
    schema_version = "stpd_calibration_frozen_borrowing_v2",
    enabled = TRUE, status = "calibration_frozen",
    calibration_only = TRUE, validation_labels_read = FALSE,
    permitted_role = "burst_bridge_or_boundary_extension_only",
    seed_band_mutation_allowed = FALSE,
    consensus_bridge_upper_sec = 0.0185,
    base_bridge_upper_sec = 0.015, min_target_valid_isi = 30L,
    max_expansion_ratio = 1.25, calibration_input_sha256 = strrep("a", 64L)
  )
  dat <- make_bounded_borrowing_train(c(
    0.100, 0.008, 0.008, 0.018, 0.100, rep(0.030, 35)
  ))
  vp_before <- stpd_event_core_params_impl(dat, params, 0.001)
  vp_before$seed_low <- 0.001
  vp_before$seed_high <- 0.010
  vp_before$bridge_high <- 0.015
  vp_before$pause_thr <- 0.100
  vp_after <- stpd_apply_bounded_borrowing_to_event_vp(
    vp_before, dat, params, train = "validation", min_isi_sec = 0.001
  )
  expect_identical(vp_after$seed_high, vp_before$seed_high)
  expect_identical(vp_after$bridge_high, vp_before$bridge_high)
  expect_lte(vp_after$cross_train_borrowing_bridge_candidate_sec, 0.015 * 1.25)
  expect_gt(
    vp_after$cross_train_borrowing_bridge_candidate_sec,
    vp_before$bridge_high
  )
  expect_true(vp_after$cross_train_borrowing_applied)
  expect_identical(
    vp_after$cross_train_borrowing_status,
    "proposal_frozen_for_candidate_level_adjudication"
  )

  candidates <- stpd_event_grammar_detect_burst_events(
    dat, params, vp_after, 0.001, "validation"
  )
  borrowed <- candidates[
    candidates$candidate_layer == "event_grammar_burst_event" &
      candidates$start_isi <= 3L & candidates$end_isi >= 5L,
    , drop = FALSE
  ]
  expect_gt(nrow(borrowed), 0L)
  expect_true(all(borrowed$cross_train_borrowing_applied))
  expect_true(all(borrowed$cross_train_borrowing_seed_unchanged))
  # The 18-ms ISI is admitted only as an extension; it remains above seed_high.
  expect_gt(dat$ISI_sec[[5L]], vp_after$seed_high)
})

test_that("canonical Pause and explicit hard thresholds dominate borrowing", {
  params <- default_params_sec()
  params$event_grammar$bounded_borrowing_contract <- list(
    enabled = TRUE, status = "calibration_frozen",
    consensus_bridge_upper_sec = 0.025,
    base_bridge_upper_sec = 0.020, min_target_valid_isi = 8L,
    max_expansion_ratio = 1.25, calibration_input_sha256 = strrep("b", 64L)
  )
  dat <- make_bounded_borrowing_train(rep(0.030, 20))
  vp <- stpd_event_core_params_impl(dat, params, 0.001)
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.020
  vp$pause_thr <- 0.021
  bounded <- stpd_apply_bounded_borrowing_to_event_vp(
    vp, dat, params, train = "validation", min_isi_sec = 0.001
  )
  expect_identical(bounded$bridge_high, vp$bridge_high)
  expect_lt(
    bounded$cross_train_borrowing_bridge_candidate_sec,
    vp$pause_thr
  )
  expect_identical(bounded$seed_high, vp$seed_high)

  params$burst$adaptive_use_train_ranges <- TRUE
  params$burst$adaptive_train_ranges$validation <- list(
    high_sec = 0.019, threshold_mode = "hard_threshold",
    hard_threshold = TRUE
  )
  blocked <- stpd_apply_bounded_borrowing_to_event_vp(
    vp, dat, params, train = "validation", min_isi_sec = 0.001
  )
  expect_false(blocked$cross_train_borrowing_applied)
  expect_identical(
    blocked$cross_train_borrowing_status,
    "blocked_by_explicit_train_hard_threshold"
  )
})

exercise_bounded_borrowing_scale <- function(unit_sec, min_isi_sec) {
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(burst = list(
    seed_lower_sec = min_isi_sec,
    seed_upper_sec = unit_sec,
    bridge_upper_sec = 1.5 * unit_sec,
    contrast_S = 2.5
  ))
  params$event_grammar$bounded_borrowing_contract <- list(
    enabled = TRUE, status = "calibration_frozen",
    consensus_bridge_upper_sec = 1.8 * unit_sec,
    base_bridge_upper_sec = 1.5 * unit_sec,
    min_target_valid_isi = 30L, max_expansion_ratio = 1.25,
    calibration_input_sha256 = strrep("c", 64L)
  )
  dat <- make_bounded_borrowing_train(c(
    10 * unit_sec, 0.8 * unit_sec, 0.8 * unit_sec,
    1.8 * unit_sec, 10 * unit_sec, rep(3 * unit_sec, 35)
  ))
  vp <- stpd_event_core_params_impl(dat, params, min_isi_sec)
  vp$seed_low <- min_isi_sec
  vp$seed_high <- unit_sec
  vp$bridge_high <- 1.5 * unit_sec
  vp$pause_thr <- 12 * unit_sec
  vp <- stpd_apply_bounded_borrowing_to_event_vp(
    vp, dat, params, "validation", min_isi_sec
  )
  candidates <- stpd_event_grammar_detect_burst_events(
    dat, params, vp, min_isi_sec, "validation"
  )
  list(vp = vp, candidates = candidates)
}

test_that("fast-timescale simulation admits only borrowed Burst extension", {
  result <- exercise_bounded_borrowing_scale(0.003, 0.0005)
  expect_identical(result$vp$seed_high, 0.003)
  expect_true(result$vp$cross_train_borrowing_applied)
  expect_true(any(
    result$candidates$candidate_layer == "event_grammar_burst_event" &
      result$candidates$start_isi <= 3L & result$candidates$end_isi >= 5L &
      result$candidates$final_label == "burst"
  ))
})

test_that("medium-timescale simulation admits only borrowed Burst extension", {
  result <- exercise_bounded_borrowing_scale(0.010, 0.001)
  expect_identical(result$vp$seed_high, 0.010)
  expect_true(result$vp$cross_train_borrowing_applied)
  expect_true(any(
    result$candidates$candidate_layer == "event_grammar_burst_event" &
      result$candidates$start_isi <= 3L & result$candidates$end_isi >= 5L &
      result$candidates$final_label == "burst"
  ))
})

test_that("slow-timescale simulation admits only borrowed Burst extension", {
  result <- exercise_bounded_borrowing_scale(0.030, 0.001)
  expect_identical(result$vp$seed_high, 0.030)
  expect_true(result$vp$cross_train_borrowing_applied)
  expect_true(any(
    result$candidates$candidate_layer == "event_grammar_burst_event" &
      result$candidates$start_isi <= 3L & result$candidates$end_isi >= 5L &
      result$candidates$final_label == "burst"
  ))
})

test_that("candidate-level guard rejects expansion that invades a sustained state", {
  params <- default_params_sec()
  vp <- list(long_max_spikes = 15L)

  too_large <- stpd_event_grammar_burst_extension_guard(
    c(rep(0.010, 7), rep(0.018, 8)),
    native_bridge_upper = 0.015,
    candidate_bridge_upper = 0.020,
    n_spikes = 16L,
    vp = vp,
    params = params
  )
  expect_false(too_large$pass)
  expect_match(too_large$reason, "candidate_exceeds_burst_spike_ceiling")
  expect_match(too_large$reason, "borrowed_support_dominates")

  native_large <- stpd_event_grammar_burst_extension_guard(
    rep(0.010, 19),
    native_bridge_upper = 0.015,
    candidate_bridge_upper = 0.020,
    n_spikes = 20L,
    vp = vp,
    params = params
  )
  expect_true(native_large$pass)
  expect_identical(native_large$borrowed_n, 0L)
  expect_true(native_large$size_pass)

  sustained_tail <- stpd_event_grammar_burst_extension_guard(
    c(rep(0.010, 8), rep(0.018, 3)),
    native_bridge_upper = 0.015,
    candidate_bridge_upper = 0.020,
    n_spikes = 12L,
    vp = vp,
    params = params
  )
  expect_false(sustained_tail$pass)
  expect_match(sustained_tail$reason, "sustained_borrowed_run")

  bounded_bridge <- stpd_event_grammar_burst_extension_guard(
    c(0.010, 0.010, 0.018, 0.010, 0.010),
    native_bridge_upper = 0.015,
    candidate_bridge_upper = 0.020,
    n_spikes = 6L,
    vp = vp,
    params = params
  )
  expect_true(bounded_bridge$pass)
})

test_that("sustained borrowed support rolls back to its first ISI", {
  isi <- c(rep(0.010, 7), rep(0.018, 8), 0.100)
  mask <- stpd_event_grammar_burst_intrusion_mask(
    isi = isi,
    valid = rep(TRUE, length(isi)),
    native_bridge_upper = 0.015,
    candidate_bridge_upper = 0.020,
    max_consecutive_borrowed = 2L
  )

  expect_false(any(mask[1:7]))
  expect_true(all(mask[8:15]))
  expect_false(mask[16])
  # The rollback boundary is immediately before the first invaded ISI, not
  # after the detector has already consumed two tolerated extension ISIs.
  expect_identical(max(which(!mask[seq_len(15)])), 7L)
})
