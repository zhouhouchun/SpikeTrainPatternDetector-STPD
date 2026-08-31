test_that("Pause strong-tail KDE is label blind and scale equivariant", {
  set.seed(20260830)
  vals <- setNames(lapply(seq_len(6L), function(i) {
    c(
      stats::rlnorm(260, log(0.050), 0.12),
      stats::rlnorm(10, log(0.400), 0.035)
    )
  }), paste0("train_", seq_len(6L)))

  base <- stpd_event_grammar_pause_strong_tail(vals)
  expect_identical(base$status, "resolved_stable_log_kde_upper_tail")
  expect_gte(base$jackknife_activation_rate, 0.80)
  expect_gte(base$upper_tail_train_n, base$upper_tail_train_required)

  for (scale in c(4, 10)) {
    scaled <- stpd_event_grammar_pause_strong_tail(
      lapply(vals, function(x) x * scale)
    )
    expect_identical(scaled$status, base$status)
    expect_equal(
      scaled$threshold_sec, scale * base$threshold_sec,
      tolerance = 1e-10
    )
    expect_equal(scaled$upper_mass, base$upper_mass, tolerance = 1e-12)
    expect_equal(scaled$valley_depth, base$valley_depth, tolerance = 1e-10)
    expect_equal(
      scaled$jackknife_activation_rate,
      base$jackknife_activation_rate,
      tolerance = 1e-12
    )
  }
})

test_that("Pause strong-tail refuses sparse or cluster-concentrated evidence", {
  sparse <- list(
    train_1 = seq(0.01, 0.09, length.out = 60),
    train_2 = seq(0.011, 0.091, length.out = 60)
  )
  sparse_out <- stpd_event_grammar_pause_strong_tail(sparse)
  expect_match(sparse_out$status, "^unresolved_")

  set.seed(20260831)
  concentrated <- setNames(lapply(seq_len(6L), function(i) {
    base <- stats::rlnorm(260, log(0.050), 0.12)
    if (i == 1L) c(base, stats::rlnorm(12, log(0.450), 0.025)) else base
  }), paste0("train_", seq_len(6L)))
  concentrated_out <- stpd_event_grammar_pause_strong_tail(concentrated)
  expect_match(concentrated_out$status, "^unresolved_")
})

test_that("Pause entry and strong thresholds are both wired into detector vp", {
  params <- default_params_sec()
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$effective_bands <- list(
    pause = list(
      seed_lower_sec = 0.080,
      seed_upper_sec = 0.200,
      bridge_upper_sec = 0.200,
      seed_upper_sec_source = "user"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, rep(0.050, 29))),
    ISI_sec = c(NA_real_, rep(0.050, 29)),
    stringsAsFactors = FALSE
  )

  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )
  expect_equal(vp$pause_entry_thr, 0.080)
  expect_equal(vp$pause_thr, 0.080)
  expect_equal(vp$pause_strong_thr, 0.200)
  expect_true(vp$pause_strong_active)
  expect_identical(vp$pause_threshold_source_mode, "user")
})

test_that("an explicit Pause lower bound is the complete one-sided entry gate", {
  params <- default_params_sec()
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$effective_bands <- list(
    pause = list(
      seed_lower_sec = 0.050,
      seed_upper_sec = 0.200,
      bridge_upper_sec = 0.200,
      seed_lower_sec_source = "user",
      seed_upper_sec_source = "user",
      bridge_upper_sec_source = "user"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  isi <- c(rep(0.040, 20L), 0.050, 0.060, rep(0.040, 10L))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )
  out <- stpd_event_core_detect_pause(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )

  expect_equal(vp$pause_entry_thr, 0.050)
  expect_equal(vp$pause_strong_thr, 0.200)
  expect_equal(out$start_isi, 22L)
  expect_equal(out$end_isi, 23L)
  expect_equal(out$pause_effective_threshold_sec, 0.050)
  expect_true(all(out$gap_semantics == "canonical_pause"))
  expect_true(all(out$action == "accept"))
})

test_that("unresolved strong tail does not veto ordinary automatic q90 Pause", {
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    pause = list(
      seed_lower_sec = 0.080,
      seed_upper_sec = 0.200,
      bridge_upper_sec = 0.200,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    pause = list(
      strong_tail = list(status = "unresolved_no_stable_upper_tail")
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  isi <- c(rep(0.050, 30L), 0.300, rep(0.050, 10L))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )
  expect_false(vp$pause_strong_active)
  out <- stpd_event_core_detect_pause(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )
  expect_gte(nrow(out), 1L)
  expect_true(all(out$gap_semantics == "canonical_pause"))
  expect_true(all(out$action == "accept"))
  expect_true(all(
    out$pause_candidate_decision == "accepted"
  ))
  expect_true(all(out$pause_candidate_reason_code ==
    "automatic_q90_entry_strong_tail_unresolved"))
  expect_true(all(out$hard_for_event))
  expect_true(all(out$hard_for_state_direct_support))
  expect_gte(nrow(stpd_event_core_pause_hard_boundaries(out)), 1L)
  expect_gte(nrow(stpd_event_core_final_event_boundaries(out)), 1L)
})

test_that("a stable automatic strong tail refines rather than disables Pause", {
  params <- default_params_sec()
  params$event_grammar$threshold_source_mode <- "histogram"
  params$event_grammar$effective_bands <- list(
    pause = list(
      seed_lower_sec = 0.080,
      seed_upper_sec = 0.200,
      bridge_upper_sec = 0.200,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    pause = list(
      strong_tail = list(status = "resolved_stable_log_kde_upper_tail")
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  isi <- c(rep(0.050, 30L), 0.100, 0.300, rep(0.050, 10L))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )
  out <- stpd_event_core_detect_pause(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )

  expect_true(vp$pause_strong_active)
  expect_equal(out$pause_effective_threshold_sec, 0.200)
  expect_equal(out$start_isi, 33L)
  expect_equal(out$end_isi, 33L)
  expect_identical(
    out$pause_candidate_reason_code,
    "automatic_stable_strong_tail_threshold"
  )
})
