test_that("automatic Broad HFS histogram band is scale equivariant", {
  vals_by_train <- list(
    train_a = c(
      rep(0.010, 8), rep(0.018, 12), rep(0.030, 20),
      rep(0.050, 8), rep(0.120, 2)
    ),
    train_b = c(
      rep(0.012, 10), rep(0.022, 10), rep(0.034, 20),
      rep(0.055, 8), rep(0.140, 2)
    )
  )
  base <- stpd_event_grammar_histogram_suggest(
    vals_by_train, min_isi_sec = 0.001, bin_width_sec = 0.005
  )
  pooled <- unlist(vals_by_train, use.names = FALSE)

  expect_equal(
    base$high_frequency_spiking$seed_upper_sec,
    as.numeric(stats::quantile(pooled, 0.25, names = FALSE, type = 7))
  )
  expect_equal(
    base$high_frequency_spiking$bridge_upper_sec,
    as.numeric(stats::quantile(pooled, 0.75, names = FALSE, type = 7))
  )
  expect_equal(
    base$high_frequency_spiking$connector_upper_sec,
    min(
      as.numeric(stats::quantile(pooled, 0.90, names = FALSE, type = 7)),
      1.35 * as.numeric(stats::quantile(pooled, 0.75, names = FALSE, type = 7))
    )
  )
  expect_true(base$high_frequency_spiking$available)
  expect_true(base$high_frequency_spiking$requires_candidate_local_background)
  expect_false(base$high_frequency_spiking$promotable_to_user_override)
  expect_match(
    base$high_frequency_spiking$envelope_proposal$status,
    "^(resolved_stable_lower_tail_valley|fallback_q75_)"
  )
  expect_true(
    "jackknife_full_to_fold_center_log_deviation" %in%
      names(base$high_frequency_spiking$envelope_proposal)
  )
  if (identical(
      base$high_frequency_spiking$envelope_proposal$status,
      "resolved_stable_lower_tail_valley")) {
    expect_lte(
      base$high_frequency_spiking$envelope_proposal$
        jackknife_full_to_fold_center_log_deviation,
      0.10
    )
  }

  for (scale in c(4, 10)) {
    scaled <- lapply(vals_by_train, function(x) x * scale)
    resolved <- stpd_event_grammar_histogram_suggest(
      scaled, min_isi_sec = 0.001, bin_width_sec = 0.005
    )
    expect_equal(
      resolved$high_frequency_spiking$seed_upper_sec,
      scale * base$high_frequency_spiking$seed_upper_sec,
      tolerance = 1e-12
    )
    expect_equal(
      resolved$high_frequency_spiking$bridge_upper_sec,
      scale * base$high_frequency_spiking$bridge_upper_sec,
      tolerance = 1e-12
    )
    expect_equal(
      resolved$high_frequency_spiking$connector_upper_sec,
      scale * base$high_frequency_spiking$connector_upper_sec,
      tolerance = 1e-12
    )
  }
})

test_that("resolved Broad HFS band is authoritative over UI defaults", {
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.010,
      bridge_upper_sec = 0.015
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, rep(0.010, 39))),
    ISI_sec = c(NA_real_, rep(0.010, 39)),
    stringsAsFactors = FALSE
  )

  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )

  expect_equal(vp$hf_spiking_short_upper, 0.010)
  expect_equal(vp$hf_spiking_q80_max, 0.015)
  expect_equal(vp$hf_spiking_q90_max, 0.015)
  expect_equal(vp$hf_spiking_epoch_bridge, 0.015)
})

test_that("histogram HFS abstains without a slower local background", {
  for (isi_sec in c(0.020, 0.030, 0.050, 0.100, 0.200)) {
    dat <- data.frame(
      timestamp_sec = cumsum(c(0, rep(isi_sec, 59))),
      ISI_sec = c(NA_real_, rep(isi_sec, 59)),
      stringsAsFactors = FALSE
    )
    params <- default_params_sec()
    params$event_grammar$effective_bands <- list(
      high_frequency_spiking = list(
        seed_lower_sec = max(0.001, isi_sec / 4),
        seed_upper_sec = isi_sec,
        bridge_upper_sec = 1.5 * isi_sec,
        seed_upper_sec_source = "histogram"
      )
    )
    params$event_grammar$histogram_suggest <- list(
      high_frequency_spiking = list(
        available = TRUE,
        requires_candidate_local_background = TRUE,
        status = "proposal_requires_candidate_local_background"
      )
    )
    params$event_grammar$threshold_table <- data.frame()
    vp <- stpd_event_grammar_params_impl(
      dat, params, min_isi_sec = 0.001, train = "homogeneous"
    )
    out <- stpd_event_core_detect_hf_spiking(
      dat, params, vp, min_isi_sec = 0.001, train = "homogeneous"
    )
    expect_equal(nrow(out), 0L, info = paste("ISI", isi_sec))
  }
})

test_that("histogram HFS accepts a localized fast state with slower flanks", {
  isi <- c(rep(0.060, 40), rep(0.015, 40), rep(0.060, 40))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi),
    stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params <- stpd_attach_thresholds_to_params_impl(
    params,
    ds = list(trains = list(localized = dat)),
    min_isi_sec = 0.001
  )
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "localized"
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "localized"
  )
  expect_gte(nrow(out), 1L)
  expect_equal(out$start_isi[1L], 42L)
  expect_equal(out$end_isi[1L], 81L)
  expect_true(all(out$hf_spiking_identifiability_pass))
  expect_true(all(out$hf_spiking_auto_requires_local_background))
})

test_that("HFS lower proposal does not cut a shorter embedded Burst", {
  isi <- c(rep(0.015, 24), rep(0.003, 3), rep(0.015, 32))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi),
    stringsAsFactors = FALSE
  )
  detect_with_lower <- function(lower) {
    params <- default_params_sec()
    params$event_grammar$effective_bands <- list(
      high_frequency_spiking = list(
        seed_lower_sec = lower,
        seed_upper_sec = 0.020,
        bridge_upper_sec = 0.025,
        seed_lower_sec_source = "user",
        seed_upper_sec_source = "user",
        bridge_upper_sec_source = "user"
      )
    )
    params$event_grammar$threshold_table <- data.frame()
    vp <- stpd_event_grammar_params_impl(
      dat, params, min_isi_sec = 0.001, train = "embedded_burst"
    )
    stpd_event_core_detect_hf_spiking(
      dat, params, vp, min_isi_sec = 0.001, train = "embedded_burst"
    )
  }
  low <- detect_with_lower(0.001)
  high <- detect_with_lower(0.005)
  expect_gte(nrow(low), 1L)
  expect_equal(high$start_isi, low$start_isi)
  expect_equal(high$end_isi, low$end_isi)
  expect_true(all(
    high$hf_spiking_seed_lower_role ==
      "audit_only_shorter_embedded_burst_support_retained"
  ))
})

test_that("remote slow activity cannot manufacture local HFS contrast", {
  isi <- c(
    rep(0.200, 60), rep(0.065, 10), rep(0.050, 40),
    rep(0.065, 10), rep(0.200, 60)
  )
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi),
    stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.050,
      bridge_upper_sec = 0.055,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE,
      status = "proposal_requires_candidate_local_background"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "remote_leakage"
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "remote_leakage"
  )
  expect_equal(nrow(out), 0L)
})

test_that("any histogram-sourced HFS field preserves the auto guard", {
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, rep(0.020, 59))),
    ISI_sec = c(NA_real_, rep(0.020, 59)),
    stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.030,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "user",
      bridge_upper_sec_source = "user"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "mixed_source"
  )
  expect_true(vp$hf_spiking_auto_requires_local_background)
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "mixed_source"
  )
  expect_equal(nrow(out), 0L)
})

test_that("q25 fast cores separated by bounded proposal support form one HFS envelope", {
  central <- c(
    rep(0.018, 3), rep(0.030, 4),
    rep(0.019, 3), rep(0.029, 5),
    rep(0.018, 3), rep(0.031, 4),
    rep(0.019, 3), rep(0.028, 5)
  )
  isi <- c(rep(0.080, 30), central, rep(0.080, 30))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi),
    stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.035,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE,
      status = "proposal_requires_candidate_local_background"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "q25_q50_envelope"
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "q25_q50_envelope"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$start_isi, 32L)
  expect_equal(out$end_isi, 61L)
  expect_gte(out$hf_spiking_fast_core_isi_count, 12L)
  expect_gte(out$hf_spiking_fast_core_fraction, 0.35)
  expect_true(out$hf_spiking_envelope_compactness_pass)
  expect_identical(
    out$hf_spiking_support_semantics,
    "q25_anchor_stable_valley_or_q75_proposal_envelope"
  )
})

test_that("proposal-envelope expansion rolls back before a Tonic-like intrusion", {
  hfs <- c(rep(c(0.018, 0.030), 11), 0.018, 0.018)
  tonic_like_tail <- rep(0.034, 30)
  isi <- c(rep(0.080, 30), hfs, tonic_like_tail, rep(0.080, 30))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi),
    stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.035,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE,
      status = "proposal_requires_candidate_local_background"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "q50_intrusion_rollback"
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001,
    train = "q50_intrusion_rollback"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$start_isi, 32L)
  expect_equal(out$end_isi, 55L)
  expect_true(out$hf_spiking_envelope_rollback_to_intrusion_onset)
  expect_equal(out$hf_spiking_envelope_right_intrusion_start_isi, 56L)
  expect_equal(out$hf_spiking_envelope_pre_rollback_end_isi, 85L)
  expect_true(out$hf_spiking_fast_core_gate_pass)
})

test_that("an internal Tonic-supported core drought rolls back at its entrance", {
  isi <- c(rep(0.018, 5), rep(0.060, 30), rep(0.019, 5))
  core <- isi <= 0.020
  tonic_like <- c(rep(FALSE, 5), rep(TRUE, 30), rep(FALSE, 5))
  out <- stpd_event_grammar_hf_seed_anchored_envelope_runs(
    core_flag = core,
    envelope_flag = rep(TRUE, length(isi)),
    isi = isi,
    valid = rep(TRUE, length(isi)),
    envelope_upper = 0.070,
    min_core_count = 3L,
    min_core_fraction = 0.10,
    min_envelope_fraction = 0.75,
    q80_ratio_max = 1,
    q90_ratio_max = 1,
    max_unanchored_boundary_isi = 5L,
    tonic_like_flag = tonic_like,
    tonic_upshift_ratio_min = 1.35
  )

  expect_equal(nrow(out), 2L)
  expect_equal(out$start_isi, c(1L, 36L))
  expect_equal(out$end_isi, c(5L, 40L))
  expect_true(all(out$rollback_to_intrusion_onset))
  expect_equal(unique(out$internal_intrusion_start_isi), 6L)
  expect_equal(unique(out$internal_intrusion_end_isi), 35L)
})

test_that("a q25 drought alone does not cut Broad HFS support", {
  isi <- c(rep(0.018, 5), rep(0.034, 30), rep(0.019, 5))
  out <- stpd_event_grammar_hf_seed_anchored_envelope_runs(
    core_flag = isi <= 0.020,
    envelope_flag = rep(TRUE, length(isi)),
    isi = isi,
    valid = rep(TRUE, length(isi)),
    envelope_upper = 0.040,
    min_core_count = 3L,
    min_core_fraction = 0.10,
    min_envelope_fraction = 0.75,
    q80_ratio_max = 1,
    q90_ratio_max = 1,
    max_unanchored_boundary_isi = 5L,
    tonic_like_flag = rep(FALSE, length(isi)),
    tonic_upshift_ratio_min = 1.35
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$start_isi, 1L)
  expect_equal(out$end_isi, 40L)
  expect_false(out$rollback_to_intrusion_onset)
})

test_that("automatic connector proposal is an inclusive upper ceiling", {
  run_case <- function(gap, max_connector_n = 3L) {
    central <- c(rep(c(0.018, 0.040), 5), gap,
                 rep(c(0.018, 0.040), 5))
    isi <- c(rep(0.120, 30), central, rep(0.120, 30))
    dat <- data.frame(
      timestamp_sec = cumsum(c(0, isi)),
      ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
    )
    params <- default_params_sec()
    params$event_grammar$effective_bands <- list(
      high_frequency_spiking = list(
        seed_lower_sec = 0.001,
        seed_upper_sec = 0.020,
        bridge_upper_sec = 0.050,
        fast_core_upper_sec = 0.020,
        envelope_upper_sec = 0.050,
        connector_upper_sec = 0.055,
        seed_lower_sec_source = "histogram",
        seed_upper_sec_source = "histogram",
        bridge_upper_sec_source = "histogram"
      )
    )
    params$event_grammar$histogram_suggest <- list(
      high_frequency_spiking = list(
        available = TRUE,
        requires_candidate_local_background = TRUE,
        status = "proposal_requires_candidate_local_background"
      )
    )
    params$event_grammar$threshold_table <- data.frame()
    vp <- stpd_event_grammar_params_impl(
      dat, params, min_isi_sec = 0.001, train = "connector_cap"
    )
    vp$hf_spiking_max_consec_large <- max_connector_n
    stpd_event_core_detect_hf_spiking(
      dat, params, vp, min_isi_sec = 0.001, train = "connector_cap"
    )
  }

  expect_equal(nrow(run_case(0.055)), 1L)
  expect_equal(nrow(run_case(0.056)), 0L)
  expect_equal(nrow(run_case(0.055, max_connector_n = 0L)), 0L)
})

test_that("long unanchored proposal-envelope runs are review-only and cannot bootstrap AUTO HFS", {
  central <- c(
    rep(c(0.018, 0.040), 5), 0.055,
    rep(0.040, 30), 0.055,
    rep(c(0.018, 0.040), 5)
  )
  isi <- c(rep(0.120, 30), central, rep(0.120, 30))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.050,
      fast_core_upper_sec = 0.020,
      envelope_upper_sec = 0.050,
      connector_upper_sec = 0.060,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE,
      status = "proposal_requires_candidate_local_background"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "unanchored_absorption"
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001,
    train = "unanchored_absorption"
  )

  expect_gte(nrow(out), 1L)
  expect_true(all(out$action == "abstain"))
  expect_true(all(out$hf_spiking_review_only))
  expect_true(all(grepl(
    "proposal_envelope_component_exceeds_unanchored_auto_budget",
    out$hf_spiking_review_reason, fixed = TRUE
  )))
  selected <- stpd_event_core_weighted_select(
    out, patterns = "high_frequency_spiking"
  )
  expect_false(any(selected$selected_for_auto))
})

test_that("one-sided record-edge HFS evidence is retained for review but not AUTO", {
  central <- rep(c(0.018, 0.040), 15)
  isi <- c(central, rep(0.120, 30))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.050,
      fast_core_upper_sec = 0.020,
      envelope_upper_sec = 0.050,
      connector_upper_sec = 0.060,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE,
      status = "proposal_requires_candidate_local_background"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "record_edge_review"
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "record_edge_review"
  )

  expect_equal(nrow(out), 1L)
  expect_identical(out$action, "abstain")
  expect_true(out$hf_spiking_review_only)
  expect_match(
    out$hf_spiking_review_reason,
    "bilateral_local_background_not_estimable_record_edge_or_short_flank",
    fixed = TRUE
  )
  selected <- stpd_event_core_weighted_select(
    out, patterns = "high_frequency_spiking"
  )
  expect_false(any(selected$selected_for_auto))
})

test_that("automatic HFS cannot disable its local-background guard via cache metadata", {
  isi <- rep(0.020, 80)
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.030,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = FALSE,
      status = "malformed_cached_contract"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "malformed_cache"
  )
  expect_true(vp$hf_spiking_auto_requires_local_background)
  expect_false(vp$hf_spiking_background_contract_consistent)
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "malformed_cache"
  )
  expect_equal(nrow(out), 0L)
})

test_that("automatic HFS requires bilateral stochastic as well as median separation", {
  left <- c(rep(0.050, 4), rep(0.200, 6))
  candidate <- c(rep(0.020, 7), rep(0.100, 13))
  right <- c(rep(0.200, 6), rep(0.050, 4))
  isi <- c(left, candidate, right)
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.110,
      fast_core_upper_sec = 0.020,
      envelope_upper_sec = 0.110,
      connector_upper_sec = 0.150,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE,
      status = "proposal_requires_candidate_local_background"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "overlapping_flanks"
  )
  expect_lt(
    stpd_event_grammar_probability_faster(candidate, left), 0.75
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001,
    train = "overlapping_flanks"
  )
  expect_equal(nrow(out), 0L)
})

test_that("only ISIs above the proposal envelope spend HFS connector budget", {
  central <- c(
    rep(c(0.018, 0.040), 8),
    rep(c(0.018, 0.060, 0.040), 5)
  )
  isi <- c(rep(0.120, 30), central, rep(0.120, 30))
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi), stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.050,
      fast_core_upper_sec = 0.020,
      envelope_upper_sec = 0.050,
      connector_upper_sec = 0.070,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = TRUE,
      requires_candidate_local_background = TRUE,
      status = "proposal_requires_candidate_local_background"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "connector_budget"
  )
  out <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "connector_budget"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$hf_spiking_connector_budget_count, 5L)
  expect_equal(out$hf_spiking_connector_budget_threshold_sec, 0.050)
  expect_equal(out$hf_spiking_connector_upper_sec, 0.070)
  expect_gt(out$hf_spiking_envelope_fraction, 0.75)
})

test_that("legacy HFT cannot auto-promote without a selected Broad-HFS parent", {
  isi <- rep(0.020, 80)
  dat <- data.frame(
    timestamp_sec = cumsum(c(0, isi)),
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE
  )
  params <- default_params_sec()
  params$detector$patterns_to_run <- "high_frequency_tonic"
  params$event_grammar$effective_bands <- list(
    high_frequency_spiking = list(
      seed_lower_sec = 0.001,
      seed_upper_sec = 0.020,
      bridge_upper_sec = 0.030,
      seed_lower_sec_source = "histogram",
      seed_upper_sec_source = "histogram",
      bridge_upper_sec_source = "histogram"
    )
  )
  params$event_grammar$histogram_suggest <- list(
    high_frequency_spiking = list(
      available = FALSE,
      requires_candidate_local_background = TRUE,
      status = "homogeneous_histogram_abstain"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  out <- stpd_detect_train_hf_protected_impl(
    dat, params, min_isi_sec = 0.001, train = "hft_without_parent",
    lock_manual = FALSE
  )

  expect_false(any(out$pattern_auto %in% c(
    "high_frequency_tonic", "high_frequency_irregular_state"
  )))
  legacy <- attr(out, "legacy_hft_diagnostic")
  expect_s3_class(legacy, "data.frame")
  expect_gt(nrow(legacy), 0L)
})
