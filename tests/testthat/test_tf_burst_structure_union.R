b2b_structure_union_collector <- function(run_id) {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("d", 64L), collapse = ""),
    "burst_structure_union", "train_1"
  )
  collector
}

b2b_structure_union_run <- function(
    dat, params = default_params(), vp = NULL, min_isi_sec = 0.001,
    run_id = "run_b2b_structure_union") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  params <- effective_params_for_detector(params)
  if (is.null(vp)) {
    vp <- stpd_event_grammar_params_impl(
      dat, params, min_isi_sec, train = "train_1"
    )
  }
  baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, min_isi_sec, "train_1", pipeline = "final"
  )
  collector <- b2b_structure_union_collector(run_id)
  shard <- stpd_candidate_lineage_collector_begin_train(
    collector, "train_1"
  )
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(
      train = "train_1", n_interval_rows = as.integer(nrow(dat)),
      min_isi_sec = as.numeric(min_isi_sec),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
  observed <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, min_isi_sec, "train_1", pipeline = "final",
    candidate_lineage_collector = shard
  )
  list(
    dat = dat, params = params, vp = vp, baseline = baseline,
    observed = observed, collector = collector, shard = shard,
    snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard)
  )
}

b2b_structure_packets <- function() {
  intervals <- c(
    rep(0.080, 3L), rep(0.006, 3L),
    rep(0.080, 3L), rep(0.007, 3L),
    rep(0.080, 3L), rep(0.008, 3L), rep(0.080, 3L)
  )
  timestamp <- c(0, cumsum(intervals))
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, intervals), stringsAsFactors = FALSE
  )
}

test_that("structure-first observations are exhaustive before deterministic cap", {
  dat <- ensure_train_isi_percentiles(
    b2b_structure_packets(), 0.001, force = TRUE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$max_candidates <- 2L
  set.seed(21031)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()
  run <- b2b_structure_union_run(
    dat, params, vp, run_id = "run_b2b_structure_cap"
  )
  expect_identical(run$observed, run$baseline)
  expect_identical(
    serialize(run$observed, NULL, version = 3L),
    serialize(run$baseline, NULL, version = 3L)
  )
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)

  pre <- run$snapshot$observations$burst_structure_first_pre_cap_v1
  post <- run$snapshot$observations$burst_structure_first_post_cap_v1
  receipt <- run$snapshot$observations$
    burst_structure_first_scan_receipt_v1
  expect_identical(nrow(pre), 3L)
  expect_identical(pre$detector_start_row, c(5L, 11L, 17L))
  expect_identical(pre$detector_end_row, c(7L, 13L, 19L))
  expect_identical(pre$start_isi, c(4L, 10L, 16L))
  expect_identical(pre$end_isi, c(6L, 12L, 18L))
  independent_order <- order(
    -pre$priority, -pre$score, pre$detector_start_row,
    pre$detector_end_row, pre$proposal_ordinal,
    method = "radix", na.last = TRUE
  )
  independent_rank <- integer(nrow(pre))
  independent_rank[independent_order] <- seq_len(nrow(pre))
  expect_identical(pre$deterministic_cap_rank, independent_rank)
  expect_identical(pre$retained_by_cap, c(TRUE, TRUE, FALSE))
  expect_identical(pre$post_cap_ordinal, c(1L, 2L, NA_integer_))
  expect_identical(
    pre$cap_decision, c("retained", "retained", "truncated_by_cap")
  )
  expect_identical(post$proposal_ordinal, c(1L, 2L))
  expect_identical(post$source_payload_sha256,
                   pre$source_payload_sha256[1:2])
  expect_identical(receipt$applicability_status,
                   "scan_exhausted_with_proposals")
  expect_identical(receipt$expected_window_n, receipt$visited_window_n)
  expect_identical(receipt$pre_cap_proposal_n, 3L)
  expect_identical(receipt$retained_n, 2L)
  expect_identical(receipt$truncated_n, 1L)
  expect_true(receipt$scan_exhausted)
  expect_identical(receipt$structure_cap_status, "applied_complete")
})

test_that("union preserves two-route provenance and caps observed inputs only", {
  dat <- ensure_train_isi_percentiles(
    stpd_golden_test_dataset("middle_burst")$trains$train_1,
    0.001, force = TRUE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$max_candidates <- 1L
  run <- b2b_structure_union_run(
    dat, params, vp, run_id = "run_b2b_union_cap"
  )
  expect_identical(run$observed, run$baseline)
  pre <- run$snapshot$observations$burst_union_pre_cap_v1
  post <- run$snapshot$observations$burst_union_post_cap_v1
  receipt <- run$snapshot$observations$burst_union_rank_receipt_v1
  expect_identical(nrow(pre), 2L)
  expect_identical(
    pre$source_route, c("structure_first", "threshold_centred")
  )
  expect_identical(pre$source_ordinal, c(1L, 1L))
  expect_identical(pre$start_isi, c(3L, 3L))
  expect_identical(pre$end_isi, c(5L, 5L))
  expect_false(identical(
    pre$source_payload_sha256[[1L]], pre$source_payload_sha256[[2L]]
  ))
  expect_identical(pre$strict_structure_anchor, c(TRUE, FALSE))
  expect_identical(pre$deterministic_cap_rank, c(1L, 2L))
  expect_identical(pre$retained_by_cap, c(TRUE, FALSE))
  expect_identical(post$source_route, "structure_first")
  expect_identical(post$union_ordinal, 1L)
  expect_identical(nrow(run$observed), 1L)
  expect_identical(
    as.character(run$observed$candidate_layer),
    "structure_first_burst_screen"
  )
  expect_true(receipt$ranking_exhausted)
  expect_identical(
    receipt$rank_scope_status, "complete_over_observed_inputs"
  )
  expect_identical(
    receipt$threshold_upstream_status,
    "stage4_incomplete_early_stop"
  )
  expect_identical(
    receipt$overall_universe_status, "candidate_universe_unavailable"
  )
  expect_identical(
    receipt$union_cap_status,
    "applied_complete_over_observed_inputs"
  )

  vp$max_candidates <- 2L
  uncapped <- b2b_structure_union_run(
    dat, params, vp, run_id = "run_b2b_union_same_geometry_uncapped"
  )
  uncapped_pre <- uncapped$snapshot$observations$burst_union_pre_cap_v1
  uncapped_post <- uncapped$snapshot$observations$burst_union_post_cap_v1
  expect_identical(
    uncapped_post$source_route,
    c("structure_first", "threshold_centred")
  )
  expect_identical(uncapped_post$union_ordinal, c(1L, 2L))
  expect_identical(
    uncapped_post$source_payload_sha256,
    uncapped_pre$source_payload_sha256
  )
  expect_identical(
    uncapped_post[c("detector_start_row", "detector_end_row")],
    uncapped_pre[c("detector_start_row", "detector_end_row")]
  )
})

test_that("union strict-anchor rank exactly mirrors the scientific helper", {
  threshold_only <- data.frame(
    candidate_id = c("threshold_strict_anchor", "threshold_high_priority"),
    candidate_layer = c(
      "structure_first_burst_screen", "threshold_centred_screen"
    ),
    candidate_class = c("compatible_anchor", "threshold_candidate"),
    final_label = c("burst", "burst"),
    strict_boundary_pass = c(TRUE, FALSE),
    priority = c(1, 10000), score = c(1, 10000),
    start_isi = c(2L, 6L), end_isi = c(4L, 8L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  vp <- list(max_candidates = 1L)
  scientific <- stpd_event_core_cap_burst_candidate_union(
    structure_first = NULL, threshold_centred = threshold_only, vp = vp
  )
  pre <- stpd_candidate_lineage_union_pre_payload(
    structure_first = NULL, threshold_centred = threshold_only,
    train = "train_1", max_candidates = 1L
  )
  post <- stpd_candidate_lineage_union_post_payload(
    scientific, pre, "train_1"
  )
  expect_identical(scientific$candidate_id, "threshold_strict_anchor")
  expect_identical(pre$source_route, rep("threshold_centred", 2L))
  expect_identical(pre$strict_structure_anchor, c(TRUE, FALSE))
  expect_identical(pre$deterministic_cap_rank, c(1L, 2L))
  expect_identical(pre$retained_by_cap, c(TRUE, FALSE))
  expect_identical(post$source_candidate_id, "threshold_strict_anchor")
})

test_that("structure applicability distinguishes disabled, exhausted-zero and short", {
  middle <- stpd_golden_test_dataset("middle_burst")$trains$train_1
  disabled_params <- default_params()
  disabled_params$spiketrainpattern$burst$structure_first_enabled <- FALSE
  uniform_time <- cumsum(c(0, rep(0.030, 20L)))
  uniform <- data.frame(
    idx = seq_along(uniform_time), timestamp_sec = uniform_time,
    ISI_sec = c(NA_real_, diff(uniform_time)), stringsAsFactors = FALSE
  )
  short <- data.frame(
    idx = 1:2, timestamp_sec = c(0, 0.030),
    ISI_sec = c(NA_real_, 0.030), stringsAsFactors = FALSE
  )
  cases <- list(
    disabled = list(dat = middle, params = disabled_params,
                    status = "not_applied_disabled", exhausted = FALSE),
    empty = list(dat = uniform, params = default_params(),
                 status = "scan_exhausted_zero", exhausted = TRUE),
    short = list(dat = short, params = default_params(),
                 status = "not_applicable_short_train", exhausted = FALSE)
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    run <- b2b_structure_union_run(
      case$dat, case$params, run_id = paste0("run_b2b_", name)
    )
    pre <- run$snapshot$observations$burst_structure_first_pre_cap_v1
    post <- run$snapshot$observations$burst_structure_first_post_cap_v1
    receipt <- run$snapshot$observations$
      burst_structure_first_scan_receipt_v1
    expect_identical(
      pre, stpd_candidate_lineage_empty_hook_payload(
        "burst_structure_first_pre_cap_v1"
      ), info = name
    )
    expect_identical(
      post, stpd_candidate_lineage_empty_hook_payload(
        "burst_structure_first_post_cap_v1"
      ), info = name
    )
    expect_identical(receipt$applicability_status, case$status, info = name)
    expect_identical(receipt$scan_exhausted, case$exhausted, info = name)
    if (identical(name, "disabled")) {
      forged_status <- receipt
      forged_status$applicability_status <-
        "not_applicable_short_train"
      expect_s3_class(
        tryCatch(stpd_candidate_lineage_validate_hook_payload(
          run$shard, "burst_structure_first_scan_receipt_v1",
          forged_status
        ), error = identity),
        "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
      )
    }
    union_pre <- run$snapshot$observations$burst_union_pre_cap_v1
    union_post <- run$snapshot$observations$burst_union_post_cap_v1
    union_receipt <- run$snapshot$observations$burst_union_rank_receipt_v1
    if (name %in% c("empty", "short")) {
      expect_identical(
        union_pre, stpd_candidate_lineage_empty_hook_payload(
          "burst_union_pre_cap_v1"
        ), info = name
      )
      expect_identical(
        union_post, stpd_candidate_lineage_empty_hook_payload(
          "burst_union_post_cap_v1"
        ), info = name
      )
    } else if (nrow(union_pre)) {
      expect_true(
        all(union_pre$source_route == "threshold_centred"), info = name
      )
    }
    expect_identical(
      union_receipt$overall_universe_status,
      "candidate_universe_unavailable", info = name
    )
    expect_identical(run$observed, run$baseline, info = name)
  }
})

test_that("structure-first remains a native root outside the seed band", {
  intervals <- c(
    0.100, 0.100, 0.029, 0.030, 0.028,
    0.100, 0.110, 0.100, 0.090, 0.100
  )
  timestamp <- c(0, cumsum(intervals))
  dat <- data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, intervals), stringsAsFactors = FALSE
  )
  dat <- ensure_train_isi_percentiles(dat, 0.0009, force = TRUE)
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.0009, train = "train_1"
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  run <- b2b_structure_union_run(
    dat, params, vp, min_isi_sec = 0.0009,
    run_id = "run_b2b_structure_native_root"
  )
  structure_pre <- run$snapshot$observations$
    burst_structure_first_pre_cap_v1
  union_pre <- run$snapshot$observations$burst_union_pre_cap_v1
  raw <- run$snapshot$observations$burst_raw_threshold_support_v1
  expect_identical(nrow(structure_pre), 1L)
  expect_gt(structure_pre$intra_q90_sec, vp$bridge_high)
  expect_false(any(raw$seed_band_member[
    raw$isi_index >= structure_pre$start_isi &
      raw$isi_index <= structure_pre$end_isi
  ]))
  expect_identical(nrow(union_pre), 1L)
  expect_identical(union_pre$source_route, "structure_first")
  expect_identical(
    union_pre$source_payload_sha256,
    structure_pre$source_payload_sha256
  )
})

test_that("union never upgrades an unobserved threshold universe", {
  dat <- ensure_train_isi_percentiles(
    b2b_structure_packets(), 0.001, force = TRUE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$max_candidates <- 1L
  run <- b2b_structure_union_run(
    dat, params, vp, run_id = "run_b2b_upstream_unavailable"
  )
  union_pre <- run$snapshot$observations$burst_union_pre_cap_v1
  receipt <- run$snapshot$observations$burst_union_rank_receipt_v1
  expect_gt(nrow(union_pre), nrow(run$observed))
  expect_identical(receipt$rank_scope_status,
                   "complete_over_observed_inputs")
  expect_identical(receipt$threshold_upstream_status,
                   "stage4_incomplete_early_stop")
  expect_identical(receipt$overall_universe_status,
                   "candidate_universe_unavailable")
  expect_false(any(grepl("direct_complete", unlist(receipt), fixed = TRUE)))
})

test_that("structure and union observation payload tampering fails atomically", {
  dat <- ensure_train_isi_percentiles(
    b2b_structure_packets(), 0.001, force = TRUE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$max_candidates <- 2L
  run <- b2b_structure_union_run(
    dat, params, vp, run_id = "run_b2b_tamper"
  )
  before <- stpd_candidate_lineage_collector_observation_snapshot(run$shard)
  bad_structure <- before$observations$burst_structure_first_pre_cap_v1
  bad_structure$deterministic_cap_rank[c(1L, 2L)] <- c(2L, 1L)
  expect_s3_class(
    tryCatch(stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_structure_first_pre_cap_v1", bad_structure
    ), error = identity),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  bad_post <- before$observations$burst_structure_first_post_cap_v1[-1, ,
                                                                    drop = FALSE]
  expect_s3_class(
    tryCatch(stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_structure_first_post_cap_v1", bad_post
    ), error = identity),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  bad_receipt <- before$observations$burst_union_rank_receipt_v1
  bad_receipt$overall_universe_status <- "direct_complete"
  expect_s3_class(
    tryCatch(stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_union_rank_receipt_v1", bad_receipt
    ), error = identity),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  forged_scan_counts <- before$observations$
    burst_structure_first_scan_receipt_v1
  forged_scan_counts$expected_window_n <-
    forged_scan_counts$expected_window_n + 1L
  forged_scan_counts$visited_window_n <-
    forged_scan_counts$visited_window_n + 1L
  expect_s3_class(
    tryCatch(stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_structure_first_scan_receipt_v1",
      forged_scan_counts
    ), error = identity),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  forged_structure_k <- before$observations$
    burst_structure_first_scan_receipt_v1
  forged_structure_k$max_candidates <- 999L
  expect_s3_class(
    tryCatch(stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_structure_first_scan_receipt_v1",
      forged_structure_k
    ), error = identity),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  forged_union_k <- before$observations$burst_union_rank_receipt_v1
  forged_union_k$max_candidates <- 999L
  expect_s3_class(
    tryCatch(stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_union_rank_receipt_v1", forged_union_k
    ), error = identity),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  forged_scientific <- run$observed
  forged_scientific$priority[[1L]] <-
    forged_scientific$priority[[1L]] + 123
  expect_s3_class(
    tryCatch(stpd_candidate_lineage_union_post_payload(
      forged_scientific,
      before$observations$burst_union_pre_cap_v1,
      "train_1"
    ), error = identity),
    "stpd_candidate_lineage_collector_union_post_not_closed"
  )
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before
  )
})

test_that("collector-off cannot enter structure or union observation helpers", {
  dat <- ensure_train_isi_percentiles(
    stpd_golden_test_dataset("middle_burst")$trains$train_1,
    0.001, force = TRUE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final"
  )
  short <- data.frame(
    idx = 1:2, timestamp_sec = c(0, 0.030),
    ISI_sec = c(NA_real_, 0.030), stringsAsFactors = FALSE
  )
  short <- ensure_train_isi_percentiles(short, 0.001, force = TRUE)
  short_vp <- stpd_event_grammar_params_impl(
    short, params, 0.001, train = "train_1"
  )
  short_baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    short, params, short_vp, 0.001, "train_1", pipeline = "final"
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_capture_structure_first = function(...) {
      stop("structure observation entered while collector is off")
    },
    stpd_candidate_lineage_structure_pre_payload = function(...) {
      stop("structure payload built while collector is off")
    },
    stpd_candidate_lineage_capture_burst_union = function(...) {
      stop("union observation entered while collector is off")
    },
    stpd_candidate_lineage_union_pre_payload = function(...) {
      stop("union payload built while collector is off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final",
    candidate_lineage_collector = NULL
  )
  expect_identical(observed, baseline)
  expect_identical(
    serialize(observed, NULL, version = 3L),
    serialize(baseline, NULL, version = 3L)
  )
  short_observed <- stpd_event_grammar_detect_burst_events_dispatch(
    short, params, short_vp, 0.001, "train_1", pipeline = "final",
    candidate_lineage_collector = NULL
  )
  expect_identical(short_observed, short_baseline)
})
