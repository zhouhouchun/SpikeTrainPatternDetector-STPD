stage4_lineage_fixture <- function(intervals = c(
    0.080, 0.006, 0.006, 0.012, 0.006, 0.006, 0.080)) {
  timestamp <- c(0, cumsum(intervals))
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, intervals),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stage4_lineage_collector <- function(run_id) {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("e", 64L), collapse = ""),
    "burst_stage4", "train_1"
  )
  collector
}

stage4_lineage_run <- function(
    intervals = c(0.080, 0.006, 0.006, 0.012, 0.006, 0.006, 0.080),
    max_candidates = 100L, min_spikes = 4L,
    single_connector_enabled = FALSE, manual_negative = FALSE,
    run_id = "run_burst_stage4") {
  dat <- ensure_train_isi_percentiles(
    stage4_lineage_fixture(intervals), 0.001, force = TRUE
  )
  params <- effective_params_for_detector(default_params())
  params$event_grammar$single_expanded_bridge_enabled <-
    isTRUE(single_connector_enabled)
  if (isTRUE(manual_negative)) {
    dat$pattern_manual_negative <- rep("not_burst", nrow(dat))
  }
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  vp$cross_train_borrowing_bridge_candidate_sec <- 0.015
  vp$min_seed_isi_n <- 2L
  vp$min_spikes <- as.integer(min_spikes)
  vp$max_expand <- 10L
  vp$max_candidates <- as.integer(max_candidates)
  baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final"
  )
  collector <- stage4_lineage_collector(run_id)
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
      min_isi_sec = 0.001,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
  observed <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final",
    candidate_lineage_collector = shard
  )
  list(
    dat = dat, params = params, vp = vp, baseline = baseline,
    observed = observed, collector = collector, shard = shard,
    snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard)
  )
}

test_that("Stage-4 records only visited attempts and emitted candidates", {
  set.seed(41021)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()
  run <- stage4_lineage_run(run_id = "run_stage4_complete")
  expect_identical(run$observed, run$baseline)
  expect_identical(
    serialize(run$observed, NULL, version = 3L),
    serialize(run$baseline, NULL, version = 3L)
  )
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)

  observations <- run$snapshot$observations
  attempts <- observations$burst_threshold_stage4_attempts_v1
  candidates <- observations$burst_threshold_stage4_candidates_v1
  receipt <- observations$burst_threshold_stage4_receipt_v1
  union_pre <- observations$burst_union_pre_cap_v1
  threshold_union <- union_pre[
    union_pre$source_route == "threshold_centred", , drop = FALSE
  ]

  expect_identical(attempts$attempt_ordinal, seq_len(9L))
  expect_identical(
    attempts$subroute,
    c(rep("seed_expansion", 8L), "dense_episode")
  )
  expect_identical(
    attempts$terminal_status,
    c(
      "filter_rejected", rep("candidate_emitted", 3L),
      "filter_rejected", rep("candidate_emitted", 2L),
      rep("duplicate_skipped", 2L)
    )
  )
  expect_identical(
    attempts$duplicate_of_attempt_ordinal[c(8L, 9L)], c(4L, 4L)
  )
  expect_identical(candidates$candidate_append_ordinal, seq_len(5L))
  expect_identical(
    candidates$source_attempt_ordinal, c(2L, 3L, 4L, 6L, 7L)
  )
  expect_identical(nrow(threshold_union), nrow(candidates))
  expect_identical(
    threshold_union$source_candidate_id, candidates$candidate_id
  )
  expect_identical(
    threshold_union$source_payload_sha256,
    candidates$source_payload_sha256
  )
  expect_identical(
    threshold_union$detector_start_row, candidates$detector_start_row
  )
  expect_identical(
    threshold_union$detector_end_row, candidates$detector_end_row
  )
  expect_identical(receipt$observed_attempt_n, 9L)
  expect_identical(receipt$emitted_candidate_n, 5L)
  expect_identical(receipt$duplicate_n, 2L)
  expect_identical(receipt$filter_rejected_n, 2L)
  expect_true(receipt$stage4_search_exhausted)
  expect_identical(receipt$coverage_status, "stage4_search_complete")
  expect_identical(receipt$seed_status, "search_exhausted")
  expect_identical(receipt$dense_status, "search_exhausted")
  expect_identical(receipt$connector_status, "not_applied_disabled")
})

test_that("Stage-4 shared K is reported as ordered early stop", {
  run <- stage4_lineage_run(
    max_candidates = 1L, run_id = "run_stage4_early_stop"
  )
  expect_identical(run$observed, run$baseline)
  observations <- run$snapshot$observations
  attempts <- observations$burst_threshold_stage4_attempts_v1
  candidates <- observations$burst_threshold_stage4_candidates_v1
  receipt <- observations$burst_threshold_stage4_receipt_v1
  union_receipt <- observations$burst_union_rank_receipt_v1

  expect_identical(nrow(attempts), 2L)
  expect_identical(
    attempts$terminal_status, c("filter_rejected", "candidate_emitted")
  )
  expect_identical(nrow(candidates), 1L)
  expect_true(receipt$cap_check_triggered)
  expect_false(receipt$stage4_search_exhausted)
  expect_identical(receipt$stop_subroute, "seed_expansion")
  expect_identical(receipt$stop_source_root_ordinal, 1L)
  expect_identical(receipt$stop_rows_n, 1L)
  expect_identical(receipt$seed_status, "truncated_by_shared_cap")
  expect_identical(receipt$dense_status, "not_entered_shared_cap")
  expect_identical(receipt$connector_status, "not_applied_disabled")
  expect_identical(
    receipt$coverage_status, "stage4_incomplete_early_stop"
  )
  expect_identical(
    union_receipt$threshold_upstream_status,
    "stage4_incomplete_early_stop"
  )
  expect_identical(receipt$dense_attempt_n, 0L)
  expect_identical(receipt$connector_attempt_n, 0L)
})

test_that("Stage-4 distinguishes rejected, empty and short searches", {
  rejected <- stage4_lineage_run(
    intervals = c(0.080, 0.006, 0.006, 0.080), min_spikes = 5L,
    run_id = "run_stage4_rejected"
  )
  rejected_obs <- rejected$snapshot$observations
  expect_true(any(
    rejected_obs$burst_threshold_stage4_attempts_v1$terminal_status ==
      "filter_rejected"
  ))
  expect_identical(
    nrow(rejected_obs$burst_threshold_stage4_candidates_v1), 0L
  )
  expect_identical(
    rejected_obs$burst_threshold_stage4_receipt_v1$coverage_status,
    "stage4_search_complete"
  )

  empty <- stage4_lineage_run(
    intervals = rep(0.030, 7L), run_id = "run_stage4_empty"
  )
  empty_obs <- empty$snapshot$observations
  expect_identical(nrow(empty_obs$burst_seed_runs_v1), 0L)
  expect_identical(
    nrow(empty_obs$burst_threshold_stage4_attempts_v1), 0L
  )
  expect_identical(
    empty_obs$burst_threshold_stage4_receipt_v1$seed_status,
    "not_applicable_no_source_roots"
  )
  expect_identical(
    empty_obs$burst_threshold_stage4_receipt_v1$coverage_status,
    "stage4_search_complete"
  )

  short <- stage4_lineage_run(
    intervals = 0.030, run_id = "run_stage4_short"
  )
  short_obs <- short$snapshot$observations
  for (hook in c(
      "burst_threshold_stage4_entry_v1",
      "burst_threshold_stage4_episode_roots_v1",
      "burst_threshold_stage4_attempts_v1",
      "burst_threshold_stage4_candidates_v1")) {
    expect_identical(
      short_obs[[hook]], stpd_candidate_lineage_empty_hook_payload(hook)
    )
  }
  expect_identical(
    short_obs$burst_threshold_stage4_receipt_v1$coverage_status,
    "stage4_not_applicable_short_train"
  )
  expect_identical(
    short_obs$burst_union_rank_receipt_v1$threshold_upstream_status,
    "stage4_not_applicable_short_train"
  )
})

test_that("Stage-4 source, candidate, receipt and union tampering fail", {
  run <- stage4_lineage_run(run_id = "run_stage4_tamper")
  before <- run$snapshot
  observations <- before$observations
  error_class <-
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"

  bad_attempts <- observations$burst_threshold_stage4_attempts_v1
  bad_attempts$duplicate_of_attempt_ordinal[[8L]] <- 3L
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_attempts_v1", bad_attempts
    ), error = identity
  ), error_class)

  forged_reason <- observations$burst_threshold_stage4_attempts_v1
  forged_reason$terminal_reason[[1L]] <- "fabricated_scientific_reason"
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_attempts_v1", forged_reason
    ), error = identity
  ), error_class)

  unreachable <- observations$burst_threshold_stage4_attempts_v1
  unreachable$proposed_start_row[[1L]] <- 2L
  unreachable$proposed_end_row[[1L]] <- 2L
  unreachable$proposed_start_isi[[1L]] <- 1L
  unreachable$proposed_end_isi[[1L]] <- 1L
  unreachable$geometry_key[[1L]] <- "2_2"
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_attempts_v1", unreachable
    ), error = identity
  ), error_class)

  empty_attempts <- stpd_candidate_lineage_empty_hook_payload(
    "burst_threshold_stage4_attempts_v1"
  )
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_attempts_v1", empty_attempts
    ), error = identity
  ), error_class)

  bad_candidates <- observations$burst_threshold_stage4_candidates_v1
  bad_candidates$priority[[1L]] <- bad_candidates$priority[[1L]] + 1
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_candidates_v1", bad_candidates
    ), error = identity
  ), error_class)

  rehashed_candidate <- observations$burst_threshold_stage4_candidates_v1
  rehashed_candidate$priority[[1L]] <-
    rehashed_candidate$priority[[1L]] + 1
  hash_fields <- setdiff(
    names(rehashed_candidate), "candidate_observation_sha256"
  )
  rehashed_candidate$candidate_observation_sha256[[1L]] <-
    stpd_threshold_first_hash_domain(
      "stpd-burst-stage4-candidate-observation-v1",
      rehashed_candidate[1L, hash_fields, drop = FALSE]
    )
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_candidates_v1",
      rehashed_candidate
    ), error = identity
  ), error_class)

  bad_entry <- observations$burst_threshold_stage4_entry_v1
  bad_entry$max_expand <- bad_entry$max_expand + 1L
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_entry_v1", bad_entry
    ), error = identity
  ), error_class)

  bad_receipt <- observations$burst_threshold_stage4_receipt_v1
  bad_receipt$observed_attempt_n <- bad_receipt$observed_attempt_n + 1L
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_threshold_stage4_receipt_v1", bad_receipt
    ), error = identity
  ), error_class)

  bad_union <- observations$burst_union_pre_cap_v1
  threshold_row <- which(bad_union$source_route == "threshold_centred")[[1L]]
  bad_union$source_payload_sha256[[threshold_row]] <-
    paste(rep("0", 64L), collapse = "")
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(
      run$shard, "burst_union_pre_cap_v1", bad_union
    ), error = identity
  ), error_class)
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before
  )
})

test_that("connector keeps an independent duplicate domain and episode root", {
  run <- stage4_lineage_run(
    intervals = c(0.080, 0.006, 0.006, 0.020, 0.006, 0.006, 0.080),
    single_connector_enabled = TRUE,
    run_id = "run_stage4_connector"
  )
  expect_identical(run$observed, run$baseline)
  observations <- run$snapshot$observations
  attempts <- observations$burst_threshold_stage4_attempts_v1
  candidates <- observations$burst_threshold_stage4_candidates_v1
  roots <- observations$burst_threshold_stage4_episode_roots_v1
  connector <- attempts[
    attempts$subroute == "single_expanded_bridge", , drop = FALSE
  ]
  emitted <- connector$terminal_status == "candidate_emitted"
  expect_true(any(emitted))
  expect_true(all(
    connector$duplicate_domain[connector$geometry_claimed] ==
      "single_connector_bridge_seen"
  ))
  expect_identical(
    connector$source_root_sha256, roots$support_sha256[
      connector$source_root_ordinal
    ]
  )
  expect_true(any(
    candidates$subroute == "single_expanded_bridge" &
      candidates$detector_start_row == 3L &
      candidates$detector_end_row == 7L
  ))
  expect_true(any(
    attempts$subroute != "single_expanded_bridge" &
      attempts$geometry_key == "3_7"
  ))
})

test_that("an appended scientific reject remains a Stage-4 candidate", {
  run <- stage4_lineage_run(
    manual_negative = TRUE, run_id = "run_stage4_appended_reject"
  )
  expect_identical(run$observed, run$baseline)
  observations <- run$snapshot$observations
  candidates <- observations$burst_threshold_stage4_candidates_v1
  attempts <- observations$burst_threshold_stage4_attempts_v1
  rejected <- candidates$final_label == "reject"
  expect_true(any(rejected))
  source_attempts <- candidates$source_attempt_ordinal[rejected]
  attempt_rows <- match(source_attempts, attempts$attempt_ordinal)
  expect_true(all(
    attempts$terminal_status[attempt_rows] == "candidate_emitted"
  ))
  expect_identical(
    observations$burst_threshold_stage4_receipt_v1$emitted_candidate_n,
    as.integer(nrow(candidates))
  )
})

test_that("collector-off never enters Stage-4 observation helpers", {
  dat <- ensure_train_isi_percentiles(
    stage4_lineage_fixture(), 0.001, force = TRUE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final"
  )
  short <- ensure_train_isi_percentiles(
    stage4_lineage_fixture(0.030), 0.001, force = TRUE
  )
  short_vp <- stpd_event_grammar_params_impl(
    short, params, 0.001, train = "train_1"
  )
  short_baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    short, params, short_vp, 0.001, "train_1", pipeline = "final"
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_stage4_observer_new = function(...) {
      stop("Stage-4 observer entered while collector is off")
    },
    stpd_candidate_lineage_stage4_short_receipt = function(...) {
      stop("Stage-4 short receipt entered while collector is off")
    },
    stpd_candidate_lineage_stage4_finalize = function(...) {
      stop("Stage-4 finalize entered while collector is off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final",
    candidate_lineage_collector = NULL
  )
  short_observed <- stpd_event_grammar_detect_burst_events_dispatch(
    short, params, short_vp, 0.001, "train_1", pipeline = "final",
    candidate_lineage_collector = NULL
  )
  expect_identical(observed, baseline)
  expect_identical(
    serialize(observed, NULL, version = 3L),
    serialize(baseline, NULL, version = 3L)
  )
  expect_identical(short_observed, short_baseline)
})
