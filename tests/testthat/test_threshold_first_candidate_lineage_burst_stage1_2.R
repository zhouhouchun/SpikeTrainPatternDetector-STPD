burst_stage12_error <- function(expr) tryCatch(expr, error = identity)

burst_stage12_collector <- function(run_id = "run_burst_stage12") {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("c", 64L), collapse = ""),
    "golden_middle_burst", "train_1"
  )
  collector
}

burst_stage12_enter <- function(shard, n_interval_rows, min_isi_sec = 0.001) {
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(
      train = "train_1", n_interval_rows = as.integer(n_interval_rows),
      min_isi_sec = as.numeric(min_isi_sec),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
}

burst_stage12_expected <- function(dat, params, min_isi_sec, train) {
  prepared <- ensure_train_isi_percentiles(
    dat, min_isi_sec, force = TRUE
  )
  effective <- effective_params_for_detector(params)
  vp <- stpd_event_grammar_params_impl(
    prepared, effective, min_isi_sec, train = train
  )
  isi <- suppressWarnings(as.numeric(prepared$ISI_sec))
  artifact <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !artifact
  if (length(valid)) valid[1L] <- FALSE
  seed <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  native_bridge <- valid & is.finite(isi) & isi <= vp$bridge_high
  support_rows <- seq.int(2L, nrow(prepared))
  raw <- data.frame(
    train = rep(train, length(support_rows)),
    detector_row_index = as.integer(support_rows),
    isi_index = seq_along(support_rows),
    isi_sec = as.numeric(isi[support_rows]),
    artifact_isi = as.logical(artifact[support_rows]),
    valid_isi = as.logical(valid[support_rows]),
    seed_band_member = as.logical(seed[support_rows]),
    native_bridge_extension_eligible = as.logical(
      native_bridge[support_rows]
    ),
    min_valid_isi_sec = rep(as.numeric(min_isi_sec), length(support_rows)),
    seed_low_sec = rep(as.numeric(vp$seed_low), length(support_rows)),
    seed_high_sec = rep(as.numeric(vp$seed_high), length(support_rows)),
    native_bridge_high_sec = rep(
      as.numeric(vp$bridge_high), length(support_rows)
    ),
    min_seed_isi_count = rep(
      as.integer(vp$min_seed_isi_n), length(support_rows)
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  runs <- stpd_event_grammar_bool_runs(seed)
  seed_runs <- stpd_candidate_lineage_empty_hook_payload(
    "burst_seed_runs_v1"
  )
  if (nrow(runs)) {
    counts <- as.integer(runs$end_isi - runs$start_isi + 1L)
    seed_runs <- data.frame(
      train = rep(train, nrow(runs)), run_ordinal = seq_len(nrow(runs)),
      detector_start_row = as.integer(runs$start_isi),
      detector_end_row = as.integer(runs$end_isi),
      start_isi = as.integer(runs$start_isi - 1L),
      end_isi = as.integer(runs$end_isi - 1L),
      seed_isi_count = counts,
      min_seed_isi_count = rep(
        as.integer(vp$min_seed_isi_n), nrow(runs)
      ),
      min_seed_pass = counts >= as.integer(vp$min_seed_isi_n),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  list(raw = raw, seed_runs = seed_runs, prepared = prepared, vp = vp)
}

test_that("Burst stages 1 and 2 are captured directly before filtering", {
  ds <- stpd_golden_test_dataset("middle_burst")
  dat <- ds$trains$train_1
  params <- default_params()
  min_isi_sec <- 0.001
  expected <- burst_stage12_expected(
    dat, params, min_isi_sec, "train_1"
  )
  baseline <- run_detector_one_train(
    dat, params, min_isi_sec = min_isi_sec, train = "train_1",
    lock_manual = FALSE
  )

  collector <- burst_stage12_collector()
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  observed <- run_detector_one_train(
    dat, params, min_isi_sec = min_isi_sec, train = "train_1",
    lock_manual = FALSE, candidate_lineage_collector = shard
  )
  expect_identical(observed, baseline)
  expect_identical(
    serialize(observed, NULL, version = 3L),
    serialize(baseline, NULL, version = 3L)
  )
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(
    snapshot$observation_index$hook_id,
    c(
      "hf_protected_impl_entry_v1",
      "pause_raw_entry_v1",
      "pause_raw_support_v1",
      "pause_raw_runs_v1",
      "pause_raw_output_v1",
      "pause_boundary_projection_sources_v1",
      "pause_boundary_projection_output_v1",
      "pause_raw_receipt_v1",
      "burst_dispatch_final_route_v1",
      "burst_structure_first_pre_cap_v1",
      "burst_structure_first_post_cap_v1",
      "burst_structure_first_scan_receipt_v1",
      "burst_detector_final_entry_v1",
      "burst_raw_threshold_support_v1",
      "burst_seed_runs_v1",
      "burst_threshold_stage4_entry_v1",
      "burst_threshold_stage4_episode_roots_v1",
      "burst_threshold_stage4_attempts_v1",
      "burst_threshold_stage4_candidates_v1",
      "burst_threshold_stage4_receipt_v1",
      "burst_union_pre_cap_v1",
      "burst_union_post_cap_v1",
      "burst_union_rank_receipt_v1",
      "burst_refractory_entry_v1",
      "burst_refractory_attempts_v1",
      "burst_refractory_output_v1",
      "burst_refractory_receipt_v1",
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1",
      "burst_pause_ownership_selection_v1",
      "burst_pause_ownership_attempts_v1",
      "burst_pause_ownership_output_v1",
      "burst_pause_ownership_receipt_v1",
      "contextual_pause_entry_v1",
      "contextual_pause_generic_input_v1",
      "contextual_pause_ownership_support_v1",
      "contextual_pause_generic_downgrade_attempts_v1",
      "contextual_pause_generic_post_downgrade_v1",
      "contextual_pause_interburst_pair_attempts_v1",
      "contextual_pause_interburst_candidates_v1",
      "contextual_pause_combined_output_v1",
      "contextual_pause_receipt_v1",
      "burst_pause_boundary_entry_v1",
      "burst_pause_boundary_sources_v1",
      "burst_pause_boundary_attempts_v1",
      "burst_pause_boundary_output_v1",
      "burst_pause_boundary_receipt_v1",
      "burst_hard_threshold_entry_v1",
      "burst_hard_threshold_support_v1",
      "burst_hard_threshold_runs_v1",
      "burst_hard_threshold_pre_refractory_candidates_v1",
      "burst_hard_threshold_refractory_attempts_v1",
      "burst_hard_threshold_pre_boundary_output_v1",
      "burst_hard_threshold_boundary_sources_v1",
      "burst_hard_threshold_boundary_attempts_v1",
      "burst_hard_threshold_post_boundary_output_v1",
      "burst_hard_threshold_receipt_v1",
      "gap_final_entry_v1",
      "gap_final_candidate_input_v1",
      "gap_final_arbitration_v1",
      "gap_final_materialized_output_v1",
      "gap_final_receipt_v1",
      "hfs_state_entry_v1",
      "hfs_state_candidates_v1",
      "hfs_state_support_roles_v1",
      "hfs_state_parent_selection_v1",
      "hfs_state_receipt_v1",
      "tonic_state_entry_v1",
      "tonic_state_candidates_v1",
      "tonic_state_frequency_regularity_v1",
      "tonic_state_fragment_redetection_v1",
      "tonic_state_receipt_v1",
      "nested_hfs_review_entry_v1",
      "nested_hfs_review_candidates_v1",
      "nested_hfs_review_local_contrast_v1",
      "nested_hfs_review_parent_invariance_v1",
      "nested_hfs_review_receipt_v1",
      "candidate_universe_release_manifest_v1",
      "candidate_universe_release_products_v1",
      "candidate_universe_release_receipt_v1"
    )
  )
  expect_identical(
    snapshot$observations$burst_raw_threshold_support_v1,
    expected$raw
  )
  expect_identical(
    snapshot$observations$burst_seed_runs_v1,
    expected$seed_runs
  )
  expect_identical(
    snapshot$observation_index$record_n,
    as.integer(vapply(snapshot$observations, nrow, integer(1)))
  )
  expect_identical(
    expected$raw$detector_row_index, expected$raw$isi_index + 1L
  )
  if (nrow(expected$seed_runs)) {
    expect_identical(
      expected$seed_runs$detector_start_row,
      expected$seed_runs$start_isi + 1L
    )
    expect_identical(
      expected$seed_runs$detector_end_row,
      expected$seed_runs$end_isi + 1L
    )
  }
  stpd_candidate_lineage_collector_end_train(collector, shard)
})

test_that("undersized raw seed runs are observed before candidate filtering", {
  intervals <- c(0.030, 0.030, 0.030, 0.005, 0.030, 0.030, 0.030)
  spike_time <- c(0, cumsum(intervals))
  dat <- data.frame(
    idx = seq_along(spike_time), timestamp_sec = spike_time,
    ISI_sec = c(NA_real_, intervals), stringsAsFactors = FALSE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  vp$min_seed_isi_n <- 2L
  collector <- burst_stage12_collector(
    run_id = "run_burst_stage12_undersized"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  burst_stage12_enter(shard, nrow(dat), 0.001)
  observed <- stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, 0.001, "train_1",
    candidate_lineage_collector = shard
  )
  expect_identical(observed, data.frame())
  runs <- stpd_candidate_lineage_collector_observation_snapshot(shard)$
    observations$burst_seed_runs_v1
  expect_identical(nrow(runs), 1L)
  expect_identical(runs$seed_isi_count, 1L)
  expect_identical(runs$min_seed_isi_count, 2L)
  expect_false(runs$min_seed_pass)
})

test_that("empty raw-run stages are present as typed zero payloads", {
  spike_time <- cumsum(c(0, rep(0.030, 12L)))
  dat <- data.frame(
    idx = seq_along(spike_time), timestamp_sec = spike_time,
    ISI_sec = c(NA_real_, diff(spike_time)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  params <- effective_params_for_detector(default_params())
  prepared <- ensure_train_isi_percentiles(dat, 0.001, force = TRUE)
  vp <- stpd_event_grammar_params_impl(
    prepared, params, 0.001, train = "train_1"
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  baseline <- stpd_event_grammar_detect_burst_events_final_impl(
    prepared, params, vp, 0.001, "train_1"
  )

  collector <- burst_stage12_collector(run_id = "run_burst_stage12_empty")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  burst_stage12_enter(shard, nrow(prepared), 0.001)
  observed <- stpd_event_grammar_detect_burst_events_final_impl(
    prepared, params, vp, 0.001, "train_1",
    candidate_lineage_collector = shard
  )
  expect_identical(observed, baseline)
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  raw <- snapshot$observations$burst_raw_threshold_support_v1
  runs <- snapshot$observations$burst_seed_runs_v1
  expect_identical(nrow(raw), nrow(prepared) - 1L)
  expect_false(any(raw$seed_band_member))
  expect_identical(
    runs, stpd_candidate_lineage_empty_hook_payload("burst_seed_runs_v1")
  )
  expect_identical(
    vapply(runs, typeof, character(1)),
    stpd_candidate_lineage_observation_hook_schema("burst_seed_runs_v1")
  )
})

test_that("short trains emit both typed empty stage observations", {
  dat <- data.frame(
    idx = 1:2, timestamp_sec = c(0, 0.03),
    ISI_sec = c(NA_real_, 0.03), stringsAsFactors = FALSE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  collector <- burst_stage12_collector(run_id = "run_burst_stage12_short")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  burst_stage12_enter(shard, nrow(dat), 0.001)
  observed <- stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, 0.001, "train_1",
    candidate_lineage_collector = shard
  )
  expect_identical(observed, data.frame())
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(
    snapshot$observations$burst_raw_threshold_support_v1,
    stpd_candidate_lineage_empty_hook_payload(
      "burst_raw_threshold_support_v1"
    )
  )
  expect_identical(
    snapshot$observations$burst_seed_runs_v1,
    stpd_candidate_lineage_empty_hook_payload("burst_seed_runs_v1")
  )
})

test_that("Burst stage hook schemas reject semantic tampering", {
  collector <- burst_stage12_collector(run_id = "run_burst_stage12_tamper")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  burst_stage12_enter(shard, 2L, 0.001)
  expect_s3_class(
    burst_stage12_error(stpd_candidate_lineage_collector_capture(
      shard, "burst_detector_final_entry_v1",
      data.frame(
        train = "train_1", burst_pipeline_id = "consistency_optimized",
        stringsAsFactors = FALSE, check.names = FALSE
      )
    )),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "burst_detector_final_entry_v1",
    data.frame(
      train = "train_1", burst_pipeline_id = "final",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
  raw <- data.frame(
    train = "train_1", detector_row_index = 2L,
    isi_index = 1L, isi_sec = 0.005,
    artifact_isi = FALSE, valid_isi = TRUE,
    seed_band_member = FALSE,
    native_bridge_extension_eligible = TRUE,
    min_valid_isi_sec = 0.001, seed_low_sec = 0.001,
    seed_high_sec = 0.010, native_bridge_high_sec = 0.015,
    min_seed_isi_count = 2L,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_s3_class(
    burst_stage12_error(stpd_candidate_lineage_collector_capture(
      shard, "burst_raw_threshold_support_v1", raw
    )),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  malformed_runs <- data.frame(
    train = "train_1", run_ordinal = 1L,
    detector_start_row = 3L, detector_end_row = 5L,
    start_isi = 2L, end_isi = 4L,
    seed_isi_count = 2L, min_seed_isi_count = 2L,
    min_seed_pass = TRUE, stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_s3_class(
    burst_stage12_error(stpd_candidate_lineage_collector_capture(
      shard, "burst_seed_runs_v1", malformed_runs
    )),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(shard)$
      observation_index$hook_id,
    c("hf_protected_impl_entry_v1", "burst_detector_final_entry_v1")
  )
})

test_that("native bridge eligibility is not narrowed to the seed lower bound", {
  intervals <- c(0.080, 0.003, 0.006, 0.007, 0.080)
  spike_time <- c(0, cumsum(intervals))
  dat <- data.frame(
    idx = seq_along(spike_time), timestamp_sec = spike_time,
    ISI_sec = c(NA_real_, intervals), stringsAsFactors = FALSE
  )
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$seed_low <- 0.005
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  collector <- burst_stage12_collector(run_id = "run_burst_bridge_semantics")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  burst_stage12_enter(shard, nrow(dat), 0.001)
  stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, 0.001, "train_1",
    candidate_lineage_collector = shard
  )
  raw <- stpd_candidate_lineage_collector_observation_snapshot(shard)$
    observations$burst_raw_threshold_support_v1
  below_seed <- raw$isi_sec == 0.003
  expect_identical(sum(below_seed), 1L)
  expect_false(raw$seed_band_member[below_seed])
  expect_true(raw$native_bridge_extension_eligible[below_seed])
})

test_that("stages 1 and 2 remain exhaustive before a live candidate cap", {
  intervals <- c(
    rep(0.080, 3L), rep(0.005, 4L),
    rep(0.080, 3L), rep(0.005, 4L),
    rep(0.080, 3L), rep(0.005, 4L), rep(0.080, 3L)
  )
  spike_time <- c(0, cumsum(intervals))
  dat <- data.frame(
    idx = seq_along(spike_time), timestamp_sec = spike_time,
    ISI_sec = c(NA_real_, intervals), stringsAsFactors = FALSE
  )
  dat <- ensure_train_isi_percentiles(dat, 0.001, force = TRUE)
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  vp$min_seed_isi_n <- 2L
  vp$min_spikes <- 4L
  vp$max_expand <- 8L
  vp$max_candidates <- 1L
  baseline <- stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, 0.001, "train_1"
  )

  collector <- burst_stage12_collector(run_id = "run_burst_stage12_cap")
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  burst_stage12_enter(shard, nrow(dat), 0.001)
  observed <- stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, 0.001, "train_1",
    candidate_lineage_collector = shard
  )
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(observed, baseline)
  expect_identical(nrow(observed), 1L)
  expect_identical(
    nrow(snapshot$observations$burst_raw_threshold_support_v1),
    nrow(dat) - 1L
  )
  expect_identical(
    snapshot$observations$burst_seed_runs_v1$run_ordinal, 1:3
  )
  expect_true(all(
    snapshot$observations$burst_seed_runs_v1$min_seed_pass
  ))
})

test_that("collector-off Burst execution cannot enter observation helpers", {
  dat <- stpd_golden_test_dataset("middle_burst")$trains$train_1
  dat <- ensure_train_isi_percentiles(dat, 0.001, force = TRUE)
  params <- effective_params_for_detector(default_params())
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  baseline <- stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, 0.001, "train_1"
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_collector_capture = function(...) {
      stop("collector capture entered while audit is off")
    },
    stpd_candidate_lineage_empty_hook_payload = function(...) {
      stop("typed payload builder entered while audit is off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_event_grammar_detect_burst_events_final_impl(
    dat, params, vp, 0.001, "train_1",
    candidate_lineage_collector = NULL
  )
  expect_identical(observed, baseline)
})

test_that("alternate inner Burst pipelines expose only the parallel hard root", {
  dat <- stpd_golden_test_dataset("middle_burst")$trains$train_1
  params <- default_params()
  params$event_grammar$burst_detector_pipeline <- "consistency_optimized"
  baseline <- run_detector_one_train(
    dat, params, train = "train_1", lock_manual = FALSE
  )
  collector <- burst_stage12_collector(
    run_id = "run_burst_stage12_alternate"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  observed <- run_detector_one_train(
    dat, params, train = "train_1", lock_manual = FALSE,
    candidate_lineage_collector = shard
  )
  expect_identical(observed, baseline)
  snapshot <- stpd_candidate_lineage_collector_observation_snapshot(shard)
  expect_identical(
    snapshot$observation_index$hook_id,
    c(
      "hf_protected_impl_entry_v1",
      "pause_raw_entry_v1",
      "pause_raw_support_v1",
      "pause_raw_runs_v1",
      "pause_raw_output_v1",
      "pause_boundary_projection_sources_v1",
      "pause_boundary_projection_output_v1",
      "pause_raw_receipt_v1",
      "contextual_pause_entry_v1",
      "contextual_pause_generic_input_v1",
      "contextual_pause_ownership_support_v1",
      "contextual_pause_generic_downgrade_attempts_v1",
      "contextual_pause_generic_post_downgrade_v1",
      "contextual_pause_interburst_pair_attempts_v1",
      "contextual_pause_interburst_candidates_v1",
      "contextual_pause_combined_output_v1",
      "contextual_pause_receipt_v1",
      "burst_hard_threshold_entry_v1",
      "burst_hard_threshold_support_v1",
      "burst_hard_threshold_runs_v1",
      "burst_hard_threshold_pre_refractory_candidates_v1",
      "burst_hard_threshold_refractory_attempts_v1",
      "burst_hard_threshold_pre_boundary_output_v1",
      "burst_hard_threshold_boundary_sources_v1",
      "burst_hard_threshold_boundary_attempts_v1",
      "burst_hard_threshold_post_boundary_output_v1",
      "burst_hard_threshold_receipt_v1",
      "gap_final_entry_v1",
      "gap_final_candidate_input_v1",
      "gap_final_arbitration_v1",
      "gap_final_materialized_output_v1",
      "gap_final_receipt_v1",
      "hfs_state_entry_v1",
      "hfs_state_candidates_v1",
      "hfs_state_support_roles_v1",
      "hfs_state_parent_selection_v1",
      "hfs_state_receipt_v1",
      "tonic_state_entry_v1",
      "tonic_state_candidates_v1",
      "tonic_state_frequency_regularity_v1",
      "tonic_state_fragment_redetection_v1",
      "tonic_state_receipt_v1",
      "nested_hfs_review_entry_v1",
      "nested_hfs_review_candidates_v1",
      "nested_hfs_review_local_contrast_v1",
      "nested_hfs_review_parent_invariance_v1",
      "nested_hfs_review_receipt_v1"
    )
  )
  expect_false(any(c(
    "burst_seed_support_v1", "burst_seed_runs_v1",
    "burst_stage1_candidates_v1", "burst_stage2_attempts_v1"
  ) %in% snapshot$observation_index$hook_id))
})
