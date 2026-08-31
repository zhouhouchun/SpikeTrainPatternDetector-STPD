refractory_lineage_fixture <- function(intervals) {
  timestamp <- c(0, cumsum(intervals))
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, intervals), stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

refractory_lineage_params <- function(action) {
  params <- effective_params_for_detector(default_params())
  for (path in c("detector", "burst")) {
    params[[path]]$refractory_suspect_sec <- 0.0015
    params[[path]]$refractory_suspect_action <- action
  }
  params$spiketrainpattern$qc$refractory_suspect_isi_sec <- 0.0015
  params$spiketrainpattern$qc$refractory_suspect_action <- action
  effective_params_for_detector(params)
}

refractory_lineage_collector <- function(run_id) {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("f", 64L), collapse = ""),
    "burst_refractory", "train_1"
  )
  collector
}

refractory_lineage_run <- function(
    action = "warn_only",
    intervals = c(0.080, rep(0.006, 3L), 0.0011,
                  rep(0.006, 3L), 0.080),
    min_spikes = 4L, run_id = paste0("run_refractory_", action)) {
  dat <- ensure_train_isi_percentiles(
    refractory_lineage_fixture(intervals), 0.001, force = TRUE
  )
  params <- refractory_lineage_params(action)
  vp <- stpd_event_grammar_params_impl(
    dat, params, 0.001, train = "train_1"
  )
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  vp$cross_train_borrowing_bridge_candidate_sec <- 0.015
  vp$min_seed_isi_n <- 2L
  vp$min_spikes <- as.integer(min_spikes)
  vp$max_expand <- 12L
  vp$max_candidates <- 100L

  baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final"
  )
  collector <- refractory_lineage_collector(run_id)
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
      min_isi_sec = 0.001, stringsAsFactors = FALSE,
      check.names = FALSE
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

refractory_lineage_validate_error <- function(shard, hook, payload) {
  expect_s3_class(
    tryCatch(
      stpd_candidate_lineage_validate_hook_payload(shard, hook, payload),
      error = identity
    ),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
}

test_that("refractory observation covers every normalized action without interference", {
  aliases <- c(
    warn_only = "warn_only",
    demote_to_possible = "demote_to_possible",
    exclude_candidate = "exclude_candidate",
    split_at_suspect = "split_at_suspect",
    exclude_suspect_isi_and_reevaluate =
      "exclude_suspect_isi_and_reevaluate",
    mark_multiunit_contamination = "mark_multiunit_contamination"
  )
  expected_non_split <- list(
    warn_only = list(
      final_label = "burst", action = "accept",
      gate_status = c(
        "structure_first_two_sided_pass",
        "event_grammar_two_sided_burst_event_pass"
      ),
      priority = c(1260, 1250), relation = "metadata_updated",
      retained_output_n = 2L, retyped_output_n = 0L
    ),
    demote_to_possible = list(
      final_label = "possible_burst", action = "demote_to_possible",
      gate_status = "refractory_suspect_demoted_to_possible_burst",
      priority = c(1260, 1250), relation = "retyped",
      retained_output_n = 0L, retyped_output_n = 2L
    ),
    exclude_candidate = list(
      final_label = "reject", action = "reject",
      gate_status = "refractory_suspect_candidate_rejected",
      priority = c(0, 0), relation = "retyped",
      retained_output_n = 0L, retyped_output_n = 2L
    ),
    mark_multiunit_contamination = list(
      final_label = "possible_burst", action = "demote_to_possible",
      gate_status =
        "refractory_suspect_marked_possible_multiunit_contamination",
      priority = c(1260, 1250), relation = "retyped",
      retained_output_n = 0L, retyped_output_n = 2L
    )
  )
  set.seed(82021)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()

  for (action in names(aliases)) {
    run <- refractory_lineage_run(
      action = action, run_id = paste0("run_refractory_grid_", action)
    )
    expect_identical(run$observed, run$baseline, info = action)
    expect_identical(
      serialize(run$observed, NULL, version = 3L),
      serialize(run$baseline, NULL, version = 3L), info = action
    )
    obs <- run$snapshot$observations
    entry <- obs$burst_refractory_entry_v1
    attempts <- obs$burst_refractory_attempts_v1
    output <- obs$burst_refractory_output_v1
    receipt <- obs$burst_refractory_receipt_v1
    union_post <- obs$burst_union_post_cap_v1

    expect_identical(entry$normalized_policy_action, aliases[[action]])
    expect_identical(receipt$normalized_policy_action, aliases[[action]])
    expect_identical(entry$input_candidate_n, as.integer(nrow(union_post)))
    expect_identical(receipt$input_candidate_n, as.integer(nrow(union_post)))
    expect_identical(receipt$output_candidate_n, as.integer(nrow(output)))
    expect_identical(receipt$candidate_attempt_n,
                     as.integer(sum(attempts$attempt_kind == "candidate")))
    expect_identical(receipt$fragment_attempt_n,
                     as.integer(sum(attempts$attempt_kind == "fragment")))
    expect_identical(attempts$attempt_ordinal, seq_len(nrow(attempts)))
    expect_identical(output$output_ordinal, seq_len(nrow(output)))
    expect_true(receipt$replay_exhausted)
    expect_identical(receipt$coverage_status,
                     paste0(
                       "refractory_replay_complete_over_",
                       "observed_post_union_inputs"
                     ))
    expect_identical(
      receipt$evidence_origin,
      "deterministic_replay_from_live_post_union_input_output"
    )
    expect_identical(receipt$input_scope,
                     "observed_post_union_candidates")
    expect_identical(receipt$upstream_universe_status,
                     "candidate_universe_unavailable")
    expect_true(all(output$source_input_ordinal %in%
                      seq_len(nrow(union_post))))
    expect_identical(
      output$source_candidate_id,
      union_post$source_candidate_id[output$source_input_ordinal]
    )
    expect_identical(
      output$source_payload_sha256,
      union_post$source_payload_sha256[output$source_input_ordinal]
    )
    if (action %in% names(expected_non_split)) {
      expected <- expected_non_split[[action]]
      expect_true(all(output$final_label == expected$final_label),
                  info = action)
      expect_true(all(output$action == expected$action), info = action)
      expect_identical(output$gate_status, rep_len(
        expected$gate_status, nrow(output)
      ), info = action)
      expect_identical(output$priority, as.numeric(expected$priority),
                       info = action)
      expect_true(all(output$relation == expected$relation), info = action)
      expect_identical(receipt$retained_output_n,
                       expected$retained_output_n, info = action)
      expect_identical(receipt$retyped_output_n,
                       expected$retyped_output_n, info = action)
      expect_identical(receipt$rejected_parent_n, 0L, info = action)
      expect_identical(receipt$fragment_output_n, 0L, info = action)
    }
  }
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)
})

test_that("split policies close parents, surviving fragments and discarded fragments", {
  for (action in c(
      "split_at_suspect", "exclude_suspect_isi_and_reevaluate")) {
    surviving <- refractory_lineage_run(
      action = action,
      intervals = c(0.080, rep(0.006, 3L), 0.0011,
                    rep(0.006, 3L), 0.080),
      run_id = paste0("run_refractory_fragments_", action)
    )
    obs <- surviving$snapshot$observations
    attempts <- obs$burst_refractory_attempts_v1
    output <- obs$burst_refractory_output_v1
    receipt <- obs$burst_refractory_receipt_v1
    fragments <- attempts$attempt_kind == "fragment"
    fragment_output <- output$relation == "fragment_of_rejected_parent"
    expect_true(any(fragments))
    expect_true(any(fragment_output))
    expect_true(any(output$relation == "rejected_parent"))
    expect_true(all(attempts$source_input_ordinal[fragments] %in%
                      output$source_input_ordinal[fragment_output]))
    expect_identical(receipt$fragment_output_n,
                     as.integer(sum(fragment_output)))
    expect_identical(receipt$rejected_parent_n,
                     as.integer(sum(output$relation == "rejected_parent")))
    expect_identical(
      attempts$output_candidate_id[attempts$terminal_status ==
                                     "fragment_emitted"],
      output$output_candidate_id[match(
        attempts$output_ordinal[attempts$terminal_status ==
                                  "fragment_emitted"],
        output$output_ordinal
      )]
    )

    discarded <- refractory_lineage_run(
      action = action,
      intervals = c(0.080, 0.006, 0.0011, 0.006, 0.080),
      min_spikes = 4L,
      run_id = paste0("run_refractory_discarded_", action)
    )
    d_obs <- discarded$snapshot$observations
    d_attempts <- d_obs$burst_refractory_attempts_v1
    d_receipt <- d_obs$burst_refractory_receipt_v1
    expect_true(any(
      d_attempts$attempt_kind == "fragment" &
        d_attempts$terminal_status == "fragment_discarded"
    ))
    expect_true(all(
      d_attempts$terminal_reason[
        d_attempts$terminal_status == "fragment_discarded"
      ] %in% c("undersized_fragment", "fragment_metrics_unavailable",
               "invalid_parent_geometry", "no_non_suspect_support")
    ))
    expect_identical(
      d_receipt$fragment_discarded_n,
      as.integer(sum(d_attempts$terminal_status == "fragment_discarded"))
    )
    expect_identical(d_receipt$fragment_output_n, 0L)
  }
})

test_that("no-suspect and empty refractory observations are typed and closed", {
  no_suspect <- refractory_lineage_run(
    intervals = c(0.080, rep(0.006, 6L), 0.080),
    run_id = "run_refractory_no_suspect"
  )
  ns <- no_suspect$snapshot$observations
  expect_identical(ns$burst_refractory_receipt_v1$suspect_candidate_n, 0L)
  expect_identical(ns$burst_refractory_receipt_v1$fragment_attempt_n, 0L)
  expect_identical(ns$burst_refractory_receipt_v1$coverage_status,
                   paste0(
                     "refractory_replay_complete_over_",
                     "observed_post_union_inputs"
                   ))
  expect_true(all(ns$burst_refractory_output_v1$relation %in%
                    c("retained", "metadata_updated")))

  empty <- refractory_lineage_run(
    intervals = rep(0.030, 8L), run_id = "run_refractory_empty"
  )
  eo <- empty$snapshot$observations
  for (hook in c("burst_refractory_attempts_v1",
                 "burst_refractory_output_v1")) {
    expect_identical(eo[[hook]],
                     stpd_candidate_lineage_empty_hook_payload(hook))
  }
  expect_identical(eo$burst_refractory_entry_v1$input_candidate_n, 0L)
  expect_identical(eo$burst_refractory_entry_v1$applicability_status,
                   "not_applicable_empty_input")
  expect_identical(eo$burst_refractory_receipt_v1$coverage_status,
                   paste0(
                     "refractory_not_applicable_empty_",
                     "observed_post_union_inputs"
                   ))
})

test_that("already-possible suspect candidates are metadata-updated, not retyped", {
  run <- refractory_lineage_run(
    action = "demote_to_possible",
    run_id = "run_refractory_already_possible"
  )
  context <- unserialize(serialize(
    run$shard$burst_refractory_context, NULL, version = 3L
  ))
  context$input_candidates$final_label[[1L]] <- "possible_burst"
  if ("class" %in% names(context$input_candidates)) {
    context$input_candidates$class[[1L]] <- "possible_burst"
  }
  union_post <- run$snapshot$observations$burst_union_post_cap_v1
  context$source_hashes[[1L]] <-
    stpd_candidate_lineage_refractory_candidate_hash(
      context$input_candidates[1L, , drop = FALSE],
      union_post$source_route[[1L]]
    )
  context$scientific_out <-
    stpd_event_core_apply_refractory_suspect_policy(
      context$input_candidates, context$params, dat = context$dat,
      vp = context$vp, min_isi_sec = context$min_isi_sec
    )
  replay <- stpd_candidate_lineage_refractory_replay(context)
  expect_false(is.null(replay))
  expect_identical(replay$output$relation[[1L]], "metadata_updated")
  expect_identical(replay$output$final_label[[1L]], "possible_burst")
  expect_identical(replay$output$relation[[2L]], "retyped")
  expect_identical(replay$receipt$retained_output_n, 1L)
  expect_identical(replay$receipt$retyped_output_n, 1L)
})

test_that("refractory train and mutable context remain closed to the union", {
  run <- refractory_lineage_run(
    action = "split_at_suspect", run_id = "run_refractory_scope"
  )
  context <- run$shard$burst_refractory_context
  wrong_train <- tryCatch(
    stpd_candidate_lineage_capture_burst_refractory(
      run$shard, "wrong_train", context$input_candidates,
      context$scientific_out, context$dat, context$params, context$vp,
      context$min_isi_sec
    ),
    error = identity
  )
  expect_s3_class(
    wrong_train, "stpd_candidate_lineage_collector_train_scope_invalid"
  )

  entry <- run$snapshot$observations$burst_refractory_entry_v1
  bad_entry <- entry
  bad_entry$train <- "wrong_train"
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_entry_v1", bad_entry
  )

  original_context <- context
  forged_context <- unserialize(serialize(
    original_context, NULL, version = 3L
  ))
  forged_context$train <- "wrong_train"
  run$shard$burst_refractory_context <- forged_context
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_entry_v1", entry
  )

  forged_context <- unserialize(serialize(
    original_context, NULL, version = 3L
  ))
  forged_context$source_hashes[[1L]] <- paste(rep("0", 64L), collapse = "")
  forged_payload <- stpd_candidate_lineage_refractory_replay(
    forged_context
  )$attempts
  run$shard$burst_refractory_context <- forged_context
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_attempts_v1", forged_payload
  )

  forged_context <- unserialize(serialize(
    original_context, NULL, version = 3L
  ))
  forged_context$input_candidates$final_label[[1L]] <- "long_burst"
  if ("class" %in% names(forged_context$input_candidates)) {
    forged_context$input_candidates$class[[1L]] <- "long_burst"
  }
  union_post <- run$snapshot$observations$burst_union_post_cap_v1
  forged_context$source_hashes[[1L]] <-
    stpd_candidate_lineage_refractory_candidate_hash(
      forged_context$input_candidates[1L, , drop = FALSE],
      union_post$source_route[[1L]]
    )
  forged_context$scientific_out <-
    stpd_event_core_apply_refractory_suspect_policy(
      forged_context$input_candidates, forged_context$params,
      dat = forged_context$dat, vp = forged_context$vp,
      min_isi_sec = forged_context$min_isi_sec
    )
  forged_payload <- stpd_candidate_lineage_refractory_replay(
    forged_context
  )$output
  run$shard$burst_refractory_context <- forged_context
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_output_v1", forged_payload
  )

  run$shard$burst_refractory_context <- original_context
  original_union <- run$shard$observations$burst_union_post_cap_v1
  bad_union <- original_union
  bad_union$train[] <- "wrong_train"
  run$shard$observations$burst_union_post_cap_v1 <- bad_union
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_entry_v1", entry
  )
  run$shard$observations$burst_union_post_cap_v1 <- original_union

  original_receipt <- run$shard$observations$burst_union_rank_receipt_v1
  bad_receipt <- original_receipt
  bad_receipt$overall_universe_status <- "candidate_universe_complete"
  run$shard$observations$burst_union_rank_receipt_v1 <- bad_receipt
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_entry_v1", entry
  )
  run$shard$observations$burst_union_rank_receipt_v1 <- original_receipt
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard),
    run$snapshot
  )
})

test_that("refractory source, reason, geometry, mapping, output and receipt tampering fail", {
  run <- refractory_lineage_run(
    action = "split_at_suspect", run_id = "run_refractory_tamper"
  )
  before <- run$snapshot
  obs <- before$observations

  bad <- obs$burst_refractory_attempts_v1
  bad$source_input_ordinal[[1L]] <- bad$source_input_ordinal[[1L]] + 1L
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_attempts_v1", bad
  )
  bad <- obs$burst_refractory_attempts_v1
  bad$source_payload_sha256[[1L]] <- paste(rep("0", 64L), collapse = "")
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_attempts_v1", bad
  )
  bad <- obs$burst_refractory_attempts_v1
  bad$terminal_reason[[1L]] <- "fabricated_refractory_reason"
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_attempts_v1", bad
  )
  fragment <- which(obs$burst_refractory_attempts_v1$attempt_kind ==
                      "fragment")[[1L]]
  bad <- obs$burst_refractory_attempts_v1
  bad$proposed_detector_end_row[[fragment]] <-
    bad$proposed_detector_end_row[[fragment]] + 1L
  bad$proposed_end_isi[[fragment]] <- bad$proposed_end_isi[[fragment]] + 1L
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_attempts_v1", bad
  )
  bad <- obs$burst_refractory_attempts_v1
  emitted <- which(bad$output_ordinal > 0L)[[1L]]
  bad$output_ordinal[[emitted]] <- bad$output_ordinal[[emitted]] + 1L
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_attempts_v1", bad
  )

  bad_output <- obs$burst_refractory_output_v1
  bad_output$final_label[[1L]] <- "long_burst"
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_output_v1", bad_output
  )
  bad_output <- obs$burst_refractory_output_v1
  bad_output$final_label[[1L]] <- "long_burst"
  mutated_scientific_output <- run$observed[1L, , drop = FALSE]
  mutated_scientific_output$final_label <- "long_burst"
  bad_output$output_payload_sha256[[1L]] <-
    stpd_candidate_lineage_refractory_candidate_hash(
      mutated_scientific_output, "refractory_policy_output"
    )
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_output_v1", bad_output
  )

  bad_entry <- obs$burst_refractory_entry_v1
  bad_entry$normalized_policy_action <- "exclude_candidate"
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_entry_v1", bad_entry
  )
  bad_entry <- obs$burst_refractory_entry_v1
  bad_entry$refractory_threshold_sec <-
    bad_entry$refractory_threshold_sec + 0.0001
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_entry_v1", bad_entry
  )
  bad_receipt <- obs$burst_refractory_receipt_v1
  bad_receipt$fragment_attempt_n <- bad_receipt$fragment_attempt_n + 1L
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_receipt_v1", bad_receipt
  )
  bad_receipt <- obs$burst_refractory_receipt_v1
  bad_receipt$output_payload_sha256 <- paste(rep("a", 64L), collapse = "")
  refractory_lineage_validate_error(
    run$shard, "burst_refractory_receipt_v1", bad_receipt
  )
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before
  )
})

test_that("collector-off never enters refractory observation helpers", {
  dat <- ensure_train_isi_percentiles(
    refractory_lineage_fixture(
      c(0.080, rep(0.006, 3L), 0.0011, rep(0.006, 3L), 0.080)
    ),
    0.001, force = TRUE
  )
  params <- refractory_lineage_params("split_at_suspect")
  vp <- stpd_event_grammar_params_impl(dat, params, 0.001, "train_1")
  vp$seed_low <- 0.001
  vp$seed_high <- 0.010
  vp$bridge_high <- 0.015
  vp$min_seed_isi_n <- 2L
  vp$min_spikes <- 4L
  vp$max_expand <- 12L
  vp$max_candidates <- 100L
  baseline <- stpd_event_grammar_detect_burst_events_dispatch(
    dat, params, vp, 0.001, "train_1", pipeline = "final"
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_capture_burst_refractory = function(...) {
      stop("refractory observation helper entered while collector is off")
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
})
