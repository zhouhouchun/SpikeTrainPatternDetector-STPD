pause_boundary_candidate_fixture <- function() {
  data.frame(
    train = rep("train_1", 3L),
    candidate_id = c("blocked", "retained", "invalid"),
    start_isi = c(5L, 10L, NA_integer_),
    end_isi = c(8L, 12L, NA_integer_),
    final_label = rep("burst", 3L), action = rep("accept", 3L),
    priority = c(1200, 1100, 1000), score = c(12, 11, 10),
    refractory_suspect_n = 0,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

pause_boundary_raw_fixture <- function() {
  data.frame(
    train = "train_1", candidate_id = "pause_1",
    start_isi = 7L, end_isi = 7L,
    gap_semantics = "canonical_pause", hard_for_event = TRUE,
    action = "accept", stringsAsFactors = FALSE, check.names = FALSE
  )
}

pause_boundary_context <- function(candidates = pause_boundary_candidate_fixture(),
                                   raw = pause_boundary_raw_fixture()) {
  boundaries <- stpd_event_core_pause_hard_boundaries(raw)
  list(
    train = "train_1", input_candidates = candidates,
    scientific_out =
      stpd_event_core_apply_hard_boundaries_to_burst_candidates(
        candidates, boundaries
      ),
    raw_pause_candidates = raw, boundaries = boundaries
  )
}

pause_boundary_live_dat <- function() {
  isi <- c(NA_real_, rep(0.040, 3L), rep(0.006, 3L), 0.120,
           rep(0.006, 3L), rep(0.040, 4L))
  data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(replace(isi, 1L, 0)), ISI_sec = isi,
    pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

pause_boundary_live_collector <- function(run_id) {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("b", 64L), collapse = ""),
    "pause_boundary", "train_1"
  )
  collector
}

pause_boundary_live_run <- function(run_id = "run_pause_boundary") {
  dat <- pause_boundary_live_dat()
  params <- effective_params_for_detector(default_params())
  params$detector$patterns_to_run <- c("burst", "pause")
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  collector <- pause_boundary_live_collector(run_id)
  shard <- stpd_candidate_lineage_collector_begin_train(
    collector, "train_1"
  )
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = shard
  )
  list(
    dat = dat, params = params, baseline = baseline, observed = observed,
    collector = collector, shard = shard,
    snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard)
  )
}

pause_boundary_expect_invalid <- function(shard, hook, payload) {
  expect_s3_class(
    tryCatch(
      stpd_candidate_lineage_validate_hook_payload(shard, hook, payload),
      error = identity
    ),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
}

test_that("controlled boundary replay rejects whole candidates without redetection", {
  replay <- stpd_candidate_lineage_pause_boundary_replay(
    pause_boundary_context()
  )
  expect_false(is.null(replay))
  expect_identical(replay$attempts$attempt_ordinal, 1:3)
  expect_identical(replay$attempts$source_input_ordinal, 1:3)
  expect_identical(replay$output$output_ordinal, 1:3)
  expect_identical(replay$output$source_input_ordinal, 1:3)
  expect_false(any(replay$attempts$redetection_performed))

  blocked <- replay$attempts$source_candidate_id == "blocked"
  expect_identical(replay$attempts$terminal_status[blocked],
                   "rejected_without_redetection")
  expect_identical(replay$attempts$terminal_reason[blocked],
                   "canonical_pause_overlap")
  expect_identical(replay$output$relation[blocked],
                   "rejected_by_canonical_pause_boundary")
  expect_identical(replay$output$action[blocked], "reject")
  expect_true(replay$output$hard_boundary_conflict[blocked])
  expect_identical(replay$output$hard_boundary_conflict_kind[blocked],
                   "canonical_pause")
  expect_identical(replay$output$hard_boundary_decision[blocked],
                   "rejected_without_redetection")

  retained <- replay$attempts$source_candidate_id == "retained"
  expect_identical(replay$attempts$terminal_status[retained],
                   "retained_without_redetection")
  expect_identical(replay$output$relation[retained], "metadata_updated")
  expect_identical(replay$output$action[retained], "accept")
  expect_false(replay$output$hard_boundary_conflict[retained])

  invalid <- replay$attempts$source_candidate_id == "invalid"
  expect_false(replay$attempts$geometry_available[invalid])
  expect_identical(replay$attempts$terminal_reason[invalid],
                   "invalid_geometry_not_blocked_by_scientific_policy")
  expect_false(replay$attempts$blocked[invalid])
  expect_identical(replay$receipt$input_candidate_n, 3L)
  expect_identical(replay$receipt$output_candidate_n, 3L)
  expect_identical(replay$receipt$blocked_candidate_n, 1L)
  expect_identical(replay$receipt$retained_candidate_n, 2L)
  expect_identical(replay$receipt$invalid_geometry_n, 1L)
  expect_identical(replay$receipt$redetection_n, 0L)
  expect_true(replay$receipt$replay_exhausted)

  source <- pause_boundary_candidate_fixture()
  target <- pause_boundary_context()$scientific_out
  expect_identical(target$candidate_id, source$candidate_id)
  expect_identical(target$start_isi, source$start_isi)
  expect_identical(target$end_isi, source$end_isi)
  expect_identical(target$final_label, source$final_label)
  expect_identical(target$priority, source$priority)
  expect_identical(target$score, source$score)
})

test_that("empty and no-boundary replay are typed and complete", {
  empty_candidates <- pause_boundary_candidate_fixture()[0, , drop = FALSE]
  empty <- stpd_candidate_lineage_pause_boundary_replay(
    pause_boundary_context(empty_candidates)
  )
  expect_identical(
    empty$attempts,
    stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_boundary_attempts_v1"
    )
  )
  expect_identical(
    empty$output,
    stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_boundary_output_v1"
    )
  )
  expect_identical(
    empty$receipt$coverage_status,
    "hard_boundary_not_applicable_empty_observed_refractory_output"
  )

  raw_empty <- pause_boundary_raw_fixture()[0, , drop = FALSE]
  none <- stpd_candidate_lineage_pause_boundary_replay(
    pause_boundary_context(pause_boundary_candidate_fixture(), raw_empty)
  )
  expect_identical(nrow(none$sources), 0L)
  expect_false(any(none$attempts$blocked))
  expect_false(any(none$attempts$redetection_performed))
  expect_true(all(none$output$relation == "retained"))
  expect_identical(none$receipt$canonical_boundary_n, 0L)
  expect_identical(none$receipt$blocked_candidate_n, 0L)
  expect_identical(
    none$receipt$coverage_status,
    "hard_boundary_decisions_complete_over_observed_refractory_output"
  )
})

test_that("live boundary observation is byte, RNG and options neutral", {
  set.seed(83011)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()
  run <- pause_boundary_live_run("run_pause_boundary_neutral")
  expect_identical(run$observed, run$baseline)
  expect_identical(
    serialize(run$observed, NULL, version = 3L),
    serialize(run$baseline, NULL, version = 3L)
  )
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)
  obs <- run$snapshot$observations
  expect_false(is.null(obs$burst_pause_boundary_entry_v1))
  expect_false(is.null(obs$burst_pause_boundary_attempts_v1))
  expect_false(is.null(obs$burst_pause_boundary_output_v1))
  expect_false(is.null(obs$burst_pause_boundary_receipt_v1))
  expect_false(any(obs$burst_pause_boundary_attempts_v1$redetection_performed))
  expect_identical(obs$burst_pause_boundary_receipt_v1$redetection_n, 0L)
  expect_identical(
    obs$burst_pause_boundary_receipt_v1$upstream_universe_status,
    "candidate_universe_unavailable"
  )
  expect_identical(
    obs$burst_pause_boundary_receipt_v1$input_scope,
    "observed_post_refractory_union_descendants"
  )
  expect_identical(
    obs$burst_pause_boundary_receipt_v1$upstream_pause_status,
    "post_ownership_contextual_pause_observation_closed"
  )
})

test_that("collector-off never enters Pause-boundary observation", {
  dat <- pause_boundary_live_dat()
  params <- effective_params_for_detector(default_params())
  params$detector$patterns_to_run <- c("burst", "pause")
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_capture_burst_pause_boundary = function(...) {
      stop("Pause-boundary observer entered while collector was off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = NULL
  )
  expect_identical(observed, baseline)
  expect_identical(serialize(observed, NULL, version = 3L),
                   serialize(baseline, NULL, version = 3L))
})

test_that("wrong train and hook payload tampering fail atomically", {
  run <- pause_boundary_live_run("run_pause_boundary_tamper")
  before <- run$snapshot
  obs <- before$observations
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_capture_burst_pause_boundary(
      run$shard, "train_2",
      run$shard$burst_pause_boundary_context$input_candidates,
      run$shard$burst_pause_boundary_context$scientific_out,
      run$shard$burst_pause_boundary_context$raw_pause_candidates,
      run$shard$burst_pause_boundary_context$boundaries
    ), error = identity
  ), "stpd_candidate_lineage_collector_train_scope_invalid")

  mutations <- list(
    burst_pause_boundary_entry_v1 = function(x) {
      x$input_payload_sha256 <- paste(rep("0", 64L), collapse = ""); x
    },
    burst_pause_boundary_sources_v1 = function(x) {
      if (nrow(x)) x$source_pause_ordinals[[1L]] <- "99" else
        attr(x, "forged") <- TRUE
      x
    },
    burst_pause_boundary_attempts_v1 = function(x) {
      if (nrow(x)) x$terminal_reason[[1L]] <- "fabricated_reason"
      x
    },
    burst_pause_boundary_output_v1 = function(x) {
      if (nrow(x)) x$relation[[1L]] <- "split_and_redetected"
      x
    },
    burst_pause_boundary_receipt_v1 = function(x) {
      x$blocked_candidate_n <- x$blocked_candidate_n + 1L; x
    }
  )
  for (hook in names(mutations)) {
    pause_boundary_expect_invalid(run$shard, hook,
                                  mutations[[hook]](obs[[hook]]))
  }
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before
  )
})

test_that("mutable context and upstream cross-hook rehashing are rejected", {
  run <- pause_boundary_live_run("run_pause_boundary_cross_hook")
  obs <- run$snapshot$observations

  original_raw_receipt <-
    run$shard$observations$pause_raw_receipt_v1
  changed_raw_receipt <- original_raw_receipt
  changed_raw_receipt$raw_output_n <-
    changed_raw_receipt$raw_output_n + 1L
  run$shard$observations$pause_raw_receipt_v1 <- changed_raw_receipt
  pause_boundary_expect_invalid(
    run$shard, "burst_pause_boundary_receipt_v1",
    obs$burst_pause_boundary_receipt_v1
  )
  run$shard$observations$pause_raw_receipt_v1 <- original_raw_receipt
  expect_silent(stpd_candidate_lineage_validate_hook_payload(
    run$shard, "burst_pause_boundary_receipt_v1",
    obs$burst_pause_boundary_receipt_v1
  ))

  original_context <- run$shard$burst_pause_boundary_context
  changed <- original_context
  if (nrow(changed$raw_pause_candidates)) {
    changed$raw_pause_candidates$score[[1L]] <-
      suppressWarnings(as.numeric(changed$raw_pause_candidates$score[[1L]])) + 1
  } else {
    changed$raw_pause_candidates <- pause_boundary_raw_fixture()
    changed$boundaries <- stpd_event_core_pause_hard_boundaries(
      changed$raw_pause_candidates
    )
  }
  run$shard$burst_pause_boundary_context <- changed
  pause_boundary_expect_invalid(
    run$shard, "burst_pause_boundary_receipt_v1",
    obs$burst_pause_boundary_receipt_v1
  )
  forged_replay <- stpd_candidate_lineage_pause_boundary_replay(changed)
  expect_false(is.null(forged_replay))
  pause_boundary_expect_invalid(
    run$shard, "burst_pause_boundary_receipt_v1",
    forged_replay$receipt
  )
  run$shard$burst_pause_boundary_context <- original_context

  if (nrow(original_context$raw_pause_candidates)) {
    wrong_pause_train <- unserialize(serialize(
      original_context, NULL, version = 3L
    ))
    wrong_pause_train$raw_pause_candidates$train[] <- "train_2"
    expect_null(stpd_candidate_lineage_pause_boundary_replay(
      wrong_pause_train
    ))
  }

  original_refractory <- run$shard$burst_refractory_context
  changed_refractory <- original_refractory
  if (nrow(changed_refractory$scientific_out)) {
    changed_refractory$scientific_out$score[[1L]] <-
      changed_refractory$scientific_out$score[[1L]] + 1
  }
  run$shard$burst_refractory_context <- changed_refractory
  pause_boundary_expect_invalid(
    run$shard, "burst_pause_boundary_receipt_v1",
    obs$burst_pause_boundary_receipt_v1
  )
  run$shard$burst_refractory_context <- original_refractory

  if (nrow(original_refractory$input_candidates)) {
    changed_refractory <- unserialize(serialize(
      original_refractory, NULL, version = 3L
    ))
    changed_refractory$input_candidates$score[[1L]] <-
      changed_refractory$input_candidates$score[[1L]] + 1
    changed_refractory$scientific_out <-
      stpd_event_core_apply_refractory_suspect_policy(
        changed_refractory$input_candidates, changed_refractory$params,
        dat = changed_refractory$dat, vp = changed_refractory$vp,
        min_isi_sec = changed_refractory$min_isi_sec
      )
    union_post <- obs$burst_union_post_cap_v1
    changed_refractory$source_hashes <- vapply(
      seq_len(nrow(changed_refractory$input_candidates)), function(i) {
        stpd_candidate_lineage_refractory_candidate_hash(
          changed_refractory$input_candidates[i, , drop = FALSE],
          union_post$source_route[[i]]
        )
      }, character(1)
    )
    changed_boundary <- unserialize(serialize(
      original_context, NULL, version = 3L
    ))
    changed_boundary$input_candidates <-
      changed_refractory$scientific_out
    changed_boundary$scientific_out <-
      stpd_event_core_apply_hard_boundaries_to_burst_candidates(
        changed_boundary$input_candidates, changed_boundary$boundaries
      )
    forged_replay <- stpd_candidate_lineage_pause_boundary_replay(
      changed_boundary
    )
    expect_false(is.null(forged_replay))
    run$shard$burst_refractory_context <- changed_refractory
    run$shard$burst_pause_boundary_context <- changed_boundary
    pause_boundary_expect_invalid(
      run$shard, "burst_pause_boundary_receipt_v1",
      forged_replay$receipt
    )
    run$shard$burst_refractory_context <- original_refractory
    run$shard$burst_pause_boundary_context <- original_context
  }

  original_union <- run$shard$observations$burst_union_post_cap_v1
  changed_union <- original_union
  if (nrow(changed_union)) {
    changed_union$source_payload_sha256[[1L]] <-
      paste(rep("9", 64L), collapse = "")
  }
  run$shard$observations$burst_union_post_cap_v1 <- changed_union
  pause_boundary_expect_invalid(
    run$shard, "burst_pause_boundary_receipt_v1",
    obs$burst_pause_boundary_receipt_v1
  )
  run$shard$observations$burst_union_post_cap_v1 <- original_union
})
