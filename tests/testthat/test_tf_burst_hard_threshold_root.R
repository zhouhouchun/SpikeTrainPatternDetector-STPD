hard_root_dat <- function(isi) {
  data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(replace(isi, 1L, 0)),
    ISI_sec = isi,
    pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

hard_root_params <- function(dat, burst_max = 0.010,
                             mode = "hard_threshold", hard = TRUE) {
  p <- effective_params_for_detector(default_params())
  p$detector$patterns_to_run <- c("burst", "long_burst", "pause")
  p <- merge_train_isi_thresholds_into_params(p, list(train_1 = list(
    burst_max_sec = burst_max, threshold_mode = mode,
    hard_threshold = hard, source = "test_hard_threshold"
  )))
  stpd_attach_thresholds_to_params(
    p, list(trains = list(train_1 = dat)), min_isi_sec = 0.001
  )
}

hard_root_empty_pause <- function() {
  data.frame(
    train = character(), candidate_id = character(),
    start_isi = integer(), end_isi = integer(),
    gap_semantics = character(), hard_for_event = logical(),
    action = character(), stringsAsFactors = FALSE, check.names = FALSE
  )
}

hard_root_context <- function(isi, boundaries = NULL,
                              raw_pause = hard_root_empty_pause(),
                              patterns = c("burst"),
                              classic_max = 5L, long_min = 6L,
                              long_max = 8L) {
  dat <- hard_root_dat(isi)
  params <- hard_root_params(dat)
  vp <- stpd_event_grammar_params_impl(dat, params, 0.001, "train_1")
  vp$bridge_high <- 0.0125
  vp$min_seed_isi_n <- 2L
  vp$min_spikes <- 4L
  vp$classic_max_spikes <- classic_max
  vp$long_min_spikes <- long_min
  vp$long_max_spikes <- long_max
  pre <- stpd_event_core_detect_hard_isi_thresholds(
    dat, params, vp, 0.001, "train_1"
  )
  if (is.null(boundaries)) {
    boundaries <- stpd_event_core_pause_hard_boundaries(raw_pause)
  }
  post <- stpd_event_core_apply_hard_boundaries_to_burst_candidates(
    pre, boundaries
  )
  list(
    train = "train_1", dat = dat, params = params, vp = vp,
    min_isi_sec = 0.001, patterns = patterns,
    scientific_pre_boundary = pre, scientific_post_boundary = post,
    raw_pause_candidates = raw_pause, boundaries = boundaries
  )
}

hard_root_expect_invalid <- function(shard, hook, payload) {
  expect_s3_class(
    tryCatch(
      stpd_candidate_lineage_validate_hook_payload(shard, hook, payload),
      error = identity
    ),
    "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
  )
}

test_that("hard root replays inclusive seed and bridge boundaries without cap", {
  # Runs: [2,4] passes with one bridge-only ISI; [6,7] fails the spike gate;
  # [9,13], [15,20], and [22,30] exercise all three size labels.
  isi <- c(
    NA, 0.010, 0.0125, 0.009, 0.0126,
    0.009, 0.009, 0.050,
    rep(0.009, 5), 0.050,
    rep(0.009, 6), 0.050,
    rep(0.009, 9), 0.050
  )
  context <- hard_root_context(isi)
  replay <- stpd_candidate_lineage_hard_threshold_replay(context)
  expect_false(is.null(replay))

  support <- replay$support
  expect_true(support$seed_band_member[support$detector_row_index == 2L])
  expect_false(support$seed_band_member[support$detector_row_index == 3L])
  expect_true(support$bridge_band_member[support$detector_row_index == 3L])
  expect_false(support$bridge_band_member[support$detector_row_index == 5L])

  expect_identical(replay$runs$detector_start_row, c(2L, 6L, 9L, 15L, 22L))
  expect_identical(
    replay$runs$terminal_reason,
    c("hard_threshold_run_emitted", "insufficient_spike_count",
      "hard_threshold_run_emitted", "hard_threshold_run_emitted",
      "hard_threshold_run_emitted")
  )
  emitted <- replay$pre_refractory_candidates
  expect_identical(emitted$candidate_id, paste0("hard_isi_threshold_", 1:4))
  expect_identical(emitted$source_run_ordinal, c(1L, 3L, 4L, 5L))
  expect_identical(emitted$final_label,
                   c("burst", "long_burst", "long_burst", "possible_burst"))
  expect_identical(emitted$proposal_track,
                   c("event", "event", "event", "review"))
  expect_true(all(emitted$event_materialization_status ==
                    "proposal_not_final_selection"))
  expect_identical(replay$receipt$cap_applied, FALSE)
  expect_identical(replay$receipt$deduplication_applied, FALSE)
  expect_identical(replay$receipt$run_scan_exhausted, TRUE)
  expect_identical(replay$receipt$bridge_run_n, 5L)
  expect_identical(replay$receipt$pre_refractory_candidate_n, 4L)
  expect_identical(replay$receipt$root_independence_scope,
                   "parallel_candidate_root_with_shared_dat_params_vp")
})

test_that("hard root preserves whole-candidate canonical Pause decisions", {
  raw <- data.frame(
    train = "train_1", candidate_id = "pause_1",
    start_isi = 3L, end_isi = 3L,
    gap_semantics = "canonical_pause", hard_for_event = TRUE,
    action = "accept", stringsAsFactors = FALSE, check.names = FALSE
  )
  boundaries <- stpd_event_core_pause_hard_boundaries(raw)
  context <- hard_root_context(
    c(NA, 0.009, 0.009, 0.009, 0.050, 0.009, 0.009, 0.009, 0.050),
    boundaries = boundaries, raw_pause = raw
  )
  replay <- stpd_candidate_lineage_hard_threshold_replay(context)
  expect_identical(replay$receipt$boundary_blocked_n, 1L)
  expect_identical(replay$boundary_attempts$blocked, c(TRUE, FALSE))
  expect_false(any(replay$boundary_attempts$redetection_performed))
  expect_identical(replay$post_boundary_output$action, c("reject", "accept"))
  expect_identical(
    replay$post_boundary_output$hard_boundary_decision,
    c("rejected_without_redetection", "not_applicable")
  )
  expect_identical(replay$post_boundary_output$detector_start_row,
                   replay$pre_boundary_output$detector_start_row)
  expect_identical(replay$post_boundary_output$detector_end_row,
                   replay$pre_boundary_output$detector_end_row)
})

test_that("refractory-rejected hard proposals remain diagnostic", {
  context <- hard_root_context(c(NA, 0.009, 0.0011, 0.009, 0.050))
  for (path in c("detector", "burst")) {
    context$params[[path]]$refractory_suspect_sec <- 0.0015
    context$params[[path]]$refractory_suspect_action <- "exclude_candidate"
  }
  context$params$spiketrainpattern$qc$refractory_suspect_isi_sec <- 0.0015
  context$params$spiketrainpattern$qc$refractory_suspect_action <-
    "exclude_candidate"
  context$scientific_pre_boundary <-
    stpd_event_core_detect_hard_isi_thresholds(
      context$dat, context$params, context$vp,
      context$min_isi_sec, context$train
    )
  context$scientific_post_boundary <-
    stpd_event_core_apply_hard_boundaries_to_burst_candidates(
      context$scientific_pre_boundary, context$boundaries
    )
  replay <- stpd_candidate_lineage_hard_threshold_replay(context)

  expect_identical(replay$pre_refractory_candidates$proposal_track, "event")
  expect_identical(replay$pre_boundary_output$final_label, "reject")
  expect_identical(replay$pre_boundary_output$action, "reject")
  expect_identical(replay$pre_boundary_output$proposal_track, "diagnostic")
  expect_identical(replay$post_boundary_output$proposal_track, "diagnostic")
  expect_identical(
    replay$receipt$output_role,
    "event_review_or_diagnostic_proposals_not_final_materialization"
  )
})

test_that("hard root reports disabled, unrequested and empty scans faithfully", {
  context <- hard_root_context(c(NA, 0.009, 0.009, 0.009, 0.050),
                               patterns = "tonic")
  unrequested <- stpd_candidate_lineage_hard_threshold_replay(context)
  expect_false(unrequested$entry$burst_family_requested)
  expect_false(unrequested$entry$generation_gated_by_requested_patterns)
  expect_identical(
    unrequested$entry$downstream_pattern_eligibility_status,
    "not_observed_in_this_root_patch"
  )
  expect_false(unrequested$receipt$pause_pattern_requested)
  expect_identical(
    unrequested$receipt$upstream_pause_status,
    "pause_pattern_not_requested"
  )
  expect_gt(nrow(unrequested$pre_refractory_candidates), 0L)

  soft <- context
  soft$params <- hard_root_params(soft$dat, mode = "soft_anchor", hard = FALSE)
  soft$scientific_pre_boundary <- data.frame()
  soft$scientific_post_boundary <- data.frame()
  disabled <- stpd_candidate_lineage_hard_threshold_replay(soft)
  expect_identical(disabled$entry$applicability_status,
                   "invoked_non_hard_threshold_range")
  expect_false(disabled$entry$hard_threshold_applicable)
  expect_identical(nrow(disabled$runs), 0L)
  expect_identical(nrow(disabled$post_boundary_output), 0L)

  no_runs <- hard_root_context(c(NA, 0.020, 0.030, 0.040))
  empty <- stpd_candidate_lineage_hard_threshold_replay(no_runs)
  expect_identical(nrow(empty$runs), 0L)
  expect_identical(empty$receipt$coverage_status,
                   "hard_threshold_run_scan_complete_no_bridge_runs")
})

hard_root_live_run <- function(run_id = "run_hard_root") {
  dat <- hard_root_dat(c(
    NA, 0.040, 0.009, 0.010, 0.009, 0.080,
    0.009, 0.009, 0.009, 0.040
  ))
  params <- hard_root_params(dat)
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("h", 64L), collapse = ""),
    "hard_root", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = shard
  )
  list(dat = dat, params = params, baseline = baseline, observed = observed,
       shard = shard,
       snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard))
}

test_that("live hard-root observation is scientific, RNG and options neutral", {
  set.seed(84231)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()
  run <- hard_root_live_run("run_hard_root_neutral")
  expect_identical(
    serialize(run$observed, NULL, version = 3L),
    serialize(run$baseline, NULL, version = 3L)
  )
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)
  obs <- run$snapshot$observations
  hooks <- c(
    "burst_hard_threshold_entry_v1", "burst_hard_threshold_support_v1",
    "burst_hard_threshold_runs_v1",
    "burst_hard_threshold_pre_refractory_candidates_v1",
    "burst_hard_threshold_refractory_attempts_v1",
    "burst_hard_threshold_pre_boundary_output_v1",
    "burst_hard_threshold_boundary_sources_v1",
    "burst_hard_threshold_boundary_attempts_v1",
    "burst_hard_threshold_post_boundary_output_v1",
    "burst_hard_threshold_receipt_v1"
  )
  expect_true(all(hooks %in% names(obs)))
  expect_identical(obs$burst_hard_threshold_receipt_v1$cap_applied, FALSE)
  expect_identical(
    obs$burst_hard_threshold_receipt_v1$upstream_universe_status,
    "candidate_universe_unavailable"
  )
  expect_true(
    obs$burst_hard_threshold_receipt_v1$pause_pattern_requested
  )
  expect_identical(
    obs$burst_hard_threshold_receipt_v1$upstream_pause_status,
    "post_ownership_contextual_pause_observation_closed"
  )
})

test_that("hard-root validation rejects tampering and a fully replayed input attack", {
  run <- hard_root_live_run("run_hard_root_adversarial")
  obs <- run$snapshot$observations
  before <- stpd_candidate_lineage_collector_observation_snapshot(run$shard)

  original_raw_receipt <-
    run$shard$observations$pause_raw_receipt_v1
  changed_raw_receipt <- original_raw_receipt
  changed_raw_receipt$raw_output_n <-
    changed_raw_receipt$raw_output_n + 1L
  run$shard$observations$pause_raw_receipt_v1 <- changed_raw_receipt
  hard_root_expect_invalid(
    run$shard, "burst_hard_threshold_receipt_v1",
    obs$burst_hard_threshold_receipt_v1
  )
  run$shard$observations$pause_raw_receipt_v1 <- original_raw_receipt
  expect_silent(stpd_candidate_lineage_validate_hook_payload(
    run$shard, "burst_hard_threshold_receipt_v1",
    obs$burst_hard_threshold_receipt_v1
  ))

  mutations <- list(
    burst_hard_threshold_entry_v1 = function(x) {
      x$hard_burst_seed_upper_sec <- x$hard_burst_seed_upper_sec + 0.001; x
    },
    burst_hard_threshold_support_v1 = function(x) {
      x$seed_band_member[[which(x$seed_band_member)[1L]]] <- FALSE; x
    },
    burst_hard_threshold_runs_v1 = function(x) {
      x$core_isi_count[[1L]] <- x$core_isi_count[[1L]] + 1L; x
    },
    burst_hard_threshold_pre_refractory_candidates_v1 = function(x) {
      x$candidate_id[[1L]] <- "forged"; x
    },
    burst_hard_threshold_pre_boundary_output_v1 = function(x) {
      x$priority[[1L]] <- x$priority[[1L]] + 1; x
    },
    burst_hard_threshold_post_boundary_output_v1 = function(x) {
      x$event_materialization_status[[1L]] <- "selected"; x
    },
    burst_hard_threshold_receipt_v1 = function(x) {
      x$pre_refractory_candidate_n <- x$pre_refractory_candidate_n + 1L; x
    }
  )
  for (hook in names(mutations)) {
    payload <- mutations[[hook]](obs[[hook]])
    hard_root_expect_invalid(run$shard, hook, payload)
  }

  forged_context <- unserialize(serialize(
    get("burst_hard_threshold_context", envir = run$shard),
    NULL, version = 3L
  ))
  forged_context$dat$ISI_sec[[3L]] <- 0.020
  forged_context$dat$timestamp_sec <- cumsum(
    replace(forged_context$dat$ISI_sec, 1L, 0)
  )
  forged_context$scientific_pre_boundary <-
    stpd_event_core_detect_hard_isi_thresholds(
      forged_context$dat, forged_context$params, forged_context$vp,
      forged_context$min_isi_sec, forged_context$train
    )
  forged_context$scientific_post_boundary <-
    stpd_event_core_apply_hard_boundaries_to_burst_candidates(
      forged_context$scientific_pre_boundary, forged_context$boundaries
    )
  forged <- stpd_candidate_lineage_hard_threshold_replay(forged_context)
  expect_false(is.null(forged))
  hard_root_expect_invalid(
    run$shard, "burst_hard_threshold_receipt_v1", forged$receipt
  )
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before
  )
})

test_that("fresh self-consistent capture cannot bypass the live begin-context", {
  context <- hard_root_context(c(NA, 0.009, 0.009, 0.009, 0.050))
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, "run_hard_root_fresh_attack",
    paste(rep("a", 64L), collapse = ""), "hard_root", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(
      train = "train_1", n_interval_rows = as.integer(nrow(context$dat)),
      min_isi_sec = context$min_isi_sec,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
  expect_error(
    stpd_candidate_lineage_capture_burst_hard_threshold_root(
      shard, context$train, context$dat, context$params, context$vp,
      context$min_isi_sec, context$patterns,
      context$scientific_pre_boundary, context$scientific_post_boundary,
      context$raw_pause_candidates, context$boundaries
    ),
    class =
      "stpd_candidate_lineage_collector_hard_threshold_begin_context_missing"
  )
  expect_false(exists(
    "burst_hard_threshold_context", envir = shard, inherits = FALSE
  ))
})

test_that("hard root rejects a raw Pause root frozen under other patterns", {
  context <- hard_root_context(
    c(NA, 0.040, 0.009, 0.009, 0.009, 0.040),
    patterns = c("burst", "pause")
  )
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, "run_hard_root_pattern_attack",
    paste(rep("q", 64L), collapse = ""), "hard_root", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(
    collector, "train_1"
  )
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  stpd_candidate_lineage_collector_capture(
    shard, "hf_protected_impl_entry_v1",
    data.frame(
      train = "train_1", n_interval_rows = as.integer(nrow(context$dat)),
      min_isi_sec = context$min_isi_sec,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )

  raw_patterns <- "tonic"
  raw_empty <- data.frame()
  raw_boundaries <- stpd_event_core_pause_hard_boundaries(raw_empty)
  stpd_candidate_lineage_begin_pause_raw_canonical(
    shard, context$train, context$dat, context$params, context$vp,
    context$min_isi_sec, raw_patterns
  )
  stpd_candidate_lineage_capture_pause_raw_canonical(
    shard, context$train, context$dat, context$params, context$vp,
    context$min_isi_sec, raw_patterns, raw_empty, raw_boundaries
  )

  expect_false(stpd_candidate_lineage_pause_raw_outputs_are_closed(
    shard, raw_empty, raw_boundaries, science_context = context
  ))
  expect_error(
    stpd_candidate_lineage_begin_burst_hard_threshold_root(
      shard, context$train, context$dat, context$params, context$vp,
      context$min_isi_sec, context$patterns,
      raw_empty, raw_boundaries
    ),
    class =
      "stpd_candidate_lineage_collector_hard_threshold_entry_not_closed"
  )
  expect_false(exists(
    "burst_hard_threshold_begin_context", envir = shard, inherits = FALSE
  ))
})

test_that("wrong-train capture fails closed", {
  run <- hard_root_live_run("run_hard_root_wrong_train")
  context <- get("burst_hard_threshold_context", envir = run$shard)
  expect_error(
    stpd_candidate_lineage_capture_burst_hard_threshold_root(
      run$shard, "train_2", context$dat, context$params, context$vp,
      context$min_isi_sec, context$patterns,
      context$scientific_pre_boundary, context$scientific_post_boundary,
      context$raw_pause_candidates, context$boundaries
    ),
    class = "stpd_candidate_lineage_collector_train_scope_invalid"
  )
})

test_that("collector-off never enters hard-root observation", {
  dat <- hard_root_dat(c(NA, 0.040, 0.009, 0.009, 0.009, 0.040))
  params <- hard_root_params(dat)
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_begin_burst_hard_threshold_root = function(...) {
      stop("hard-root begin-context entered while collector is off")
    },
    stpd_candidate_lineage_capture_burst_hard_threshold_root = function(...) {
      stop("hard-root observation entered while collector is off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = NULL
  )
  expect_identical(
    serialize(observed, NULL, version = 3L),
    serialize(baseline, NULL, version = 3L)
  )
})
