contextual_pause_dat <- function(isi) {
  data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(replace(isi, 1L, 0)), ISI_sec = isi,
    pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

contextual_pause_params_vp <- function(dat, patterns = c("burst", "pause")) {
  params <- effective_params_for_detector(default_params())
  params$detector$patterns_to_run <- patterns
  params$event_grammar$pause_relative_local_factor <- 1.55
  params$burst$merge_gap_max_n <- 2L
  params$burst$event_core_max_bridge_isi_count <- 4L
  vp <- stpd_event_grammar_params_impl(dat, params, 0.001, "train_1")
  vp$bridge_high <- 0.015
  vp$pause_thr <- 0.080
  list(params = params, vp = vp)
}

contextual_pause_bursts <- function(left = c(5L, 7L), right = c(9L, 11L),
                                    selected = TRUE) {
  data.frame(
    candidate_id = c("left_burst", "right_burst"),
    final_label = c("burst", "burst"), action = c("accept", "accept"),
    decision_action = c("accept", "accept"),
    start_isi = c(left[[1L]], right[[1L]]),
    end_isi = c(left[[2L]], right[[2L]]),
    selected_for_auto = rep(selected, 2L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

contextual_pause_generic <- function(start = 8L, end = start,
                                     candidate_id = "raw_pause_1") {
  data.frame(
    train = "train_1", candidate_id = candidate_id,
    start_isi = as.integer(start), end_isi = as.integer(end),
    final_label = "pause", action = "accept", decision_action = "accept",
    score = 1.5, gap_semantics = "canonical_pause",
    hard_for_event = TRUE, hard_for_state_direct_support = TRUE,
    envelope_bridge_eligible = FALSE,
    pause_candidate_decision = "accepted",
    pause_candidate_reason_code = "raw_canonical_fixture",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

contextual_pause_fixture <- function(gap = 0.060, gap_n = 1L,
                                     patterns = c("burst", "pause")) {
  gap_values <- rep(0.012, gap_n)
  gap_values[[ceiling(gap_n / 2)]] <- gap
  isi <- c(NA_real_, rep(0.040, 3L), rep(0.006, 3L), gap_values,
           rep(0.006, 3L), rep(0.040, 4L))
  dat <- contextual_pause_dat(isi)
  left_end <- 7L
  right_start <- left_end + gap_n + 1L
  bursts <- contextual_pause_bursts(
    c(5L, 7L), c(right_start, right_start + 2L)
  )
  pv <- contextual_pause_params_vp(dat, patterns)
  list(dat = dat, params = pv$params, vp = pv$vp, bursts = bursts,
       candidate_row = left_end + ceiling(gap_n / 2L), gap_n = gap_n)
}

contextual_pause_live_collector <- function(run_id) {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("c", 64L), collapse = ""),
    "pause_contextual", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  list(collector = collector, shard = shard)
}

test_that("generic Pause downgrade is append-only and ownership-specific", {
  dat <- contextual_pause_dat(c(NA, rep(0.020, 15L)))
  pv <- contextual_pause_params_vp(dat)
  generic <- rbind(
    contextual_pause_generic(8L, candidate_id = "blocked"),
    contextual_pause_generic(13L, candidate_id = "retained")
  )
  support <- data.frame(
    candidate_id = "owner", final_label = "burst", action = "accept",
    start_isi = 5L, end_isi = 11L, selected_for_auto = TRUE,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out <- stpd_event_core_detect_pause(
    dat, pv$params, pv$vp, 0.001, "train_1",
    burst_candidates = support, generic_candidates = generic
  )
  expect_identical(out$candidate_id, c("blocked", "retained"))
  expect_identical(out$start_isi, c(8L, 13L))
  expect_equal(out$score, c(1.5, 1.5))
  expect_identical(out$gap_semantics, c("ambiguous_gap", "canonical_pause"))
  expect_identical(out$action, c("reject", "accept"))
  expect_identical(out$decision_action, c("reject", "accept"))
  expect_identical(out$hard_for_event, c(FALSE, TRUE))
  expect_identical(out$hard_for_state_direct_support, c(FALSE, TRUE))
  expect_identical(out$envelope_bridge_eligible, c(TRUE, FALSE))
  expect_identical(
    out$pause_candidate_decision,
    c("blocked_by_frozen_burst_support", "accepted")
  )
  expect_identical(
    out$pause_candidate_reason_code,
    c("generic_pause_overlaps_frozen_burst_bridge_or_boundary",
      "raw_canonical_fixture")
  )
})

test_that("unselected and non-Burst support cannot downgrade generic Pause", {
  dat <- contextual_pause_dat(c(NA, rep(0.020, 15L)))
  pv <- contextual_pause_params_vp(dat)
  generic <- rbind(
    contextual_pause_generic(6L, candidate_id = "owned_raw_pause"),
    contextual_pause_generic(8L, candidate_id = "raw_pause_1")
  )
  cases <- list(
    transform(contextual_pause_bursts(selected = FALSE), start_isi = c(5L, 5L),
              end_isi = c(11L, 11L)),
    transform(contextual_pause_bursts(), final_label = c("tonic", "pause"),
              start_isi = c(5L, 5L), end_isi = c(11L, 11L)),
    contextual_pause_bursts()[0, , drop = FALSE]
  )
  for (support in cases) {
    out <- stpd_event_core_detect_pause(
      dat, pv$params, pv$vp, 0.001, "train_1",
      burst_candidates = support, generic_candidates = generic
    )
    expect_identical(out, generic)
  }
})

test_that("interburst proposal uses a strict local non-bridge threshold", {
  equal <- contextual_pause_fixture(gap = 0.015, gap_n = 1L)
  at_gate <- stpd_event_core_detect_pause(
    equal$dat, equal$params, equal$vp, 0.001, "train_1",
    burst_candidates = equal$bursts, generic_candidates = data.frame()
  )
  expect_equal(nrow(at_gate), 0L)

  above <- contextual_pause_fixture(gap = 0.0151, gap_n = 1L)
  hit <- stpd_event_core_detect_pause(
    above$dat, above$params, above$vp, 0.001, "train_1",
    burst_candidates = above$bursts, generic_candidates = data.frame()
  )
  expect_identical(hit$start_isi, 8L)
  expect_identical(hit$end_isi, 8L)
  expect_equal(hit$pause_context_burst_median_sec, 0.006)
  expect_equal(hit$pause_context_bridge_upper_sec, 0.015)
  expect_equal(hit$pause_effective_threshold_sec, 0.015)
  expect_equal(hit$score, 0.0151 / 0.015)
  expect_identical(hit$gap_semantics, "contextual_interburst_pause")
  expect_identical(hit$candidate_id, "event_core_interburst_pause_1")
})

test_that("interburst gap cap, tie-first and invalid support are deterministic", {
  four <- contextual_pause_fixture(gap = 0.020, gap_n = 4L)
  # Two equal maxima: which.max must retain the first one.
  four$dat$ISI_sec[c(8L, 10L)] <- 0.020
  four$dat$timestamp_sec <- cumsum(replace(four$dat$ISI_sec, 1L, 0))
  hit <- stpd_event_core_detect_pause(
    four$dat, four$params, four$vp, 0.001, "train_1",
    burst_candidates = four$bursts, generic_candidates = data.frame()
  )
  expect_identical(hit$start_isi, 8L)
  expect_identical(hit$pause_context_gap_isi_n, 4L)

  five <- contextual_pause_fixture(gap = 0.060, gap_n = 5L)
  expect_equal(nrow(stpd_event_core_detect_pause(
    five$dat, five$params, five$vp, 0.001, "train_1",
    burst_candidates = five$bursts, generic_candidates = data.frame()
  )), 0L)

  invalid <- contextual_pause_fixture(gap = 0.0005, gap_n = 1L)
  expect_equal(nrow(stpd_event_core_detect_pause(
    invalid$dat, invalid$params, invalid$vp, 0.001, "train_1",
    burst_candidates = invalid$bursts, generic_candidates = data.frame()
  )), 0L)
})

test_that("generic-first same geometry is not deduplicated or capped", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  generic <- contextual_pause_generic(8L)
  x$vp$max_candidates <- 1L
  one <- stpd_event_core_detect_pause(
    x$dat, x$params, x$vp, 0.001, "train_1",
    burst_candidates = x$bursts, generic_candidates = generic
  )
  x$vp$max_candidates <- 100L
  many <- stpd_event_core_detect_pause(
    x$dat, x$params, x$vp, 0.001, "train_1",
    burst_candidates = x$bursts, generic_candidates = generic
  )
  expect_identical(serialize(one, NULL, version = 3L),
                   serialize(many, NULL, version = 3L))
  expect_identical(nrow(one), 2L)
  expect_identical(one$start_isi, c(8L, 8L))
  expect_identical(one$candidate_id,
                   c("raw_pause_1", "event_core_interburst_pause_1"))
  expect_identical(one$gap_semantics,
                   c("canonical_pause", "contextual_interburst_pause"))
})

contextual_pause_replay_context <- function(x, generic = data.frame(),
                                            patterns = c("burst", "pause"),
                                            burst_support = x$bursts,
                                            burst_route = "final") {
  x$params$detector$patterns_to_run <- patterns
  scientific <- if ("pause" %in% patterns) {
    stpd_event_core_detect_pause(
      x$dat, x$params, x$vp, 0.001, "train_1",
      burst_candidates = burst_support, generic_candidates = generic
    )
  } else data.frame()
  list(
    train = "train_1", dat = x$dat, params = x$params, vp = x$vp,
    min_isi_sec = 0.001, patterns = patterns, raw_candidates = generic,
    boundaries = data.frame(), burst_support = burst_support,
    burst_route = burst_route, scientific_out = scientific
  )
}

test_that("same-geometry count records unique routes rather than proposal rows", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  generic <- rbind(
    contextual_pause_generic(8L, candidate_id = "raw_pause_1"),
    contextual_pause_generic(8L, candidate_id = "raw_pause_2")
  )
  context <- contextual_pause_replay_context(x, generic)
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "ownership_observation_closed")
    },
    .package = "SpikeTrainPatternDetector"
  )
  replay <- stpd_candidate_lineage_contextual_pause_replay(context, new.env())
  expect_identical(replay$combined$route,
                   c("generic_post", "generic_post", "interburst"))
  expect_identical(replay$combined$same_geometry_route_n, c(2L, 2L, 2L))
  expect_identical(replay$receipt$route_collision_n, 1L)
})

test_that("contextual replay closes independently checked routes and receipt", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  generic <- rbind(
    contextual_pause_generic(6L, candidate_id = "owned_raw_pause"),
    contextual_pause_generic(8L, candidate_id = "raw_pause_1")
  )
  context <- contextual_pause_replay_context(x, generic)
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "ownership_observation_closed")
    },
    .package = "SpikeTrainPatternDetector"
  )
  replay <- stpd_candidate_lineage_contextual_pause_replay(context, new.env())
  expect_false(is.null(replay))
  expect_identical(replay$generic_input$candidate_id,
                   c("owned_raw_pause", "raw_pause_1"))
  expect_identical(replay$generic_post$action, c("reject", "accept"))
  expect_identical(replay$generic_post$selection_eligibility,
                   c("not_selectable_action_reject",
                     "eligible_for_downstream_preselection"))
  expect_identical(replay$downgrade_attempts$overlapping_support_n, c(1L, 0L))
  expect_identical(replay$downgrade_attempts$terminal_reason,
                   c("overlaps_frozen_burst_support", "no_burst_ownership_conflict"))
  expect_identical(replay$downgrade_attempts$source_payload_sha256,
                   replay$generic_input$candidate_payload_sha256)
  expect_identical(replay$downgrade_attempts$output_payload_sha256,
                   replay$generic_post$candidate_payload_sha256)
  expect_identical(replay$downgrade_attempts$before_action,
                   c("accept", "accept"))
  expect_identical(replay$downgrade_attempts$after_action,
                   c("reject", "accept"))
  expect_identical(replay$downgrade_attempts$before_hard_for_event,
                   c(TRUE, TRUE))
  expect_identical(replay$downgrade_attempts$after_hard_for_event,
                   c(FALSE, TRUE))
  expect_identical(replay$pair_attempts$raw_gap_isi_n, 1L)
  expect_identical(replay$pair_attempts$candidate_isi, 8L)
  expect_equal(replay$pair_attempts$flank_median_sec, 0.006)
  expect_equal(replay$pair_attempts$bridge_upper_sec, 0.015)
  expect_equal(replay$pair_attempts$burst_factor, 1.55)
  expect_equal(replay$pair_attempts$contrast_threshold_sec, 0.0093)
  expect_identical(replay$pair_attempts$threshold_comparator,
                   "strict_greater_than")
  expect_identical(replay$pair_attempts$terminal_reason, "candidate_emitted")
  expect_identical(replay$interburst$candidate_id,
                   "event_core_interburst_pause_1")
  expect_identical(replay$pair_attempts$emitted_output_payload_sha256,
                   replay$interburst$candidate_payload_sha256)
  # Same geometry is deliberately represented twice: generic proposal first,
  # contextual proposal second.  This observer does not select a final Gap.
  expect_identical(replay$combined$output_ordinal, 1:3)
  expect_identical(replay$combined$route,
                   c("generic_post", "generic_post", "interburst"))
  expect_identical(replay$combined$detector_start_row, c(6L, 8L, 8L))
  expect_identical(replay$combined$candidate_id,
                   c("owned_raw_pause", "raw_pause_1",
                     "event_core_interburst_pause_1"))
  expect_identical(replay$receipt$generic_input_n, 2L)
  expect_identical(replay$receipt$generic_downgraded_n, 1L)
  expect_identical(replay$receipt$interburst_pair_attempt_n, 1L)
  expect_identical(replay$receipt$interburst_candidate_n, 1L)
  expect_identical(replay$receipt$combined_output_n, 3L)
  expect_identical(replay$receipt$route_collision_n, 1L)
  expect_false(replay$receipt$audit_capture_cap_applied)
  expect_false(replay$receipt$deduplication_applied)
  expect_identical(replay$receipt$final_gap_selection_status,
                   "not_observed")
  expect_identical(replay$receipt$recurrent_pause_state_status,
                   "not_evaluated_post_detection_only")
  expect_identical(replay$receipt$hfs_direct_support_materialization_status,
                   "not_observed")
  expect_identical(replay$receipt$hfs_envelope_status, "not_observed")
  expect_identical(replay$receipt$scientific_result_influence,
                   "none_observer_only")
  expect_false(replay$receipt$publication_authority)
})

test_that("pair audit leaves scientifically unvisited stages typed NA", {
  capped <- contextual_pause_fixture(gap = 0.060, gap_n = 5L)
  invalid_bridge <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  invalid_bridge$vp$bridge_high <- NA_real_
  equal_gate <- contextual_pause_fixture(gap = 0.015, gap_n = 1L)
  contexts <- list(
    contextual_pause_replay_context(capped),
    contextual_pause_replay_context(invalid_bridge),
    contextual_pause_replay_context(equal_gate)
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "ownership_observation_closed")
    },
    .package = "SpikeTrainPatternDetector"
  )
  replay <- lapply(contexts, stpd_candidate_lineage_contextual_pause_replay,
                   shard = new.env())

  cap_attempt <- replay[[1L]]$pair_attempts
  expect_identical(cap_attempt$terminal_reason, "gap_exceeds_count_cap")
  expect_true(is.na(cap_attempt$valid_gap_isi_n))
  expect_true(is.na(cap_attempt$candidate_isi))
  expect_true(is.na(cap_attempt$flank_median_sec))
  expect_true(is.na(cap_attempt$threshold_sec))
  expect_identical(cap_attempt$candidate_selection_rule, "not_evaluated")
  expect_identical(cap_attempt$threshold_comparator, "not_evaluated")
  expect_true(is.na(cap_attempt$strict_threshold_pass))

  bridge_attempt <- replay[[2L]]$pair_attempts
  expect_identical(bridge_attempt$terminal_reason, "invalid_bridge_upper")
  expect_identical(bridge_attempt$valid_gap_isi_n, 1L)
  expect_identical(bridge_attempt$candidate_selection_rule,
                   "which.max_first_valid_isi_on_tie")
  expect_true(is.na(bridge_attempt$contrast_threshold_sec))
  expect_true(is.na(bridge_attempt$threshold_sec))
  expect_identical(bridge_attempt$threshold_comparator, "not_evaluated")
  expect_true(is.na(bridge_attempt$strict_threshold_pass))

  equal_attempt <- replay[[3L]]$pair_attempts
  expect_identical(equal_attempt$terminal_reason,
                   "fails_strict_context_threshold")
  expect_identical(equal_attempt$threshold_comparator, "strict_greater_than")
  expect_false(equal_attempt$strict_threshold_pass)
})

test_that("contextual gating yields typed zero without scanning proposals", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  cases <- list(
    contextual_pause_replay_context(x, patterns = "burst"),
    within(contextual_pause_replay_context(x), {
      dat <- dat[1:2, , drop = FALSE]; scientific_out <- data.frame()
    }),
    within(contextual_pause_replay_context(x), {
      vp$pause_thr <- NA_real_; scientific_out <- data.frame()
    })
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "ownership_observation_closed")
    },
    .package = "SpikeTrainPatternDetector"
  )
  for (i in seq_along(cases)) {
    replay <- stpd_candidate_lineage_contextual_pause_replay(cases[[i]], new.env())
    expect_false(is.null(replay))
    for (nm in c("generic_input", "ownership_support", "downgrade_attempts",
                 "generic_post", "pair_attempts", "interburst", "combined")) {
      expect_identical(nrow(replay[[nm]]), 0L, info = paste(i, nm))
    }
    expect_identical(replay$receipt$generic_input_n, 0L)
    expect_identical(replay$receipt$interburst_pair_attempt_n, 0L)
    expect_false(replay$receipt$scan_exhausted)
  }
})

test_that("contextual two-phase lock rejects missing and changed capture atomically", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  context <- contextual_pause_replay_context(x)
  cc <- contextual_pause_live_collector("contextual_lock")
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_pause_raw_outputs_are_closed = function(...) TRUE,
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "ownership_observation_closed")
    },
    .package = "SpikeTrainPatternDetector"
  )
  before <- stpd_candidate_lineage_collector_observation_snapshot(cc$shard)
  expect_error(do.call(stpd_candidate_lineage_capture_pause_contextual,
    c(list(shard = cc$shard), context)),
    class = "stpd_candidate_lineage_collector_contextual_pause_begin_missing")
  expect_identical(stpd_candidate_lineage_collector_observation_snapshot(cc$shard),
                   before)
  begin_args <- context[names(context) != "scientific_out"]
  do.call(stpd_candidate_lineage_begin_pause_contextual,
          c(list(shard = cc$shard), begin_args))
  changed <- context; changed$vp$bridge_high <- changed$vp$bridge_high + 0.001
  expect_error(do.call(stpd_candidate_lineage_capture_pause_contextual,
    c(list(shard = cc$shard), changed)),
    class = "stpd_candidate_lineage_collector_contextual_pause_begin_conflict")
  expect_identical(stpd_candidate_lineage_collector_observation_snapshot(cc$shard),
                   before)
})

test_that("every contextual hook rejects semantic mutation without partial capture", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  context <- contextual_pause_replay_context(x, contextual_pause_generic(8L))
  cc <- contextual_pause_live_collector("contextual_hooks")
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_pause_raw_outputs_are_closed = function(...) TRUE,
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "ownership_observation_closed")
    },
    .package = "SpikeTrainPatternDetector"
  )
  begin_args <- context[names(context) != "scientific_out"]
  do.call(stpd_candidate_lineage_begin_pause_contextual,
          c(list(shard = cc$shard), begin_args))
  do.call(stpd_candidate_lineage_capture_pause_contextual,
          c(list(shard = cc$shard), context))
  before <- stpd_candidate_lineage_collector_observation_snapshot(cc$shard)
  obs <- before$observations
  mutations <- list(
    contextual_pause_entry_v1 = function(z) { z$publication_authority <- TRUE; z },
    contextual_pause_generic_input_v1 = function(z) { z$action[[1L]] <- "forged"; z },
    contextual_pause_ownership_support_v1 = function(z) { z$detector_end_row[[1L]] <- z$detector_end_row[[1L]] + 1L; z },
    contextual_pause_generic_downgrade_attempts_v1 = function(z) { z$terminal_reason[[1L]] <- "forged"; z },
    contextual_pause_generic_post_downgrade_v1 = function(z) { z$gap_semantics[[1L]] <- "forged"; z },
    contextual_pause_interburst_pair_attempts_v1 = function(z) { z$candidate_isi[[1L]] <- z$candidate_isi[[1L]] + 1L; z },
    contextual_pause_interburst_candidates_v1 = function(z) { z$action[[1L]] <- "reject"; z },
    contextual_pause_combined_output_v1 = function(z) { z$output_ordinal[[1L]] <- 99L; z },
    contextual_pause_receipt_v1 = function(z) { z$combined_output_n <- z$combined_output_n + 1L; z }
  )
  for (hook in names(mutations)) {
    err <- tryCatch(stpd_candidate_lineage_validate_hook_payload(
      cc$shard, hook, mutations[[hook]](obs[[hook]])), error = identity)
    expect_s3_class(err,
      "stpd_candidate_lineage_collector_hook_payload_schema_invalid")
  }
  expect_identical(stpd_candidate_lineage_collector_observation_snapshot(cc$shard),
                   before)
})

test_that("collector-off cannot enter contextual Pause observers", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  baseline <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE
  )
  seed_before <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    get(".Random.seed", .GlobalEnv) else NULL
  kind_before <- RNGkind(); options_before <- options()
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_begin_pause_contextual = function(...) {
      stop("contextual begin entered with collector off")
    },
    stpd_candidate_lineage_capture_pause_contextual = function(...) {
      stop("contextual capture entered with collector off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = NULL
  )
  expect_identical(serialize(observed, NULL, version = 3L),
                   serialize(baseline, NULL, version = 3L))
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)
  seed_after <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    get(".Random.seed", .GlobalEnv) else NULL
  expect_identical(seed_after, seed_before)
})

contextual_pause_active_run <- function(x, run_id, with_collector = TRUE) {
  # Keep this an actual hf_protected call while bounding unrelated Burst
  # proposal enumeration; contextual Pause itself has no cap.
  x$params$burst$max_candidates_per_train <- 5L
  x$params$burst$event_core_max_candidates_per_train <- 5L
  x$params$burst$event_grammar_max_candidates_per_train <- 5L
  x$params$burst$dataset_isi_max_candidates_per_train <- 5L
  x$params$burst$structure_max_candidates_per_train <- 5L
  x$params$burst$local_compression_max_candidates <- 5L
  cc <- if (with_collector) contextual_pause_live_collector(run_id) else NULL
  out <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = if (is.null(cc)) NULL else cc$shard
  )
  list(out = out, cc = cc, snapshot = if (is.null(cc)) NULL else
    stpd_candidate_lineage_collector_observation_snapshot(cc$shard))
}

.contextual_pause_active_cache <- new.env(parent = emptyenv())
contextual_pause_burst_active_cached <- function() {
  if (!exists("run", .contextual_pause_active_cache, inherits = FALSE)) {
    x <- contextual_pause_fixture(gap = 0.020, gap_n = 1L)
    assign("run", contextual_pause_active_run(
      x, "contextual_burst_cached", TRUE
    ), .contextual_pause_active_cache)
  }
  get("run", .contextual_pause_active_cache, inherits = FALSE)
}

test_that("active collector full is byte-neutral for success and zero-output", {
  cases <- list(
    contextual_pause_fixture(gap = 0.100, gap_n = 1L, patterns = "pause"),
    contextual_pause_fixture(gap = 0.020, gap_n = 1L, patterns = "tonic")
  )
  for (i in seq_along(cases)) {
    set.seed(7100 + i); kind0 <- RNGkind(); opt0 <- options()
    off <- contextual_pause_active_run(cases[[i]], paste0("off_", i), FALSE)
    seed_off <- get(".Random.seed", .GlobalEnv); kind_off <- RNGkind(); opt_off <- options()
    set.seed(7100 + i)
    full <- contextual_pause_active_run(cases[[i]], paste0("full_", i), TRUE)
    seed_full <- get(".Random.seed", .GlobalEnv)
    expect_identical(serialize(full$out, NULL, version = 3L),
                     serialize(off$out, NULL, version = 3L))
    expect_identical(seed_full, seed_off)
    expect_identical(RNGkind(), kind_off)
    expect_identical(options(), opt_off)
    expect_identical(kind_off, kind0)
    expect_identical(opt_off, opt0)
    obs <- full$snapshot$observations
    expect_false(is.null(obs$contextual_pause_receipt_v1))
    if (i == 2L) {
      expect_identical(obs$contextual_pause_receipt_v1$combined_output_n, 0L)
      expect_false(obs$contextual_pause_receipt_v1$scan_exhausted)
    }
  }
})

test_that("active path records no generic downgrade after canonical hard cut", {
  run <- contextual_pause_burst_active_cached()
  obs <- run$snapshot$observations
  receipt <- obs$contextual_pause_receipt_v1
  expect_identical(obs$contextual_pause_receipt_v1$generic_downgraded_n, 0L)
  if (nrow(obs$contextual_pause_generic_downgrade_attempts_v1)) {
    expect_false(any(obs$contextual_pause_generic_downgrade_attempts_v1$downgraded))
  }
  # This is an active-pipeline structural result, not a claim that the generic
  # downgrade branch (tested above directly) is unreachable for every dataset.
  expect_identical(obs$contextual_pause_receipt_v1$final_gap_selection_status,
                   "not_observed")
  expect_identical(receipt$root_id, "post_ownership_pause_proposal_root")
  expect_identical(receipt$scientific_output_validation_status,
                   "validated_exact_live_scientific_output")
  hash_fields <- c(
    "upstream_raw_receipt_sha256", "upstream_ownership_receipt_sha256",
    "entry_payload_sha256", "generic_input_payload_sha256",
    "ownership_support_payload_sha256", "downgrade_attempt_payload_sha256",
    "generic_post_payload_sha256", "pair_attempt_payload_sha256",
    "interburst_payload_sha256", "combined_payload_sha256",
    "scientific_output_sha256"
  )
  expect_true(all(grepl("^[0-9a-f]{64}$",
                        unlist(receipt[hash_fields], use.names = FALSE))))
})

test_that("stored raw and ownership mutations invalidate contextual receipt", {
  run <- contextual_pause_burst_active_cached()
  shard <- run$cc$shard; obs <- shard$observations
  receipt <- obs$contextual_pause_receipt_v1
  expect_silent(stpd_candidate_lineage_validate_hook_payload(
    shard, "contextual_pause_receipt_v1", receipt))

  raw <- obs$pause_raw_receipt_v1
  shard$observations$pause_raw_receipt_v1$raw_output_n <- raw$raw_output_n + 1L
  expect_error(stpd_candidate_lineage_validate_hook_payload(
    shard, "contextual_pause_receipt_v1", receipt),
    class = "stpd_candidate_lineage_collector_hook_payload_schema_invalid")
  shard$observations$pause_raw_receipt_v1 <- raw
  expect_silent(stpd_candidate_lineage_validate_hook_payload(
    shard, "contextual_pause_receipt_v1", receipt))

  owner <- obs$burst_pause_ownership_receipt_v1
  shard$observations$burst_pause_ownership_receipt_v1$output_support_n <-
    owner$output_support_n + 1L
  expect_error(stpd_candidate_lineage_validate_hook_payload(
    shard, "contextual_pause_receipt_v1", receipt),
    class = "stpd_candidate_lineage_collector_hook_payload_schema_invalid")
  shard$observations$burst_pause_ownership_receipt_v1 <- owner
  expect_silent(stpd_candidate_lineage_validate_hook_payload(
    shard, "contextual_pause_receipt_v1", receipt))

  owner_out <- obs$burst_pause_ownership_output_v1
  if (nrow(owner_out)) {
    shard$observations$burst_pause_ownership_output_v1$ownership_role[[1L]] <-
      "forged"
    expect_error(stpd_candidate_lineage_validate_hook_payload(
      shard, "contextual_pause_receipt_v1", receipt),
      class = "stpd_candidate_lineage_collector_hook_payload_schema_invalid")
    shard$observations$burst_pause_ownership_output_v1 <- owner_out
    expect_silent(stpd_candidate_lineage_validate_hook_payload(
      shard, "contextual_pause_receipt_v1", receipt))
  }
})

test_that("fresh wrong-pattern cross-root attack fails before contextual bind", {
  x <- contextual_pause_fixture(gap = 0.100, gap_n = 1L, patterns = "pause")
  cc <- contextual_pause_live_collector("contextual_wrong_patterns")
  stpd_candidate_lineage_collector_capture(
    cc$shard, "hf_protected_impl_entry_v1",
    data.frame(train = "train_1", n_interval_rows = as.integer(nrow(x$dat)),
      min_isi_sec = 0.001, stringsAsFactors = FALSE, check.names = FALSE)
  )
  stpd_candidate_lineage_begin_pause_raw_canonical(
    cc$shard, "train_1", x$dat, x$params, x$vp, 0.001, "pause"
  )
  raw <- stpd_event_core_detect_pause(
    x$dat, x$params, x$vp, 0.001, "train_1", burst_candidates = NULL
  )
  boundaries <- stpd_event_core_pause_hard_boundaries(raw)
  stpd_candidate_lineage_capture_pause_raw_canonical(
    cc$shard, "train_1", x$dat, x$params, x$vp, 0.001, "pause",
    raw, boundaries
  )
  expect_error(stpd_candidate_lineage_begin_pause_contextual(
    cc$shard, "train_1", x$dat, x$params, x$vp, 0.001,
    c("burst", "pause"), raw, boundaries, data.frame(), "final"),
    class = "stpd_candidate_lineage_collector_contextual_pause_raw_not_closed")
  expect_false(exists("contextual_pause_begin_context", cc$shard,
                      inherits = FALSE))
})

test_that("alternate Burst route is typed unavailable without candidate detail", {
  x <- contextual_pause_fixture(gap = 0.060, gap_n = 1L)
  context <- contextual_pause_replay_context(
    x, contextual_pause_generic(8L), burst_route = "alternate"
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "unavailable_alternate_burst_route")
    },
    .package = "SpikeTrainPatternDetector"
  )
  replay <- stpd_candidate_lineage_contextual_pause_replay(context, new.env())
  expect_identical(replay$entry$applicability_status,
    "invoked_but_lineage_unavailable_alternate_burst_route")
  for (nm in c("generic_input", "ownership_support", "downgrade_attempts",
               "generic_post", "pair_attempts", "interburst", "combined")) {
    expect_identical(nrow(replay[[nm]]), 0L, info = nm)
  }
  expect_identical(replay$receipt$candidate_universe_status, "unavailable")
  expect_identical(replay$receipt$scientific_role,
                   "proposal_lineage_unavailable")
  expect_identical(replay$receipt$scientific_output_validation_status,
    "not_validated_lineage_unavailable_alternate_burst_route")
  expect_identical(replay$receipt$scientific_output_sha256, "")
  expect_false(replay$receipt$scan_exhausted)
})

test_that("interburst construction IDs count qualifying attempts, not successes", {
  isi <- c(NA_real_, rep(0.040, 3L), rep(0.006, 3L), 0.020,
           rep(0.006, 3L), 0.021, rep(0.006, 3L), rep(0.040, 3L))
  dat <- contextual_pause_dat(isi)
  pv <- contextual_pause_params_vp(dat)
  bursts <- data.frame(
    candidate_id = c("burst_a", "burst_b", "burst_c"),
    final_label = "burst", action = "accept", decision_action = "accept",
    start_isi = c(5L, 9L, 13L), end_isi = c(7L, 11L, 15L),
    selected_for_auto = TRUE, stringsAsFactors = FALSE, check.names = FALSE
  )
  original_constructor <- stpd_event_core_candidate_from_run
  calls <- 0L
  fail_first <- function(...) {
    calls <<- calls + 1L
    if (calls == 1L) return(NULL)
    original_constructor(...)
  }
  testthat::local_mocked_bindings(
    stpd_event_core_candidate_from_run = fail_first,
    .package = "SpikeTrainPatternDetector"
  )
  scientific <- stpd_event_core_detect_pause(
    dat, pv$params, pv$vp, 0.001, "train_1",
    burst_candidates = bursts, generic_candidates = data.frame()
  )
  expect_identical(calls, 2L)
  expect_identical(scientific$candidate_id,
                   "event_core_interburst_pause_2")

  calls <- 0L
  context <- list(
    train = "train_1", dat = dat, params = pv$params, vp = pv$vp,
    min_isi_sec = 0.001, patterns = c("burst", "pause"),
    raw_candidates = data.frame(), boundaries = data.frame(),
    burst_support = bursts, burst_route = "final",
    scientific_out = scientific
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_contextual_pause_upstream = function(...) {
      list(ok = TRUE, ownership = "ownership_observation_closed")
    },
    .package = "SpikeTrainPatternDetector"
  )
  replay <- stpd_candidate_lineage_contextual_pause_replay(context, new.env())
  expect_identical(calls, 2L)
  expect_identical(replay$pair_attempts$pair_ordinal, 1:2)
  expect_identical(replay$pair_attempts$terminal_reason,
                   c("candidate_construction_failed", "candidate_emitted"))
  expect_identical(replay$pair_attempts$emitted_candidate_id,
                   c("", "event_core_interburst_pause_2"))
  expect_identical(replay$pair_attempts$emitted_output_payload_sha256[[1L]], "")
  expect_identical(
    replay$pair_attempts$emitted_output_payload_sha256[[2L]],
    stpd_candidate_lineage_contextual_pause_hash(
      scientific[1L, , drop = FALSE], "interburst-candidate"
    )
  )
  expect_identical(replay$interburst$candidate_id,
                   "event_core_interburst_pause_2")
  expect_identical(replay$receipt$interburst_pair_attempt_n, 2L)
  expect_identical(replay$receipt$interburst_candidate_n, 1L)
})
