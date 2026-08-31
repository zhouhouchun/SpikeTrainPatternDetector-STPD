pause_ownership_dat <- function(gap_isi = 0.020) {
  isi <- c(NA_real_, rep(0.040, 3L), rep(0.006, 3L), gap_isi,
           rep(0.006, 3L), rep(0.040, 4L))
  data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(replace(isi, 1L, 0)), ISI_sec = isi,
    pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

pause_ownership_atoms <- function(witness = TRUE) {
  out <- data.frame(
    train = "train_1",
    candidate_id = c("left_burst", "right_burst"),
    final_label = "burst", action = "accept",
    start_isi = c(5L, 9L), end_isi = c(7L, 11L),
    selected_for_auto = TRUE, stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (isTRUE(witness)) {
    out <- rbind(out, data.frame(
      train = "train_1", candidate_id = "combined_burst_witness",
      final_label = "burst", action = "accept",
      start_isi = 5L, end_isi = 11L, selected_for_auto = FALSE,
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  out
}

pause_ownership_context <- function(
    gap_isi = 0.020, candidates = pause_ownership_atoms(TRUE),
    boundaries = data.frame(
      start_isi = integer(), end_isi = integer(),
      boundary_kind = character(), stringsAsFactors = FALSE
    )) {
  dat <- pause_ownership_dat(gap_isi)
  params <- default_params_sec()
  params$event_grammar$structural_burst_episode_bridge_factor <- 2
  vp <- stpd_event_core_params_impl(dat, params, 0.001)
  scientific <- stpd_event_core_freeze_burst_bridges(
    dat, params, vp, candidates, 0.001, "train_1", boundaries
  )
  list(
    train = "train_1", input_candidates = candidates,
    scientific_out = scientific, dat = dat, params = params, vp = vp,
    min_isi_sec = 0.001, boundaries = boundaries
  )
}

pause_ownership_live_collector <- function(run_id) {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("c", 64L), collapse = ""),
    "pause_ownership", "train_1"
  )
  collector
}

pause_ownership_live_run <- function(run_id = "run_pause_ownership") {
  dat <- pause_ownership_dat(0.020)
  params <- effective_params_for_detector(default_params())
  params$event_grammar$structural_burst_episode_bridge_factor <- 2
  params$event_grammar$single_expanded_bridge_enabled <- TRUE
  params$detector$patterns_to_run <- c("burst", "pause")
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  collector <- pause_ownership_live_collector(run_id)
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
    baseline = baseline, observed = observed, collector = collector,
    shard = shard,
    snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard)
  )
}

pause_ownership_expect_invalid <- function(shard, hook, payload) {
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_validate_hook_payload(shard, hook, payload),
    error = identity
  ), "stpd_candidate_lineage_collector_hook_payload_schema_invalid")
}

test_that("ownership success is a support-only one-pass merge", {
  context <- pause_ownership_context()
  replay <- stpd_candidate_lineage_pause_ownership_replay(context)
  expect_false(is.null(replay))
  expect_identical(replay$receipt$successful_merge_n, 1L)
  expect_identical(replay$receipt$failed_merge_n, 0L)
  expect_identical(replay$receipt$output_support_n, 1L)
  expect_identical(replay$attempts$terminal_status, "merge_emitted")
  expect_identical(replay$attempts$terminal_reason,
                   "ownership_merge_emitted")
  expect_identical(replay$attempts$left_candidate_id, "left_burst")
  expect_identical(replay$attempts$right_candidate_id, "right_burst")
  expect_true(grepl("combined_burst_witness",
                    replay$attempts$spanning_witness_candidate_ids,
                    fixed = TRUE))
  expect_identical(replay$output$relation, "merge_ownership_child")
  expect_identical(replay$output$source_candidate_ids,
                   "left_burst;right_burst")
  expect_true(grepl("combined_burst_witness",
                    replay$output$witness_candidate_ids, fixed = TRUE))
  expect_identical(replay$output$detector_start_row, 5L)
  expect_identical(replay$output$detector_end_row, 11L)
  expect_identical(replay$output$ownership_role,
                   "pause_ownership_support_only")
  expect_identical(replay$output$event_materialization_status,
                   "not_materialized_by_this_path")
  expect_false(any(replay$attempts$redetection_performed))
  expect_identical(replay$receipt$redetection_n, 0L)
  expect_false(replay$receipt$cap_applied)
  expect_identical(replay$receipt$output_role,
                   "pause_ownership_support_only")
  expect_identical(context$input_candidates, pause_ownership_atoms(TRUE))
})

test_that("no witness, ordinary gap and canonical boundary reject explicitly", {
  no_witness <- stpd_candidate_lineage_pause_ownership_replay(
    pause_ownership_context(candidates = pause_ownership_atoms(FALSE))
  )
  expect_identical(no_witness$attempts$terminal_status, "merge_rejected")
  expect_identical(no_witness$attempts$terminal_reason,
                   "no_spanning_accepted_witness")
  expect_identical(no_witness$attempts$witness_gate, "fail")
  expect_identical(no_witness$attempts$gap_shape_gate, "not_evaluated")
  expect_identical(no_witness$receipt$failed_merge_n, 1L)
  expect_identical(no_witness$receipt$retained_atom_n, 2L)

  ordinary <- stpd_candidate_lineage_pause_ownership_replay(
    pause_ownership_context(gap_isi = 0.012)
  )
  expect_identical(ordinary$attempts$terminal_reason,
                   "expanded_bridge_count_not_one")
  expect_identical(ordinary$attempts$expanded_bridge_gate, "fail")
  expect_identical(ordinary$attempts$q90_gate, "not_evaluated")
  expect_identical(ordinary$attempts$construction_gate, "not_evaluated")
  expect_identical(ordinary$receipt$successful_merge_n, 0L)
  expect_identical(ordinary$receipt$retained_atom_n, 2L)

  boundary <- data.frame(
    start_isi = 8L, end_isi = 8L, boundary_kind = "canonical_pause",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  bounded <- stpd_candidate_lineage_pause_ownership_replay(
    pause_ownership_context(boundaries = boundary)
  )
  expect_identical(bounded$attempts$terminal_reason,
                   "canonical_pause_overlap")
  expect_identical(bounded$attempts$boundary_gate, "fail")
  expect_identical(bounded$attempts$witness_gate, "not_evaluated")
  expect_identical(bounded$receipt$successful_merge_n, 0L)
  expect_false(any(bounded$attempts$redetection_performed))
})

test_that("one-pass scheduling advances after failure and never re-feeds a merge", {
  isi <- c(
    NA_real_, rep(0.040, 3L), rep(0.006, 3L), 0.012,
    rep(0.006, 3L), 0.020, rep(0.006, 3L), rep(0.040, 4L)
  )
  dat <- data.frame(
    timestamp_sec = cumsum(replace(isi, 1L, 0)), ISI_sec = isi,
    pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  params <- default_params_sec()
  params$event_grammar$structural_burst_episode_bridge_factor <- 2
  vp <- stpd_event_core_params_impl(dat, params, 0.001)
  candidates <- data.frame(
    train = "train_1",
    candidate_id = c("a", "b", "c", "bc_witness"),
    final_label = "burst", action = "accept",
    start_isi = c(5L, 9L, 13L, 9L),
    end_isi = c(7L, 11L, 15L, 15L),
    selected_for_auto = c(TRUE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  scientific <- stpd_event_core_freeze_burst_bridges(
    dat, params, vp, candidates, 0.001, "train_1", data.frame()
  )
  replay <- stpd_candidate_lineage_pause_ownership_replay(list(
    train = "train_1", input_candidates = candidates,
    scientific_out = scientific, dat = dat, params = params, vp = vp,
    min_isi_sec = 0.001, boundaries = data.frame()
  ))
  expect_false(is.null(replay))
  expect_identical(replay$attempts$left_candidate_id, c("a", "b"))
  expect_identical(replay$attempts$right_candidate_id, c("b", "c"))
  expect_identical(
    replay$attempts$terminal_reason,
    c("no_spanning_accepted_witness", "ownership_merge_emitted")
  )
  expect_identical(
    replay$output$relation,
    c("retained_ownership_atom", "merge_ownership_child")
  )
  expect_identical(replay$output$source_candidate_ids, c("a", "b;c"))
  expect_identical(replay$receipt$pair_attempt_n, 2L)
  expect_identical(replay$receipt$successful_merge_n, 1L)
  expect_true(replay$receipt$one_pass_control_path_exhausted)
  expect_false(any(grepl(
    "event_core_bridge_merged_burst", replay$attempts$left_candidate_id,
    fixed = TRUE
  )))
})

test_that("gap length and bridge-count caps remain distinct evidence", {
  context <- pause_ownership_context()
  context$vp$max_bridge_n <- 0
  context$scientific_out <- stpd_event_core_freeze_burst_bridges(
    context$dat, context$params, context$vp, context$input_candidates,
    context$min_isi_sec, context$train, context$boundaries
  )
  replay <- stpd_candidate_lineage_pause_ownership_replay(context)
  expect_false(is.null(replay))
  expect_identical(replay$entry$max_gap_n, 1L)
  expect_identical(replay$entry$max_bridge_n, 0)
  expect_identical(replay$attempts$gap_shape_gate, "pass")
  expect_identical(replay$attempts$bridge_count_gate, "fail")
  expect_identical(replay$attempts$terminal_reason,
                   "bridge_count_exceeded")
})

test_that("empty, no-eligible and single-atom paths are typed", {
  empty_candidates <- pause_ownership_atoms(FALSE)[0, , drop = FALSE]
  empty <- stpd_candidate_lineage_pause_ownership_replay(
    pause_ownership_context(candidates = empty_candidates)
  )
  expect_identical(empty$entry$applicability_status,
                   "not_invoked_empty_input")
  expect_identical(nrow(empty$eligibility), 0L)
  expect_identical(nrow(empty$selection), 0L)
  expect_identical(nrow(empty$attempts), 0L)
  expect_identical(nrow(empty$output), 0L)
  expect_identical(empty$receipt$coverage_status,
                   "not_invoked_empty_input")
  expect_false(empty$receipt$one_pass_control_path_exhausted)
  expect_identical(
    empty$receipt$evidence_origin,
    "live_control_flow_with_deterministic_not_invoked_replay"
  )

  excluded <- pause_ownership_atoms(FALSE)
  excluded$final_label <- c("tonic", "high_frequency_spiking")
  excluded$candidate_layer <- c(
    "isi_profile_hard_threshold_burst", "nested_hfs_local_rate_review"
  )
  no_eligible <- stpd_candidate_lineage_pause_ownership_replay(
    pause_ownership_context(candidates = excluded)
  )
  expect_identical(no_eligible$entry$applicability_status,
                   "invoked_no_eligible_candidates")
  expect_false(any(no_eligible$eligibility$eligible))
  expect_identical(
    no_eligible$eligibility$eligibility_reason,
    rep("excluded_non_burst_label", 2L)
  )
  expect_identical(nrow(no_eligible$output), 0L)
  expect_false(no_eligible$receipt$one_pass_control_path_exhausted)

  single <- pause_ownership_atoms(FALSE)[1L, , drop = FALSE]
  one <- stpd_candidate_lineage_pause_ownership_replay(
    pause_ownership_context(candidates = single)
  )
  expect_identical(one$entry$applicability_status,
                   "invoked_fewer_than_two_selected_atoms")
  expect_identical(one$receipt$selector_winner_n, 1L)
  expect_identical(one$receipt$lone_tail_n, 1L)
  expect_false(one$receipt$one_pass_control_path_exhausted)
  expect_identical(one$output$relation, "retained_ownership_atom")
  expect_identical(one$output$ownership_role,
                   "pause_ownership_support_only")
  expect_identical(one$output$event_materialization_status,
                   "not_materialized_by_this_path")
})

test_that("live ownership observation is byte, RNG and options neutral", {
  set.seed(84021)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()
  run <- pause_ownership_live_run("run_pause_ownership_neutral")
  expect_identical(run$observed, run$baseline)
  expect_identical(serialize(run$observed, NULL, version = 3L),
                   serialize(run$baseline, NULL, version = 3L))
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)
  obs <- run$snapshot$observations
  for (hook in c(
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1",
      "burst_pause_ownership_selection_v1",
      "burst_pause_ownership_attempts_v1",
      "burst_pause_ownership_output_v1",
      "burst_pause_ownership_receipt_v1")) {
    expect_false(is.null(obs[[hook]]), info = hook)
  }
  expect_true(all(obs$burst_pause_ownership_output_v1$ownership_role ==
                    "pause_ownership_support_only"))
  expect_true(all(
    obs$burst_pause_ownership_output_v1$event_materialization_status ==
      "not_materialized_by_this_path"
  ))
  expect_identical(
    obs$burst_pause_ownership_receipt_v1$upstream_pause_status,
    "not_applicable_ownership_precedes_contextual_pause"
  )
  audit <- attr(run$observed, "candidate_diagnostic_audit")
  if (!is.null(audit) && nrow(audit) && "candidate_id" %in% names(audit)) {
    expect_false(any(grepl(
      "^event_core_bridge_merged_burst_",
      as.character(audit$candidate_id)
    )))
  }
  shadow <- attr(run$observed, "multitrack_shadow")$candidates
  if (!is.null(shadow) && nrow(shadow) && "candidate_id" %in% names(shadow)) {
    expect_false(any(grepl(
      "^event_core_bridge_merged_burst_",
      as.character(shadow$candidate_id)
    )))
  }
  eligible <- obs$burst_pause_ownership_eligibility_v1
  layers <- as.character(
    run$shard$burst_pause_ownership_context$input_candidates$candidate_layer
  )
  expect_false(any(
    eligible$eligible & layers[eligible$input_ordinal] %in%
      c("isi_profile_hard_threshold_burst",
        "nested_hfs_local_rate_review")
  ))
})

test_that("collector-off never enters ownership observation", {
  dat <- pause_ownership_dat()
  params <- effective_params_for_detector(default_params())
  params$event_grammar$structural_burst_episode_bridge_factor <- 2
  params$event_grammar$single_expanded_bridge_enabled <- TRUE
  params$detector$patterns_to_run <- c("burst", "pause")
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_capture_burst_pause_ownership = function(...) {
      stop("ownership observer entered while collector was off")
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

test_that("wrong train and every ownership hook mutation fail", {
  run <- pause_ownership_live_run("run_pause_ownership_tamper")
  before <- run$snapshot
  obs <- before$observations
  context <- run$shard$burst_pause_ownership_context
  expect_s3_class(tryCatch(
    stpd_candidate_lineage_capture_burst_pause_ownership(
      run$shard, "train_2", context$input_candidates,
      context$scientific_out, context$dat, context$params, context$vp,
      context$min_isi_sec, context$boundaries
    ), error = identity
  ), "stpd_candidate_lineage_collector_train_scope_invalid")

  mutations <- list(
    burst_pause_ownership_entry_v1 = function(x) {
      x$input_payload_sha256 <- paste(rep("0", 64L), collapse = ""); x
    },
    burst_pause_ownership_eligibility_v1 = function(x) {
      if (nrow(x)) x$eligibility_reason[[1L]] <- "forged"; x
    },
    burst_pause_ownership_selection_v1 = function(x) {
      if (nrow(x)) x$selected_atom[[1L]] <- !x$selected_atom[[1L]]; x
    },
    burst_pause_ownership_attempts_v1 = function(x) {
      if (nrow(x)) x$terminal_reason[[1L]] <- "forged"; x
    },
    burst_pause_ownership_output_v1 = function(x) {
      if (nrow(x)) x$ownership_role[[1L]] <- "event"; x
    },
    burst_pause_ownership_receipt_v1 = function(x) {
      x$output_support_n <- x$output_support_n + 1L; x
    }
  )
  for (hook in names(mutations)) {
    pause_ownership_expect_invalid(
      run$shard, hook, mutations[[hook]](obs[[hook]])
    )
  }
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before
  )
})

test_that("a fully rehashed ownership attack cannot escape upstream closure", {
  run <- pause_ownership_live_run("run_pause_ownership_rehashed")
  context <- unserialize(serialize(
    run$shard$burst_pause_ownership_context, NULL, version = 3L
  ))
  if (!nrow(context$input_candidates)) skip("live fixture has no Burst input")
  context$input_candidates$score[[1L]] <-
    context$input_candidates$score[[1L]] + 1
  context$scientific_out <- stpd_event_core_freeze_burst_bridges(
    context$dat, context$params, context$vp, context$input_candidates,
    context$min_isi_sec, "train_1", context$boundaries
  )
  forged <- stpd_candidate_lineage_pause_ownership_replay(context)
  expect_false(is.null(forged))
  original <- run$shard$burst_pause_ownership_context
  run$shard$burst_pause_ownership_context <- context
  pause_ownership_expect_invalid(
    run$shard, "burst_pause_ownership_receipt_v1", forged$receipt
  )
  run$shard$burst_pause_ownership_context <- original

  changed_science <- unserialize(serialize(
    original, NULL, version = 3L
  ))
  changed_science$dat$ISI_sec[[8L]] <- 0.012
  changed_science$dat$timestamp_sec <- cumsum(replace(
    changed_science$dat$ISI_sec, 1L, 0
  ))
  changed_science$scientific_out <- stpd_event_core_freeze_burst_bridges(
    changed_science$dat, changed_science$params, changed_science$vp,
    changed_science$input_candidates, changed_science$min_isi_sec,
    "train_1", changed_science$boundaries
  )
  forged_science <- stpd_candidate_lineage_pause_ownership_replay(
    changed_science
  )
  expect_false(is.null(forged_science))
  run$shard$burst_pause_ownership_context <- changed_science
  pause_ownership_expect_invalid(
    run$shard, "burst_pause_ownership_receipt_v1",
    forged_science$receipt
  )
  run$shard$burst_pause_ownership_context <- original
})
