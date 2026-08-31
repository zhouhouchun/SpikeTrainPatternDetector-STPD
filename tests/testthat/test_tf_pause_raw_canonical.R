pause_raw_dat <- function(isi) {
  data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(replace(isi, 1L, 0)),
    ISI_sec = isi,
    pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

pause_raw_params_vp <- function(dat, patterns = "pause") {
  params <- effective_params_for_detector(default_params())
  params$detector$patterns_to_run <- patterns
  params$event_grammar$pause_relative_local_factor <- 1.55
  params$event_grammar$pause_relative_global_factor <- 1.25
  # This is a controlled raw-canonical observer fixture, not an automatic-tail
  # identifiability fixture.  Freeze the 100-ms strong threshold in the params
  # consumed by the live callsite so unresolved histogram q95 evidence cannot
  # silently turn the canonical payload into a review-only empty payload.
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$effective_bands <- list(
    pause = list(
      seed_lower_sec = 0.100,
      seed_upper_sec = 0.100,
      bridge_upper_sec = 0.100,
      seed_lower_sec_source = "user",
      seed_upper_sec_source = "user",
      bridge_upper_sec_source = "user"
    )
  )
  params$event_grammar$threshold_table <- data.frame()
  vp <- stpd_event_grammar_params_impl(dat, params, 0.001, "train_1")
  vp$pause_thr <- 0.100
  vp$pause_entry_thr <- 0.100
  vp$pause_strong_thr <- 0.100
  vp$pause_strong_active <- TRUE
  vp$pause_strong_status <- "test_configured_strong_threshold"
  vp$pause_threshold_source_mode <- "user"
  vp$tonic_min <- 0.025
  vp$tonic_max <- 0.050
  list(params = params, vp = vp)
}

pause_raw_controlled <- function(patterns = "pause") {
  # The background count keeps q90 at 20 ms.  The exact 100-ms anchor is
  # isolated, while 120/130 ms form one consecutive canonical Pause run.
  isi <- c(NA_real_, rep(0.020, 15L), 0.100, rep(0.020, 15L),
           0.120, 0.130, rep(0.020, 4L))
  dat <- pause_raw_dat(isi)
  pv <- pause_raw_params_vp(dat, patterns)
  list(dat = dat, params = pv$params, vp = pv$vp,
       exact_row = 17L, run_start = 33L, run_end = 34L)
}

pause_raw_collector <- function(run_id = "run_pause_raw") {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("p", 64L), collapse = ""),
    "pause_raw", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1"
  )
  list(collector = collector, shard = shard)
}

test_that("raw canonical Pause has an independent inclusive-threshold oracle", {
  x <- pause_raw_controlled()
  out <- stpd_event_core_detect_pause(
    x$dat, x$params, x$vp, 0.001, "train_1", burst_candidates = NULL
  )

  # Independent oracle: q90=20 ms, tonic guard=57.5 ms, so the frozen
  # effective floor is exactly 100 ms.  All three anchors pass both relative
  # gates against the 20-ms background.
  expect_identical(out$start_isi, c(x$exact_row, x$run_start))
  expect_identical(out$end_isi, c(x$exact_row, x$run_end))
  expect_identical(out$candidate_id, c("event_core_pause_1",
                                       "event_core_pause_2"))
  expect_equal(out$pause_base_threshold_sec, c(0.100, 0.100))
  expect_equal(out$pause_effective_threshold_sec, c(0.100, 0.100))
  expect_equal(out$pause_tonic_guard_threshold_sec, c(0.0575, 0.0575))
  expect_equal(out$pause_global_median_sec, c(0.020, 0.020))
  expect_equal(out$score, c(1.0, 1.3))
  expect_identical(out$gap_semantics,
                   rep("canonical_pause", 2L))
  expect_true(all(out$hard_for_event))
  expect_true(all(out$hard_for_state_direct_support))
  expect_false(any(out$envelope_bridge_eligible))
  expect_true(all(out$action == "accept"))
})

test_that("explicit Pause threshold is orthogonal to Tonic audit bounds", {
  isi <- c(NA_real_, rep(0.020, 20L), 0.100, 0.110, 0.0005,
           rep(0.020, 15L))
  dat <- pause_raw_dat(isi)
  pv <- pause_raw_params_vp(dat)
  pv$vp$pause_thr <- 0.080
  pv$vp$tonic_max <- 0.100
  pv$vp$tonic_threshold_source_mode <- "manual"
  out <- stpd_event_core_detect_pause(
    dat, pv$params, pv$vp, 0.001, "train_1", burst_candidates = NULL
  )
  # The Tonic-derived 115-ms value remains visible for audit, but State
  # evidence cannot override an explicit 80-ms Event/Gap threshold.  The
  # 0.5-ms artifact can never become valid Pause support.
  expect_equal(nrow(out), 1L)
  expect_identical(out$start_isi, 22L)
  expect_identical(out$end_isi, 23L)
  expect_equal(out$pause_effective_threshold_sec, 0.080)
  expect_equal(out$pause_tonic_guard_threshold_sec, 0.115)

  dat$ISI_sec[[23L]] <- 0.115
  dat$timestamp_sec <- cumsum(replace(dat$ISI_sec, 1L, 0))
  at_guard <- stpd_event_core_detect_pause(
    dat, pv$params, pv$vp, 0.001, "train_1", burst_candidates = NULL
  )
  expect_identical(at_guard$start_isi, 22L)
  expect_identical(at_guard$end_isi, 23L)
  expect_equal(at_guard$pause_effective_threshold_sec, 0.080)
  expect_equal(at_guard$pause_tonic_guard_threshold_sec, 0.115)
})

test_that("canonical boundaries filter, sort and deduplicate independently", {
  candidates <- data.frame(
    start_isi = c(9L, 3L, 3L, 5L, 7L, 11L, 13L),
    end_isi = c(9L, 3L, 3L, 5L, 7L, 11L, 13L),
    gap_semantics = c("canonical_pause", "canonical_pause",
                      "canonical_pause", "ambiguous_gap",
                      "canonical_pause", "contextual_interburst_pause",
                      "canonical_pause"),
    hard_for_event = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
    action = c("accept", "accept", "accept", "accept", "accept", "accept",
               "reject"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out <- stpd_event_core_pause_hard_boundaries(candidates)
  expect_identical(nrow(out), 2L)
  expect_identical(out$start_isi, c(3L, 9L))
  expect_identical(out$end_isi, c(3L, 9L))
  expect_identical(out$boundary_kind, rep("canonical_pause", 2L))
})

test_that("raw Pause has no candidate cap and preserves detector-row order", {
  isi <- c(NA_real_, rep(0.020, 15L), 0.150, rep(0.020, 8L),
           0.120, rep(0.020, 8L), 0.100, rep(0.020, 8L))
  dat <- pause_raw_dat(isi)
  pv <- pause_raw_params_vp(dat)
  pv$vp$max_candidates <- 1L
  one <- stpd_event_core_detect_pause(
    dat, pv$params, pv$vp, 0.001, "train_1", burst_candidates = NULL
  )
  pv$vp$max_candidates <- 100L
  many <- stpd_event_core_detect_pause(
    dat, pv$params, pv$vp, 0.001, "train_1", burst_candidates = NULL
  )
  expect_identical(
    serialize(one, NULL, version = 3L),
    serialize(many, NULL, version = 3L)
  )
  expect_identical(one$start_isi, sort(one$start_isi))
  expect_identical(one$candidate_id, paste0("event_core_pause_", 1:3))
  expect_equal(one$score, c(1.5, 1.2, 1.0))
})

test_that("raw Pause request gating is explicit at the live callsite", {
  skip_if_not(exists("stpd_candidate_lineage_begin_pause_raw_canonical",
                     mode = "function"))
  x <- pause_raw_controlled(patterns = "tonic")
  baseline <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE
  )
  cc <- pause_raw_collector("run_pause_raw_unrequested")
  observed <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = cc$shard
  )
  expect_identical(serialize(observed, NULL, version = 3L),
                   serialize(baseline, NULL, version = 3L))
  obs <- stpd_candidate_lineage_collector_observation_snapshot(
    cc$shard
  )$observations
  expect_false(obs$pause_raw_entry_v1$pause_pattern_requested)
  expect_true(obs$pause_raw_entry_v1$generation_gated_by_requested_patterns)
  expect_identical(nrow(obs$pause_raw_output_v1), 0L)
  expect_identical(nrow(obs$pause_boundary_projection_output_v1), 0L)
})

test_that("raw Pause short, invalid-threshold and zero-hit paths are typed", {
  short <- pause_raw_dat(c(NA_real_, 0.020))
  short_pv <- pause_raw_params_vp(short)
  short_context <- list(
    train = "train_1", dat = short, params = short_pv$params,
    vp = short_pv$vp, min_isi_sec = 0.001, patterns = "pause",
    scientific_raw_output = data.frame(),
    scientific_boundaries = stpd_event_core_pause_hard_boundaries(NULL)
  )
  short_replay <- stpd_candidate_lineage_pause_raw_replay(short_context)
  expect_identical(
    short_replay$entry$applicability_status, "invoked_insufficient_input"
  )
  expect_identical(
    short_replay$support,
    stpd_candidate_lineage_empty_hook_payload("pause_raw_support_v1")
  )

  invalid <- pause_raw_controlled()
  invalid$vp$pause_thr <- NA_real_
  invalid_context <- list(
    train = "train_1", dat = invalid$dat, params = invalid$params,
    vp = invalid$vp, min_isi_sec = 0.001, patterns = "pause",
    scientific_raw_output = data.frame(),
    scientific_boundaries = stpd_event_core_pause_hard_boundaries(NULL)
  )
  invalid_replay <- stpd_candidate_lineage_pause_raw_replay(invalid_context)
  expect_identical(
    invalid_replay$entry$applicability_status,
    "invoked_invalid_pause_threshold"
  )
  expect_false(invalid_replay$receipt$scan_exhausted)

  zero_dat <- pause_raw_dat(c(NA_real_, rep(0.020, 20L)))
  zero_pv <- pause_raw_params_vp(zero_dat)
  zero_context <- list(
    train = "train_1", dat = zero_dat, params = zero_pv$params,
    vp = zero_pv$vp, min_isi_sec = 0.001, patterns = "pause",
    scientific_raw_output = data.frame(),
    scientific_boundaries = stpd_event_core_pause_hard_boundaries(NULL)
  )
  zero_replay <- stpd_candidate_lineage_pause_raw_replay(zero_context)
  expect_true(zero_replay$receipt$scan_exhausted)
  expect_identical(zero_replay$receipt$flagged_isi_n, 0L)
  expect_identical(nrow(zero_replay$support), 20L)
  expect_identical(
    zero_replay$runs,
    stpd_candidate_lineage_empty_hook_payload("pause_raw_runs_v1")
  )
})

test_that("live raw Pause capture is scientific, RNG and options neutral", {
  skip_if_not(exists("stpd_candidate_lineage_begin_pause_raw_canonical",
                     mode = "function"))
  x <- pause_raw_controlled()
  set.seed(85117)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  options_before <- options()
  baseline <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE
  )
  cc <- pause_raw_collector("run_pause_raw_neutral")
  observed <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = cc$shard
  )
  expect_identical(serialize(observed, NULL, version = 3L),
                   serialize(baseline, NULL, version = 3L))
  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(options(), options_before)
  obs <- stpd_candidate_lineage_collector_observation_snapshot(
    cc$shard
  )$observations
  hooks <- c(
    "pause_raw_entry_v1", "pause_raw_support_v1", "pause_raw_runs_v1",
    "pause_raw_output_v1", "pause_boundary_projection_sources_v1",
    "pause_boundary_projection_output_v1", "pause_raw_receipt_v1"
  )
  expect_true(all(hooks %in% names(obs)))
  expect_true(obs$pause_raw_entry_v1$pause_pattern_requested)
  expect_true(all(obs$pause_raw_output_v1$intended_semantic_track == "gap"))
  expect_true(all(
    obs$pause_raw_output_v1$selection_status ==
      "not_observed_at_this_root"
  ))
  expect_true(all(
    obs$pause_boundary_projection_output_v1$candidate_status ==
      "not_a_candidate"
  ))
  expect_false(any(obs$pause_boundary_projection_output_v1$selectable))
  expect_identical(
    obs$pause_raw_receipt_v1$recurrent_pause_evaluation_status,
    "not_evaluated_post_detection_only"
  )
  expect_false(obs$pause_raw_receipt_v1$cap_applied)
  expect_false(obs$pause_raw_receipt_v1$publication_authority)
  expect_true(stpd_candidate_lineage_pause_raw_outputs_are_closed(
    cc$shard,
    get("pause_raw_canonical_context", envir = cc$shard)$scientific_raw_output,
    get("pause_raw_canonical_context", envir = cc$shard)$scientific_boundaries,
    science_context = get("pause_raw_canonical_context", envir = cc$shard)
  ))
})

test_that("raw Pause two-phase capture rejects post-hoc and changed contexts", {
  skip_if_not(exists("stpd_candidate_lineage_begin_pause_raw_canonical",
                     mode = "function"))
  x <- pause_raw_controlled()
  raw <- stpd_event_core_detect_pause(
    x$dat, x$params, x$vp, 0.001, "train_1", burst_candidates = NULL
  )
  boundaries <- stpd_event_core_pause_hard_boundaries(raw)
  cc <- pause_raw_collector("run_pause_raw_attack")
  stpd_candidate_lineage_collector_capture(
    cc$shard, "hf_protected_impl_entry_v1",
    data.frame(
      train = "train_1", n_interval_rows = as.integer(nrow(x$dat)),
      min_isi_sec = 0.001,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )

  before_missing_begin <-
    stpd_candidate_lineage_collector_observation_snapshot(cc$shard)
  expect_error(
    stpd_candidate_lineage_capture_pause_raw_canonical(
      cc$shard, "train_1", x$dat, x$params, x$vp, 0.001, "pause",
      raw, boundaries
    ),
    class =
      "stpd_candidate_lineage_collector_pause_raw_begin_context_missing"
  )
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(cc$shard),
    before_missing_begin
  )
  stpd_candidate_lineage_begin_pause_raw_canonical(
    cc$shard, "train_1", x$dat, x$params, x$vp, 0.001, "pause"
  )
  changed <- x$dat
  changed$ISI_sec[[x$exact_row]] <- 0.140
  changed$timestamp_sec <- cumsum(replace(changed$ISI_sec, 1L, 0))
  changed_raw <- stpd_event_core_detect_pause(
    changed, x$params, x$vp, 0.001, "train_1", burst_candidates = NULL
  )
  changed_boundaries <- stpd_event_core_pause_hard_boundaries(changed_raw)
  expect_error(
    stpd_candidate_lineage_capture_pause_raw_canonical(
      cc$shard, "train_1", changed, x$params, x$vp, 0.001, "pause",
      changed_raw, changed_boundaries
    ),
    class =
      "stpd_candidate_lineage_collector_pause_raw_begin_context_conflict"
  )
})

test_that("raw Pause capture rejects a fully self-consistent helper forgery", {
  x <- pause_raw_controlled()
  raw <- stpd_event_core_detect_pause(
    x$dat, x$params, x$vp, 0.001, "train_1", burst_candidates = NULL
  )
  forged <- raw
  forged$score[[1L]] <- forged$score[[1L]] + 0.25
  forged_boundaries <- stpd_event_core_pause_hard_boundaries(forged)
  cc <- pause_raw_collector("run_pause_raw_helper_forgery")
  stpd_candidate_lineage_collector_capture(
    cc$shard, "hf_protected_impl_entry_v1",
    data.frame(
      train = "train_1", n_interval_rows = as.integer(nrow(x$dat)),
      min_isi_sec = 0.001,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
  stpd_candidate_lineage_begin_pause_raw_canonical(
    cc$shard, "train_1", x$dat, x$params, x$vp, 0.001, "pause"
  )
  testthat::local_mocked_bindings(
    stpd_event_core_detect_pause = function(...) forged,
    .package = "SpikeTrainPatternDetector"
  )
  expect_error(
    stpd_candidate_lineage_capture_pause_raw_canonical(
      cc$shard, "train_1", x$dat, x$params, x$vp, 0.001, "pause",
      forged, forged_boundaries
    ),
    class =
      "stpd_candidate_lineage_collector_pause_raw_scientific_replay_failed"
  )
})

test_that("raw Pause hook validation rejects semantic tampering atomically", {
  x <- pause_raw_controlled()
  cc <- pause_raw_collector("run_pause_raw_hook_attack")
  invisible(stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = cc$shard
  ))
  before <- stpd_candidate_lineage_collector_observation_snapshot(cc$shard)
  obs <- before$observations
  expect_true(bindingIsLocked(
    "pause_raw_canonical_begin_context", cc$shard
  ))
  expect_true(bindingIsLocked("pause_raw_canonical_context", cc$shard))
  expect_true(bindingIsLocked("pause_raw_canonical_replay_cache", cc$shard))

  mutations <- list(
    pause_raw_entry_v1 = function(z) {
      z$pause_threshold_sec <- z$pause_threshold_sec + 0.001; z
    },
    pause_raw_support_v1 = function(z) {
      z$final_pause_flag[[which(z$final_pause_flag)[1L]]] <- FALSE; z
    },
    pause_raw_runs_v1 = function(z) {
      z$score[[1L]] <- z$score[[1L]] + 1; z
    },
    pause_raw_output_v1 = function(z) {
      z$action[[1L]] <- "reject"; z
    },
    pause_boundary_projection_sources_v1 = function(z) {
      z$projected[[1L]] <- FALSE; z
    },
    pause_boundary_projection_output_v1 = function(z) {
      z$boundary_kind[[1L]] <- "forged"; z
    },
    pause_raw_receipt_v1 = function(z) {
      z$raw_output_n <- z$raw_output_n + 1L; z
    }
  )
  for (hook in names(mutations)) {
    err <- tryCatch(
      stpd_candidate_lineage_validate_hook_payload(
        cc$shard, hook, mutations[[hook]](obs[[hook]])
      ),
      error = identity
    )
    expect_s3_class(
      err, "stpd_candidate_lineage_collector_hook_payload_schema_invalid"
    )
  }
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(cc$shard), before
  )
})

test_that("collector-off cannot enter raw Pause observation helpers", {
  skip_if_not(exists("stpd_candidate_lineage_begin_pause_raw_canonical",
                     mode = "function"))
  x <- pause_raw_controlled()
  baseline <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE
  )
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_begin_pause_raw_canonical = function(...) {
      stop("raw Pause begin entered with collector off")
    },
    stpd_candidate_lineage_capture_pause_raw_canonical = function(...) {
      stop("raw Pause capture entered with collector off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    x$dat, x$params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = NULL
  )
  expect_identical(serialize(observed, NULL, version = 3L),
                   serialize(baseline, NULL, version = 3L))
})
