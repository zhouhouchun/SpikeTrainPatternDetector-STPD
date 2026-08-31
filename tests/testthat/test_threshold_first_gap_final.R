gap_final_dat <- function() {
  isi <- c(NA_real_, rep(0.020, 8L), 0.200, rep(0.020, 8L))
  data.frame(
    idx = seq_along(isi), timestamp_sec = cumsum(replace(isi, 1L, 0)),
    ISI_sec = isi, pattern_manual = "", pattern_manual_negative = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

gap_final_params <- function() {
  p <- default_params()
  p$detector$patterns_to_run <- "pause"
  p$detector$fill_others_auto <- FALSE
  p
}

gap_final_active <- function(run_id = "gap_final_active") {
  collector <- stpd_candidate_lineage_collector_new("full")
  stpd_candidate_lineage_collector_bind_run(
    collector, run_id, paste(rep("a", 64L), collapse = ""),
    "gap_final_fixture", "train_1"
  )
  shard <- stpd_candidate_lineage_collector_begin_train(collector, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    shard, "hf_protected", train = "train_1")
  out <- stpd_detect_train_hf_protected_impl(
    gap_final_dat(), gap_final_params(), 0.001, "train_1",
    lock_manual = FALSE, candidate_lineage_collector = shard)
  list(out = out, collector = collector, shard = shard,
       snapshot = stpd_candidate_lineage_collector_observation_snapshot(shard))
}

test_that("final Gap root binds, replays and materializes the live Pause", {
  run <- gap_final_active()
  obs <- run$snapshot$observations
  expect_true(all(names(stpd_candidate_lineage_gap_final_hooks()) %in% names(obs)))
  receipt <- obs$gap_final_receipt_v1
  expect_true(receipt$exact_candidate_input_binding)
  expect_identical(receipt$arbitration_replay_status, "validated_exact")
  expect_identical(receipt$scientific_audit_identity_status, "validated_exact")
  expect_identical(
    receipt$materialized_gap_identity_status,
    "validated_exact_prevalidation_identity")
  expect_identical(receipt$selected_gap_candidate_n, 1L)
  expect_identical(receipt$expected_gap_support_isi_n, 1L)
  expect_identical(receipt$retained_gap_support_isi_n, 1L)
  expect_identical(receipt$post_validation_removal_n, 0L)
  expect_false(receipt$publication_authority)
  expect_identical(receipt$scientific_result_influence, "none_observer_only")
  expect_identical(
    obs$gap_final_materialized_output_v1$final_pattern_auto, "pause")
  expect_true(obs$gap_final_materialized_output_v1$retained_as_pause)
  expect_silent(stpd_candidate_lineage_validate_hook_payload(
    run$shard, "gap_final_receipt_v1", receipt))
})

test_that("final arbitration preserves conflict priority and rejected Gap veto", {
  audit <- data.frame(
    candidate_id = c("owned_gap", "burst_owner", "free_gap"),
    final_label = c("pause", "burst", "pause"),
    action = c("reject", "accept", "accept"),
    start_isi = c(5L, 5L, 12L), end_isi = c(5L, 7L, 12L),
    score = c(9, 2, 1.5), n_isi = c(1, 3, 1),
    priority = c(300, 1250, 300), candidate_layer = "fixture",
    strict_boundary_pass = FALSE, stringsAsFactors = FALSE,
    check.names = FALSE
  )
  replay <- stpd_candidate_lineage_gap_final_replay_selection(
    audit, c("burst", "pause"))
  expect_identical(
    replay$audit$selected_for_auto, c(FALSE, TRUE, TRUE))
  expect_identical(
    replay$audit$selection_status[[1L]],
    "not_selectable_candidate_action__reject")
  expect_identical(
    replay$audit$selection_status[2:3],
    rep("selected_by_event_core_weighted_interval_grammar", 2L))
})

test_that("each final Gap payload is immutable and hash-replayed", {
  run <- gap_final_active("gap_final_tamper")
  before <- stpd_candidate_lineage_collector_observation_snapshot(run$shard)
  mutations <- list(
    gap_final_entry_v1 = function(z) { z$publication_authority <- TRUE; z },
    gap_final_candidate_input_v1 = function(z) { z$selectable[[1L]] <- !z$selectable[[1L]]; z },
    gap_final_arbitration_v1 = function(z) { z$selected_expected[[1L]] <- !z$selected_expected[[1L]]; z },
    gap_final_materialized_output_v1 = function(z) { z$final_pattern_auto[[1L]] <- "burst"; z },
    gap_final_receipt_v1 = function(z) { z$selected_gap_candidate_n <- 99L; z }
  )
  for (hook in names(mutations)) {
    expect_error(stpd_candidate_lineage_validate_hook_payload(
      run$shard, hook, mutations[[hook]](before$observations[[hook]])),
      class = "stpd_candidate_lineage_collector_hook_payload_schema_invalid")
  }
  expect_identical(
    stpd_candidate_lineage_collector_observation_snapshot(run$shard), before)
})

test_that("collector off cannot enter final Gap observer and is byte neutral", {
  dat <- gap_final_dat(); params <- gap_final_params()
  baseline <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE)
  testthat::local_mocked_bindings(
    stpd_candidate_lineage_begin_gap_final = function(...) {
      stop("final Gap begin entered with collector off")
    },
    stpd_candidate_lineage_capture_gap_final = function(...) {
      stop("final Gap capture entered with collector off")
    },
    .package = "SpikeTrainPatternDetector"
  )
  observed <- stpd_detect_train_hf_protected_impl(
    dat, params, 0.001, "train_1", lock_manual = FALSE,
    candidate_lineage_collector = NULL)
  expect_identical(
    serialize(observed, NULL, version = 3L),
    serialize(baseline, NULL, version = 3L))
})
